//! The front end entry point: [`parse`] reads a `.theokey` file, runs parsing, name resolution,
//! lowering to the IR and checking, and returns everything later passes need ([`Parsed`]).
//!
//! # Lifetimes
//!
//! The IR ([`ir::Map`]) borrows from an arena and from the symbol interner. To hand all of it out
//! as one owned value without a self-referential struct, the arena, the interner and the map are
//! leaked (`Box::leak`), once per parsed file, and are `'static` afterwards. For a compiler
//! process that parses a handful of files this is the pragmatic choice.
//!
//! # Locations
//!
//! Every [`ir::Span`] carries a [`FileId`] and byte offsets into the text of that file.
//! [`SourceMap`] (in [`Parsed::sources`]) turns them back into text, lines and columns.
//! [`SpanIndex`] (in [`Parsed::spans`]) maps the ids of IR nodes ([`IrId`], [`DefId`]) to spans,
//! which is what error reporting of later passes needs, since the IR keeps its ids but not
//! every node has a span of its own.

use std::{
    collections::{HashMap, HashSet},
    fmt, fs, io,
    path::{Path, PathBuf},
};

use dale_index::Idx;
use dale_util::{
    arena::DroplessArena,
    symbol::{Interner, Symbol},
};
use lalrpop_util::ParseError;

use crate::{
    ParserCtx,
    check::{self, CheckErr},
    ctx::{Ctx, FileTable},
    dl_theory,
    ir::{
        self, CostItem, CostParam, CostTy, DefId, Expr, FileId, IrId, ItemId, Map, Module, Rule,
        RuleSetDecl, RuleSetRef, SchemaVarDecl, SortDecl, SortRef, Span, Term,
        visit::{self, Visit},
    },
    raw_theory::{self, ModuleItem},
    resolve::resolve,
};

// ----------------------------------------------------------------------------------------------
// Source files
// ----------------------------------------------------------------------------------------------

/// A 1-based line and column (in characters).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: PathBuf,
    pub text: String,
    /// Where the file starts in the global offset space of the parser's spans; set by
    /// [`SourceMap::add`].
    pub base: usize,
    /// Byte offset of the start of each line.
    line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn new(path: PathBuf, text: String) -> Self {
        let mut line_starts = vec![0];
        line_starts.extend(text.match_indices('\n').map(|(i, _)| i + 1));
        Self {
            path,
            text,
            base: 0,
            line_starts,
        }
    }

    /// The text covered by the byte range (empty if the range is not valid for this file).
    pub fn slice(&self, start: usize, end: usize) -> &str {
        self.text.get(start..end).unwrap_or("")
    }

    pub fn line_col(&self, offset: usize) -> LineCol {
        let offset = offset.min(self.text.len());
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        let start = self.line_starts[line];
        let column = self
            .text
            .get(start..offset)
            .map_or(offset - start, |s| s.chars().count())
            + 1;
        LineCol {
            line: line + 1,
            column,
        }
    }

    /// The text of the 1-based line, without the line terminator.
    pub fn line_text(&self, line: usize) -> &str {
        let Some(&start) = self.line_starts.get(line.wrapping_sub(1)) else {
            return "";
        };
        let end = self
            .line_starts
            .get(line)
            .copied()
            .unwrap_or(self.text.len());
        self.text[start..end].trim_end_matches(['\n', '\r'])
    }
}

/// All source files of a front-end run, indexed by [`FileId`].
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    pub fn add(&mut self, mut file: SourceFile) -> FileId {
        // One extra offset between files, so that an end-of-file span stays in its file.
        file.base = self.files.last().map_or(0, |f| f.base + f.text.len() + 1);
        self.files.push(file);
        FileId::new(self.files.len() - 1)
    }

    /// The table that turns global parser offsets into file-local spans.
    pub fn table(&self) -> FileTable {
        FileTable::new(
            self.files
                .iter()
                .enumerate()
                .map(|(i, f)| (f.base, FileId::new(i)))
                .collect(),
        )
    }

    /// Converts a span of the parser (global offsets) into a file-local one.
    pub fn to_span(&self, raw: raw_theory::Span) -> Span {
        self.table().span(raw)
    }

    pub fn file(&self, id: FileId) -> Option<&SourceFile> {
        self.files.get(id.index())
    }

    pub fn files(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
        self.files
            .iter()
            .enumerate()
            .map(|(i, f)| (FileId::new(i), f))
    }

    /// The original text a span covers (empty for an unknown file or invalid range).
    pub fn snippet(&self, span: Span) -> &str {
        self.file(span.file)
            .map_or("", |f| f.slice(span.start, span.end))
    }

    /// Path and start position of a span.
    pub fn location(&self, span: Span) -> Option<(&Path, LineCol)> {
        let f = self.file(span.file)?;
        Some((&f.path, f.line_col(span.start)))
    }

    /// `path:line:col: severity[phase]: message`, followed by the source line and a marker
    /// under the span when there is one.
    pub fn render(&self, d: &Diagnostic) -> String {
        let head = format!("{}[{}]: {}", d.severity, d.phase, d.message);
        let Some(span) = d.span else { return head };
        let Some(file) = self.file(span.file) else {
            return head;
        };
        let lc = file.line_col(span.start);
        let line = file.line_text(lc.line);
        let width = file
            .slice(span.start, span.end.max(span.start))
            .lines()
            .next()
            .map_or(0, |l| l.chars().count())
            .max(1);
        format!(
            "{}:{}:{}: {head}\n  {line}\n  {}{}",
            file.path.display(),
            lc.line,
            lc.column,
            " ".repeat(lc.column - 1),
            "^".repeat(width.min(line.chars().count().saturating_sub(lc.column - 1).max(1)))
        )
    }
}

// ----------------------------------------------------------------------------------------------
// Diagnostics
// ----------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Severity::Error => "error",
            Severity::Warning => "warning",
        })
    }
}

/// The pass that reported a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Phase {
    /// Finding and reading the files of `\\module Name;` declarations.
    Load,
    Parse,
    Resolve,
    Lower,
    Check,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Phase::Load => "load",
            Phase::Parse => "parse",
            Phase::Resolve => "resolve",
            Phase::Lower => "lower",
            Phase::Check => "check",
        })
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub phase: Phase,
    pub message: String,
    pub span: Option<Span>,
}

// ----------------------------------------------------------------------------------------------
// Span index
// ----------------------------------------------------------------------------------------------

/// Spans of IR nodes by id. Covers terms, cost expressions, sort references, cost types, ruleset
/// references (by `RuleSetRef::id`), and the declarations that have a span (sorts, rules, schema
/// variables, rulesets, cost items and parameters).
#[derive(Debug, Clone, Default)]
pub struct SpanIndex {
    ir: HashMap<IrId, Span>,
    defs: HashMap<DefId, Span>,
}

impl SpanIndex {
    pub fn ir_span(&self, id: IrId) -> Option<Span> {
        self.ir.get(&id).copied()
    }

    pub fn def_span(&self, id: DefId) -> Option<Span> {
        self.defs.get(&id).copied()
    }

    fn build<'ir>(map: &'ir Map<'ir>) -> Self {
        let mut b = SpanBuilder {
            map,
            index: SpanIndex::default(),
        };
        b.visit_file(&map.file);
        b.index
    }

    /// The best span for a check result: the node it names, else the declaration it names.
    pub fn check_err_span(&self, e: &CheckErr) -> Option<Span> {
        e.id()
            .and_then(|i| self.ir_span(i))
            .or_else(|| e.def_id().and_then(|d| self.def_span(d)))
    }
}

struct SpanBuilder<'ir> {
    map: &'ir Map<'ir>,
    index: SpanIndex,
}

impl<'ir> Visit<'ir> for SpanBuilder<'ir> {
    fn visit_item_id(&mut self, x: &'ir ItemId) {
        self.visit_item(self.map.items[x]);
    }

    fn visit_term(&mut self, x: &'ir Term<'ir>) {
        self.index.ir.insert(x.id, x.span);
        visit::visit_term(self, x);
    }

    fn visit_expr(&mut self, x: &'ir Expr<'ir>) {
        self.index.ir.insert(x.id, x.span);
        visit::visit_expr(self, x);
    }

    fn visit_sort_ref(&mut self, x: &'ir SortRef<'ir>) {
        self.index.ir.insert(x.id, x.span);
        visit::visit_sort_ref(self, x);
    }

    fn visit_cost_ty(&mut self, x: &'ir CostTy<'ir>) {
        self.index.ir.insert(x.id, x.span);
        visit::visit_cost_ty(self, x);
    }

    fn visit_rule_set_ref(&mut self, x: &'ir RuleSetRef<'ir>) {
        self.index.ir.insert(x.id, x.path.span);
        visit::visit_rule_set_ref(self, x);
    }

    fn visit_module(&mut self, x: &'ir Module<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_module(self, x);
    }

    fn visit_sort_decl(&mut self, x: &'ir SortDecl<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_sort_decl(self, x);
    }

    fn visit_rule(&mut self, x: &'ir Rule<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_rule(self, x);
    }

    fn visit_schema_var_decl(&mut self, x: &'ir SchemaVarDecl<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_schema_var_decl(self, x);
    }

    fn visit_rule_set_decl(&mut self, x: &'ir RuleSetDecl<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_rule_set_decl(self, x);
    }

    fn visit_cost_item(&mut self, x: &'ir CostItem<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_cost_item(self, x);
    }

    fn visit_cost_param(&mut self, x: &'ir CostParam<'ir>) {
        self.index.defs.insert(x.id, x.span);
        visit::visit_cost_param(self, x);
    }
}

// ----------------------------------------------------------------------------------------------
// Results
// ----------------------------------------------------------------------------------------------

/// A successfully parsed and checked theory file.
pub struct Parsed {
    pub sources: SourceMap,
    pub spans: SpanIndex,
    /// The symbol interner; needed to read any [`Symbol`] of the IR.
    pub interner: &'static Interner<Symbol>,
    /// The arena the IR is allocated in (later passes may allocate into it as well).
    pub arena: &'static DroplessArena,
    pub map: &'static Map<'static>,
    /// The table that maps parser offsets to files (needed to lower further parser output).
    pub files: &'static FileTable,
    /// Warnings of the front end (errors would have produced an [`Error`]).
    pub warnings: Vec<Diagnostic>,
}

impl Parsed {
    pub fn name(&self, sym: Symbol) -> &str {
        self.interner.get(sym)
    }

    /// The original text a span covers.
    pub fn snippet(&self, span: Span) -> &str {
        self.sources.snippet(span)
    }
}

impl fmt::Debug for Parsed {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Parsed")
            .field("files", &self.sources.files.len())
            .field("items", &self.map.items.len())
            .field("warnings", &self.warnings.len())
            .finish()
    }
}

/// A file that is not valid. Holds the sources, so that its diagnostics can be rendered.
#[derive(Debug, Clone)]
pub struct Failure {
    pub sources: SourceMap,
    /// Errors, and the warnings reported up to the failing pass.
    pub diagnostics: Vec<Diagnostic>,
}

impl Failure {
    pub fn errors(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
    }

    pub fn render(&self) -> String {
        self.diagnostics
            .iter()
            .map(|d| self.sources.render(d))
            .collect::<Vec<_>>()
            .join("\n")
    }
}

#[derive(Debug)]
pub enum Error {
    Io { path: PathBuf, source: io::Error },
    Invalid(Box<Failure>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io { path, source } => write!(f, "cannot read {}: {source}", path.display()),
            Error::Invalid(failure) => f.write_str(&failure.render()),
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io { source, .. } => Some(source),
            Error::Invalid(_) => None,
        }
    }
}

// ----------------------------------------------------------------------------------------------
// Entry points
// ----------------------------------------------------------------------------------------------

/// Parses, resolves, lowers and checks the `.theokey` file at `path`, and the files of all the
/// modules it declares with `\module Name;` (looked up as `Name.theokey` in the directory of the
/// file that declares them).
pub fn parse(path: impl AsRef<Path>) -> Result<Parsed, Error> {
    let path = path.as_ref();
    let text = fs::read_to_string(path).map_err(|source| Error::Io {
        path: path.to_path_buf(),
        source,
    })?;
    parse_text(path.to_path_buf(), text)
}

/// Like [`parse`] for a root file whose text is already in memory; `path` names it for display
/// and decides where its `\module Name;` files are looked up.
pub fn parse_text(path: PathBuf, text: String) -> Result<Parsed, Error> {
    let dir = path.parent().map(Path::to_path_buf).unwrap_or_default();
    let mut loader = Loader {
        sources: SourceMap::default(),
        pcx: ParserCtx::new(),
        diagnostics: Vec::new(),
        loaded: HashSet::new(),
    };
    loader
        .loaded
        .insert(fs::canonicalize(&path).unwrap_or_else(|_| path.clone()));
    let raw = loader.parse_source(path, text);
    let raw = raw.map(|mut file| {
        loader.load_modules(&mut file.items, &dir);
        file
    });
    let Loader {
        sources,
        pcx,
        mut diagnostics,
        ..
    } = loader;

    macro_rules! fail {
        () => {
            return Err(Error::Invalid(Box::new(Failure {
                sources,
                diagnostics,
            })))
        };
    }

    let Some(raw) = raw else { fail!() };
    check_problems(&raw, &sources, &mut diagnostics);
    if diagnostics.iter().any(|d| d.severity == Severity::Error) {
        fail!()
    }

    let arena: &'static DroplessArena = Box::leak(Box::default());
    let interner: &'static Interner<Symbol> = Box::leak(Box::new(pcx.interner));
    let files: &'static FileTable = Box::leak(Box::new(sources.table()));
    let cx = Ctx {
        ir_arena: arena,
        interner,
        files,
    };

    // Resolve
    let resolver = resolve(cx, &raw);
    if !resolver.errors.is_empty() {
        for e in &resolver.errors {
            diagnostics.push(Diagnostic {
                severity: Severity::Error,
                phase: Phase::Resolve,
                message: e.format(cx),
                span: Some(sources.to_span(*e.span())),
            });
        }
        fail!()
    }

    // Lower
    let (map, unresolved) = ir::lower::lower(raw, cx, resolver);
    if !unresolved.is_empty() {
        for s in unresolved {
            diagnostics.push(Diagnostic {
                severity: Severity::Error,
                phase: Phase::Lower,
                message: "No resolution found".to_string(),
                span: Some(s),
            });
        }
        fail!()
    }
    let map: &'static Map<'static> = Box::leak(Box::new(map));

    // Check
    let spans = SpanIndex::build(map);
    let mut failed = false;
    for e in check::check_ir(map, arena, interner) {
        let warning = e.is_warning();
        failed |= !warning;
        diagnostics.push(Diagnostic {
            severity: if warning {
                Severity::Warning
            } else {
                Severity::Error
            },
            phase: Phase::Check,
            message: e.format(),
            span: spans.check_err_span(&e),
        });
    }
    if failed {
        fail!()
    }

    Ok(Parsed {
        sources,
        spans,
        interner,
        arena,
        map,
        files,
        warnings: diagnostics,
    })
}

/// There is at most one `\\problem`, at the top level of the main file.
fn check_problems(raw: &raw_theory::File, sources: &SourceMap, diagnostics: &mut Vec<Diagnostic>) {
    fn nested(items: &[ModuleItem], out: &mut Vec<raw_theory::Span>) {
        for i in items {
            match i {
                ModuleItem::Theory(t) if t.problem => out.push(t.name.span),
                ModuleItem::Theory(_) => {}
                ModuleItem::Module(m) => nested(&m.items, out),
            }
        }
    }
    let mut error = |message: &str, span: raw_theory::Span| {
        diagnostics.push(Diagnostic {
            severity: Severity::Error,
            phase: Phase::Parse,
            message: message.to_string(),
            span: Some(sources.to_span(span)),
        })
    };
    let mut seen = false;
    let mut inner = Vec::new();
    for i in &raw.items {
        match i {
            ModuleItem::Theory(t) if t.problem => {
                if seen {
                    error("there can be only one \\problem", t.name.span);
                }
                seen = true;
            }
            ModuleItem::Theory(_) => {}
            ModuleItem::Module(m) => nested(&m.items, &mut inner),
        }
    }
    for s in inner {
        error("a \\problem must be at the top level of the main file", s);
    }
}

/// Reads and parses the files of a program.
struct Loader {
    sources: SourceMap,
    pcx: ParserCtx,
    diagnostics: Vec<Diagnostic>,
    /// Canonical paths of the files read so far; a file may be the body of one module only,
    /// which also rules out cycles.
    loaded: HashSet<PathBuf>,
}

impl Loader {
    fn error(&mut self, phase: Phase, message: String, span: Option<Span>) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            phase,
            message,
            span,
        });
    }

    fn parse_source(&mut self, path: PathBuf, text: String) -> Option<raw_theory::File> {
        let id = self.sources.add(SourceFile::new(path, text));
        let file = self.sources.file(id).expect("just added");
        self.pcx.set_base(file.base);
        match dl_theory::FileParser::new().parse(&mut self.pcx, &file.text) {
            Ok(f) => Some(f),
            Err(e) => {
                self.diagnostics.push(parse_error(id, e));
                None
            }
        }
    }

    /// Fills in the items of every `\module Name;` below `items` from `dir/Name.theokey`.
    fn load_modules(&mut self, items: &mut [ModuleItem], dir: &Path) {
        for item in items {
            let ModuleItem::Module(m) = item else {
                continue;
            };
            if !m.external {
                self.load_modules(&mut m.items, dir);
                continue;
            }
            let name = self.pcx.get_symbol(m.name.node).to_string();
            let span = Some(self.sources.to_span(m.name.span));
            let path = dir.join(format!("{name}.theokey"));
            let text = match fs::read_to_string(&path) {
                Ok(t) => t,
                Err(e) if e.kind() == io::ErrorKind::NotFound => {
                    let msg = format!("module `{name}`: file `{}` not found", path.display());
                    self.error(Phase::Load, msg, span);
                    continue;
                }
                Err(e) => {
                    let msg = format!("module `{name}`: cannot read `{}`: {e}", path.display());
                    self.error(Phase::Load, msg, span);
                    continue;
                }
            };
            let key = fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
            if !self.loaded.insert(key) {
                let msg = format!(
                    "module `{name}`: file `{}` is already loaded (cyclic or repeated module)",
                    path.display()
                );
                self.error(Phase::Load, msg, span);
                continue;
            }
            if let Some(file) = self.parse_source(path, text) {
                m.items = file.items;
                self.load_modules(&mut m.items, dir);
            }
        }
    }
}

fn parse_error(file: FileId, e: ParseError<usize, dl_theory::Token<'_>, &str>) -> Diagnostic {
    let range = match &e {
        ParseError::InvalidToken { location } => Some((*location, *location + 1)),
        ParseError::UnrecognizedEof { location, .. } => Some((*location, *location)),
        ParseError::UnrecognizedToken { token, .. } | ParseError::ExtraToken { token } => {
            Some((token.0, token.2))
        }
        ParseError::User { .. } => None,
    };
    Diagnostic {
        severity: Severity::Error,
        phase: Phase::Parse,
        message: e.to_string(),
        span: range.map(|(start, end)| Span { file, start, end }),
    }
}

#[cfg(test)]
mod tests {
    use crate::ir::LOCAL_FILE;

    use super::*;

    fn text(src: &str) -> Result<Parsed, Error> {
        parse_text(PathBuf::from("test.theokey"), src.to_string())
    }

    fn failure(src: &str) -> Box<Failure> {
        match text(src) {
            Err(Error::Invalid(f)) => f,
            other => panic!("expected a failure, got {other:?}"),
        }
    }

    #[test]
    fn parses_the_example_file() {
        let p = parse("examples/RustyDL.theokey").unwrap_or_else(|e| panic!("{e}"));
        assert_eq!(6, p.map.file.theories().len());
        let first = p.map.file.theories()[0];
        assert_eq!("Prop", p.name(first.name.name));
        // Spans lead back to the text.
        assert_eq!("Prop", p.snippet(first.name.span));
    }

    #[test]
    fn missing_file_is_an_io_error() {
        assert!(matches!(
            parse("examples/does-not-exist.theokey"),
            Err(Error::Io { .. })
        ));
    }

    #[test]
    fn parse_errors_have_spans() {
        let f = failure("\\theory T { \\sorts {} \\rulesets { a { 1 + } } }");
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Parse, d.phase);
        let span = d.span.unwrap();
        assert_eq!("}", f.sources.snippet(span).trim());
        assert!(f.render().contains("test.theokey:1:"));
    }

    #[test]
    fn resolve_errors_have_spans() {
        let f = failure("\\theory T {\n  \\sorts {}\n  \\rulesets { a { nope } }\n}\n");
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Resolve, d.phase);
        let span = d.span.unwrap();
        assert_eq!("nope", f.sources.snippet(span).trim());
        let (_, lc) = f.sources.location(span).unwrap();
        assert_eq!(3, lc.line);
    }

    #[test]
    fn check_errors_have_spans_and_warnings_are_kept() {
        let f = failure(
            "\\theory T {\n  \\sorts {}\n  \\rulesets { a { if 1 { 0 } else { 1 } } }\n}\n",
        );
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Check, d.phase);
        assert_eq!("1", f.sources.snippet(d.span.unwrap()).trim());

        let p = text("\\theory T { \\sorts {} \\rulesets { c \\stage(app) { Cost::REWRITE } } }")
            .unwrap_or_else(|e| panic!("{e}"));
        assert_eq!(1, p.warnings.len());
        assert_eq!(Severity::Warning, p.warnings[0].severity);
        assert!(p.warnings[0].span.is_some());
    }

    /// Files in a fresh temporary directory, removed on drop.
    struct TempDir(PathBuf);

    impl TempDir {
        fn new(files: &[(&str, &str)]) -> Self {
            use std::sync::atomic::{AtomicUsize, Ordering};
            static N: AtomicUsize = AtomicUsize::new(0);
            let dir = std::env::temp_dir().join(format!(
                "dale-syn-test-{}-{}",
                std::process::id(),
                N.fetch_add(1, Ordering::SeqCst)
            ));
            fs::create_dir_all(&dir).unwrap();
            for (name, text) in files {
                fs::write(dir.join(name), text).unwrap();
            }
            TempDir(dir)
        }

        fn path(&self, name: &str) -> PathBuf {
            self.0.join(name)
        }
    }

    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }

    const PROP: &str = "\\theory Prop { \\sorts { \\top Any; \\formula Formula; } }";

    fn failure_of(files: &[(&str, &str)]) -> Box<Failure> {
        let dir = TempDir::new(files);
        match parse(dir.path("root.theokey")) {
            Err(Error::Invalid(f)) => f,
            other => panic!("expected a failure, got {other:?}"),
        }
    }

    #[test]
    fn modules_in_other_files() {
        let top = "\\theory Top { \\use Base::Prop::*; \\operators { \\rigid neg(Formula) -> Formula; } }";
        // `Top` may come before or after the module it uses.
        for root in [
            format!("\\module Base;\n{top}"),
            format!("{top}\n\\module Base;"),
        ] {
            let dir = TempDir::new(&[("root.theokey", &root), ("Base.theokey", PROP)]);
            let p = parse(dir.path("root.theokey")).unwrap_or_else(|e| panic!("{e}"));
            let theories = p.map.file.theories();
            assert_eq!(2, theories.len());
            let prop = theories
                .iter()
                .find(|t| p.name(t.name.name) == "Prop")
                .unwrap();
            // The span points into the file of the module, not into the root file.
            assert_ne!(LOCAL_FILE, prop.name.span.file);
            assert_eq!("Prop", p.snippet(prop.name.span));
            let (path, lc) = p.sources.location(prop.name.span).unwrap();
            assert!(path.ends_with("Base.theokey"));
            assert_eq!(1, lc.line);
        }
    }

    #[test]
    fn nested_modules() {
        let root = "\\module Outer { \\module Inner; \\theory A { \\use Inner::T::*; } }\n\
                    \\theory R { \\use Outer::Inner::T::*; }";
        let dir = TempDir::new(&[
            ("root.theokey", root),
            ("Inner.theokey", &PROP.replace("Prop", "T")),
        ]);
        let p = parse(dir.path("root.theokey")).unwrap_or_else(|e| panic!("{e}"));
        let names: Vec<_> = p
            .map
            .file
            .theories()
            .iter()
            .map(|t| p.name(t.name.name).to_string())
            .collect();
        assert_eq!(["T", "A", "R"], names[..]);
    }

    #[test]
    fn missing_module_file() {
        let f = failure_of(&[("root.theokey", "\\module Missing;")]);
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Load, d.phase);
        assert!(d.message.contains("not found"), "{}", d.message);
        assert_eq!("Missing", f.sources.snippet(d.span.unwrap()));
    }

    #[test]
    fn modules_and_theories_share_a_namespace() {
        let f = failure_of(&[("root.theokey", "\\theory X {}\n\\module X { }")]);
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Resolve, d.phase);
        assert!(d.message.contains("Already an element"), "{}", d.message);
        // Also for a module that lives in its own file.
        let f = failure_of(&[
            ("root.theokey", "\\theory X {}\n\\module X;"),
            ("X.theokey", ""),
        ]);
        assert_eq!(Phase::Resolve, f.errors().next().unwrap().phase);
        // The same name in different modules is fine.
        let dir = TempDir::new(&[(
            "root.theokey",
            "\\theory X {}\n\\module M { \\theory X {} }",
        )]);
        parse(dir.path("root.theokey")).unwrap_or_else(|e| panic!("{e}"));
    }

    #[test]
    fn module_files_cannot_load_themselves() {
        let f = failure_of(&[
            ("root.theokey", "\\module A;"),
            ("A.theokey", "\\module A;"),
        ]);
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Load, d.phase);
        assert!(d.message.contains("already loaded"), "{}", d.message);
    }

    #[test]
    fn use_cycles_are_errors() {
        let f = failure_of(&[(
            "root.theokey",
            "\\theory X { \\use Y::*; }\n\\theory Y { \\use X::*; }",
        )]);
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Resolve, d.phase);
        assert!(d.message.contains("cyclic"), "{}", d.message);
    }

    #[test]
    fn use_paths_must_name_theories() {
        let f = failure_of(&[("root.theokey", "\\module M { }\n\\theory X { \\use M::*; }")]);
        assert!(f.errors().next().unwrap().message.contains("not a theory"));
        let f = failure_of(&[("root.theokey", "\\theory X { \\use Nope::*; }")]);
        assert!(
            f.errors()
                .next()
                .unwrap()
                .message
                .contains("Failed to resolve")
        );
    }

    #[test]
    fn errors_in_module_files_point_into_that_file() {
        let f = failure_of(&[
            ("root.theokey", "\\module Bad;"),
            ("Bad.theokey", "\\theory T {\n  \\sorts { ??? }\n}"),
        ]);
        let d = f.errors().next().unwrap();
        assert_eq!(Phase::Parse, d.phase);
        let (path, lc) = f.sources.location(d.span.unwrap()).unwrap();
        assert!(path.ends_with("Bad.theokey"));
        assert_eq!(2, lc.line);
    }

    #[test]
    fn line_and_column() {
        let f = SourceFile::new(PathBuf::from("x"), "ab\ncdé\nf".to_string());
        assert_eq!(LineCol { line: 1, column: 1 }, f.line_col(0));
        assert_eq!(LineCol { line: 2, column: 3 }, f.line_col(5));
        assert_eq!(LineCol { line: 3, column: 1 }, f.line_col(8));
        assert_eq!("cdé", f.line_text(2));
        assert_eq!("f", f.line_text(3));
    }
}
