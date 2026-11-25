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
    collections::HashMap,
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
    ctx::Ctx,
    dl_theory,
    ir::{
        self, CostItem, CostParam, CostTy, DefId, Expr, FileId, IrId, ItemId, LOCAL_FILE, Map,
        Rule, RuleSetDecl, RuleSetRef, SchemaVarDecl, SortDecl, SortRef, Span, Term,
        visit::{self, Visit},
    },
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
    pub fn add(&mut self, file: SourceFile) -> FileId {
        self.files.push(file);
        FileId::new(self.files.len() - 1)
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
    Parse,
    Resolve,
    Lower,
    Check,
}

impl fmt::Display for Phase {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
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

/// Parses, resolves, lowers and checks the `.theokey` file at `path`.
pub fn parse(path: impl AsRef<Path>) -> Result<Parsed, Error> {
    let path = path.as_ref();
    let text = fs::read_to_string(path).map_err(|source| Error::Io {
        path: path.to_path_buf(),
        source,
    })?;
    parse_text(path.to_path_buf(), text)
}

/// Like [`parse`] for text that is already in memory; `path` is only used for display.
pub fn parse_text(path: PathBuf, text: String) -> Result<Parsed, Error> {
    let mut sources = SourceMap::default();
    let file_id = sources.add(SourceFile::new(path, text));
    debug_assert_eq!(file_id, LOCAL_FILE);
    let mut diagnostics = Vec::new();

    macro_rules! fail {
        () => {
            return Err(Error::Invalid(Box::new(Failure {
                sources,
                diagnostics,
            })))
        };
    }

    // Parse
    let mut pcx = ParserCtx::new();
    let raw = {
        let input = &sources.file(file_id).expect("just added").text;
        match dl_theory::FileParser::new().parse(&mut pcx, input) {
            Ok(f) => Some(f),
            Err(e) => {
                diagnostics.push(parse_error(file_id, e));
                None
            }
        }
    };
    let Some(raw) = raw else { fail!() };

    let arena: &'static DroplessArena = Box::leak(Box::default());
    let interner: &'static Interner<Symbol> = Box::leak(Box::new(pcx.interner));
    let cx = Ctx {
        ir_arena: arena,
        interner,
    };

    // Resolve
    let resolver = resolve(cx, &raw);
    if !resolver.errors.is_empty() {
        for e in &resolver.errors {
            let s = e.span();
            diagnostics.push(Diagnostic {
                severity: Severity::Error,
                phase: Phase::Resolve,
                message: e.format(cx),
                span: Some(Span {
                    file: file_id,
                    start: s.0,
                    end: s.1,
                }),
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
        warnings: diagnostics,
    })
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
        assert_eq!(6, p.map.file.theories.len());
        let first = p.map.file.theories[0];
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
