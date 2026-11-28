use std::collections::HashMap;

use dale_index::Idx;
use dale_macros::newtype_index;
use dale_util::symbol::Symbol;

use crate::{check::Sort, cost::Stage, resolve::PerNS};

pub use crate::raw_theory::{BinOp, RuleSetExt, UnOp};

pub mod lower;
pub mod visit;

newtype_index! {
  #[orderable]
  #[debug_format = "FileId({})"]
  pub struct FileId {}
}

pub const LOCAL_FILE: FileId = FileId::ZERO;

newtype_index! {
  #[orderable]
  #[debug_format = "IrId({})"]
  pub struct IrId {}
}

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
pub struct DefId {
    pub index: DefIndex,
    pub file: FileId,
}

/// A `LocalDefId` is equivalent to a `DefId` with `file == LOCAL_FILE`. Since
/// we encode this information in the type, we can ensure at compile time that
/// no `DefId`s from upstream files get thrown into the mix. There are quite a
/// few cases where we know that only `DefId`s from the local file are expected;
/// a `DefId` from a different file would signify a bug somewhere. This
/// is when `LocalDefId` comes in handy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LocalDefId {
    pub local_def_index: DefIndex,
}

pub const FILE_DEF_ID: LocalDefId = LocalDefId {
    local_def_index: FILE_DEF_INDEX,
};

impl Idx for LocalDefId {
    #[inline]
    fn new(idx: usize) -> Self {
        LocalDefId {
            local_def_index: Idx::new(idx),
        }
    }
    #[inline]
    fn index(self) -> usize {
        self.local_def_index.index()
    }
}

impl LocalDefId {
    #[inline]
    pub fn to_def_id(self) -> DefId {
        DefId {
            file: LOCAL_FILE,
            index: self.local_def_index,
        }
    }

    #[inline]
    pub fn is_top_level(self) -> bool {
        self == FILE_DEF_ID
    }
}

newtype_index! {
    #[orderable]
    #[debug_format = "DefIndex({})"]
    pub struct DefIndex {
        /// The file root is always assigned index 0 by the AST Map code
        const FILE_DEF_INDEX = 0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId {
    pub id: IrId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub span: Span,
    pub name: Symbol,
}

#[derive(Debug, Clone)]
pub struct Map<'ir> {
    pub file: File<'ir>,
    pub items: HashMap<ItemId, &'ir Item<'ir>>,
    pub defs: HashMap<DefId, Def<'ir>>,
    pub sorts: HashMap<IrId, Sort<'ir>>,
}

impl<'ir> Map<'ir> {
    pub fn get_def(&self, def_id: DefId) -> Def<'ir> {
        self.defs[&def_id]
    }

    pub fn get_sort(&self, def_id: DefId) -> SortDecl<'ir> {
        self.get_def(def_id).expect_sort_decl()
    }

    /// The sequent of the `\\problem`, if the input has one.
    pub fn problem(&self) -> Option<&'ir Seq<'ir>> {
        self.file.items.iter().find_map(|i| match i {
            ModuleItem::Theory(t) if t.problem => {
                t.items.iter().find_map(|id| match self.items[id].kind {
                    ItemKind::Problem(seq) => Some(seq),
                    _ => None,
                })
            }
            _ => None,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Def<'ir> {
    SortDecl(SortDecl<'ir>),
    OpDecl(OperatorDecl<'ir>),
    FnDecl(FunctionDecl<'ir>),
    PredDecl(PredicateDecl<'ir>),
    SchemaVar(SchemaVarDecl<'ir>),
    GenParam(GenericParam<'ir>),
    /// A `const` or `fn` of a `\costs` block.
    CostItem(CostItem<'ir>),
    /// A parameter of a ruleset or of a cost function.
    CostParam(CostParam<'ir>),
    /// A `let` or binder variable of a cost expression.
    CostLocal(Ident),
}

impl<'ir> Def<'ir> {
    pub fn expect_sort_decl(&self) -> SortDecl<'ir> {
        match self {
            Self::SortDecl(s) => *s,
            _ => panic!("Expected sort decl"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct File<'ir> {
    pub id: FileId,
    pub items: &'ir [ModuleItem<'ir>],
}

impl<'ir> File<'ir> {
    /// All theories of the file, including those in (nested) modules, in declaration order.
    pub fn theories(&self) -> Vec<Theory<'ir>> {
        let mut out = Vec::new();
        collect_theories(self.items, &mut out);
        out
    }
}

fn collect_theories<'ir>(items: &'ir [ModuleItem<'ir>], out: &mut Vec<Theory<'ir>>) {
    for i in items {
        match i {
            ModuleItem::Theory(t) => out.push(*t),
            ModuleItem::Module(m) => collect_theories(m.items, out),
        }
    }
}

/// What a file or a module contains. Modules loaded from their own file (`\module Name;`) are
/// lowered like inline ones; the spans inside tell the file apart.
#[derive(Debug, Clone, Copy)]
pub enum ModuleItem<'ir> {
    Theory(Theory<'ir>),
    Module(Module<'ir>),
}

#[derive(Debug, Clone, Copy)]
pub struct Module<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub span: Span,
    /// Declared as `\module Name;`, i.e. loaded from `Name.theokey`.
    pub external: bool,
    pub items: &'ir [ModuleItem<'ir>],
}

#[derive(Debug, Clone, Copy)]
pub struct Theory<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub items: &'ir [ItemId],
    /// A `\\problem`: only uses and, last, an [`ItemKind::Problem`].
    pub problem: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Path<'ir, R = Res> {
    pub span: Span,
    pub res: R,
    pub segments: &'ir [PathSegment],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PathSegment {
    pub id: IrId,
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Item<'ir> {
    pub id: ItemId,
    pub kind: ItemKind<'ir>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub enum ItemKind<'ir> {
    Use(&'ir UsePath<'ir>, UseKind),
    Operators(&'ir [OperatorDecl<'ir>]),
    Sorts(&'ir [SortDecl<'ir>]),
    DataTypes(DataTypeDecls),
    Functions(&'ir [FunctionDecl<'ir>]),
    Predicates(&'ir [PredicateDecl<'ir>]),
    Axioms(),
    Rules(&'ir [Rule<'ir>]),
    RuleSets(&'ir [RuleSetDecl<'ir>]),
    Costs(&'ir [CostItem<'ir>]),
    /// The sequent of a `\\problem`.
    Problem(&'ir Seq<'ir>),
}

pub type UsePath<'ir> = Path<'ir, PerNS<Option<Res>>>;

#[derive(Debug, Clone, Copy)]
pub enum UseKind {
    Single(Ident),
    Glob,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionDecl<'ir> {
    pub id: DefId,
    pub modifiers: FunctionModifiers,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub bind: Option<BindSpec<'ir>>,
    pub arg_sort_refs: &'ir [SortRef<'ir>],
    pub sort_ref: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionModifiers {
    pub rigid: bool,
    pub unique: bool,
    pub skolem: bool,
    /// `\\boundVar`: the bound variable operator.
    pub bound_var: bool,
}

/// `{S: 0, 1}`: the operator binds a variable of sort `S` in the arguments 0 and 1.
#[derive(Debug, Clone, Copy)]
pub struct BindSpec<'ir> {
    pub sort: &'ir SortRef<'ir>,
    pub args: &'ir [u8],
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct OperatorDecl<'ir> {
    pub id: DefId,
    pub rigid: bool,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub bind: Option<BindSpec<'ir>>,
    pub arg_sort_refs: &'ir [SortRef<'ir>],
    pub sort_ref: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub struct SortDecl<'ir> {
    pub id: DefId,
    pub modifiers: SortModifiers,
    pub span: Span,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub extends: Option<(Span, &'ir [SortRef<'ir>])>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SortModifiers {
    pub formula: bool,
    pub meta: bool,
    pub top: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct DataTypeDecls {}

#[derive(Debug, Clone, Copy)]
pub struct PredicateDecl<'ir> {
    pub id: DefId,
    pub rigid: bool,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub bind: Option<BindSpec<'ir>>,
    pub arg_sort_refs: &'ir [SortRef<'ir>],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SortRef<'ir> {
    pub id: IrId,
    pub span: Span,
    pub path: &'ir Path<'ir>,
    pub args: &'ir [GenericArg<'ir>],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GenericArg<'ir> {
    Sort(&'ir SortRef<'ir>),
    Const(&'ir Term<'ir>, Span),
}

#[derive(Debug, Clone, Copy)]
pub struct GenericParam<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub kind: GenericParamKind<'ir>,
    pub colon_span: Option<Span>,
}

#[derive(Debug, Clone, Copy)]
pub enum GenericParamKind<'ir> {
    Sort,
    Const { sort: &'ir SortRef<'ir>, span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Term<'ir> {
    pub id: IrId,
    pub span: Span,
    pub kind: TermKind<'ir>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TermKind<'ir> {
    Path(&'ir Path<'ir>, &'ir [GenericArg<'ir>]),
    Call(&'ir Path<'ir>, &'ir [GenericArg<'ir>], &'ir [Term<'ir>]),
    /// A number literal; only in problems, as a meta argument (`var(0)`).
    Num(u64),
}

#[derive(Debug, Clone, Copy)]
pub struct Rule<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub schema_vars: &'ir [SchemaVarDecl<'ir>],
    pub assumes: Option<(Span, TermOrSeq<'ir>)>,
    pub find: Option<(Span, TermOrSeq<'ir>)>,
    pub goal_specs: GoalSpecs<'ir>,
    pub rule_sets: Option<(Span, &'ir [RuleSetRef<'ir>])>,
    pub display_name: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct SchemaVarDecl<'ir> {
    pub id: DefId,
    pub span: Span,
    pub name: Ident,
    pub sort: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub enum TermOrSeq<'ir> {
    Term(&'ir Term<'ir>),
    Seq(&'ir Seq<'ir>),
}

#[derive(Debug, Clone, Copy)]
pub struct Seq<'ir> {
    pub id: IrId,
    pub ante: &'ir [Term<'ir>],
    pub succ: &'ir [Term<'ir>],
}

#[derive(Debug, Clone, Copy)]
pub enum GoalSpecs<'ir> {
    CloseGoal(IrId, Span),
    Specs(&'ir [GoalSpec<'ir>], Span),
}

#[derive(Debug, Clone, Copy)]
pub struct GoalSpec<'ir> {
    pub id: IrId,
    pub name: Option<Ident>,
    pub replace_with: Option<TermOrSeq<'ir>>,
    pub add: Option<TermOrSeq<'ir>>,
    pub span: Span,
}

/// A ruleset a rule belongs to, with the arguments for the ruleset's parameters.
#[derive(Debug, Clone, Copy)]
pub struct RuleSetRef<'ir> {
    pub id: IrId,
    pub path: Path<'ir>,
    pub args: &'ir [Expr<'ir>],
}

#[derive(Debug, Clone, Copy)]
pub struct RuleSetDecl<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub span: Span,
    /// `extend` / `override`: this declaration changes the ruleset `base` of a used theory
    /// instead of introducing a new one.
    pub ext: Option<RuleSetExt>,
    pub base: Option<Res>,
    pub params: &'ir [CostParam<'ir>],
    /// The asserted stage of the body (`\stage(...)`), if any.
    pub stage: Option<(Span, Stage)>,
    /// Vetoes (builtin guards, e.g. `no_duplicate_app`) that are not part of the cost.
    pub guards: &'ir [Path<'ir>],
    pub body: &'ir Expr<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub struct CostItem<'ir> {
    pub id: DefId,
    pub span: Span,
    pub name: Ident,
    pub kind: CostItemKind<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub enum CostItemKind<'ir> {
    Const {
        ty: CostTy<'ir>,
        value: &'ir Expr<'ir>,
    },
    Fn {
        params: &'ir [CostParam<'ir>],
        ret: CostTy<'ir>,
        body: &'ir Expr<'ir>,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct CostParam<'ir> {
    pub id: DefId,
    pub span: Span,
    pub name: Ident,
    pub ty: CostTy<'ir>,
}

/// `Cost`, `Int`, `Float`, `Bool`, `Op`, `Sort`, `Term` or `Term<S>`; `arg` is the sort `S`.
#[derive(Debug, Clone, Copy)]
pub struct CostTy<'ir> {
    pub id: IrId,
    pub span: Span,
    pub name: Ident,
    pub arg: Option<&'ir SortRef<'ir>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'ir> {
    pub id: IrId,
    pub span: Span,
    pub kind: ExprKind<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub enum ExprKind<'ir> {
    Int(i64),
    Float(f64),
    /// The infinite cost.
    Top,
    /// Resolved to a cost constant / function, a parameter, a local, a builtin, or (in the
    /// argument of a rule's `\rulesets`) a schema variable or operator.
    Path(&'ir Path<'ir>),
    Call(&'ir Path<'ir>, &'ir [CallArg<'ir>]),
    Unary(UnOp, &'ir Expr<'ir>),
    Binary(BinOp, &'ir Expr<'ir>, &'ir Expr<'ir>),
    If(&'ir Expr<'ir>, &'ir Expr<'ir>, &'ir Expr<'ir>),
    Let {
        def: DefId,
        name: Ident,
        value: &'ir Expr<'ir>,
        body: &'ir Expr<'ir>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum CallArg<'ir> {
    Expr(Expr<'ir>),
    Binder(Binder<'ir>),
}

/// `x in generator: body`
#[derive(Debug, Clone, Copy)]
pub struct Binder<'ir> {
    pub def: DefId,
    pub span: Span,
    pub var: Ident,
    pub generator: &'ir Expr<'ir>,
    pub body: &'ir Expr<'ir>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Res {
    Def(DefKind, DefId),
    /// A builtin of the cost language (a function, a constant `Cost::NAME`, or a guard).
    Builtin(Symbol),
    Err,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DefKind {
    File,
    Module,
    Theory,
    Use,
    Operator,
    Sort,
    DataType,
    Pred,
    Fn,
    Rule,
    SortParam,
    ConstParam,
    SchemaVar,
    RuleSet,
    CostConst,
    CostFn,
    CostParam,
    CostLocal,
}
