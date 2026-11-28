use dale_macros::newtype_index;
use dale_util::symbol::Symbol;

mod visit;
pub use visit::*;

newtype_index! {
    /// Identifies an AST node.
    #[orderable]
    #[debug_format = "NodeId({})"]
    pub struct NodeId {}
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }
}

pub type Ident = Spanned<Symbol>;

#[derive(Debug, Clone)]
pub struct File {
    pub id: NodeId,
    pub items: Vec<ModuleItem>,
}

/// What a file or a module contains.
#[derive(Debug, Clone)]
pub enum ModuleItem {
    Theory(Theory),
    Module(Module),
}

/// `\module Name { ... }` (inline) or `\module Name;` (`external`: the items come from the file
/// `Name.theokey`, which the loader fills in; the parser leaves `items` empty).
#[derive(Debug, Clone)]
pub struct Module {
    pub id: NodeId,
    pub name: Ident,
    pub span: Span,
    pub external: bool,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug, Clone)]
pub struct Theory {
    pub id: NodeId,
    pub name: Ident,
    pub items: Vec<Item>,
    /// A `\\problem`: only `\\use` items and, last, an [`ItemKind::Problem`].
    pub problem: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span(pub usize, pub usize);

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub id: NodeId,
    pub ident: Ident,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Item {
    pub id: NodeId,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Use(UseTree),
    Operators(OperatorDecls),
    Sorts(SortDecls),
    DataTypes(DataTypeDecls),
    Functions(FunctionDecls),
    Predicates(PredicateDecls),
    Axioms(),
    Rules(Rules),
    RuleSets(RuleSetDecls),
    Costs(Vec<CostItem>),
    /// The sequent of a `\\problem`.
    Problem(Seq),
}

#[derive(Debug, Clone)]
pub struct UseTree {
    pub prefix: Path,
    pub kind: UseTreeKind,
    pub span: Span,
}

impl UseTree {
    pub fn ident(&self) -> Ident {
        match self.kind {
            UseTreeKind::Simple(Some(rename)) => rename,
            UseTreeKind::Simple(None) => {
                self.prefix
                    .segments
                    .last()
                    .expect("empty prefix in a simple ident")
                    .ident
            }
            _ => panic!("`UseTree::ident` can only be used on a simple import"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum UseTreeKind {
    Simple(Option<Ident>),
    Nested {
        items: Vec<(UseTree, NodeId)>,
        span: Span,
    },
    Glob,
}

pub type OperatorDecls = Vec<FunctionDecl>;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub modifiers: FunctionModifiers,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    /// `{S: 0, 1}`: where the operator binds a variable, and its sort.
    pub bind: Option<BindSpec>,
    pub arg_sorts: Vec<Sort>,
    pub sort: Sort,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionModifiers {
    pub rigid: bool,
    pub unique: bool,
    pub skolem: bool,
    /// `\\boundVar`: the bound variable operator (`var<S>(NUMBER) -> S`).
    pub bound_var: bool,
}

/// `{S: 0, 1}` in an operator declaration: a variable of sort `S` is bound in the arguments 0
/// and 1.
#[derive(Debug, Clone)]
pub struct BindSpec {
    pub sort: Sort,
    pub args: Vec<u8>,
    pub span: Span,
}

pub type SortDecls = Vec<SortDecl>;

#[derive(Debug, Clone)]
pub struct SortDecl {
    pub id: NodeId,
    pub modifiers: SortModifiers,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    /// `\\extends S`: the one direct supersort besides the top sort.
    pub extends: Option<(Span, Sort)>,
}

#[derive(Debug, Clone, Copy)]
pub struct SortModifiers {
    pub formula: bool,
    pub meta: bool,
    pub top: bool,
}

#[derive(Debug, Clone)]
pub struct DataTypeDecls {}

pub type FunctionDecls = Vec<FunctionDecl>;

pub type PredicateDecls = Vec<PredicateDecl>;

#[derive(Debug, Clone)]
pub struct PredicateDecl {
    pub id: NodeId,
    pub rigid: bool,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    /// `{S: 0, 1}`: where the operator binds a variable, and its sort.
    pub bind: Option<BindSpec>,
    pub arg_sorts: Vec<Sort>,
}

#[derive(Debug, Clone)]
pub struct Sort {
    pub id: NodeId,
    pub span: Span,
    pub kind: SortKind,
}

#[derive(Debug, Clone)]
pub enum SortKind {
    Simple(Path),
    Parametric(Path, Vec<GenericArg>),
}

#[derive(Debug, Clone)]
pub enum GenericArg {
    Sort(Sort),
    Const(Term, Span),
}

#[derive(Debug, Clone)]
pub struct GenericParam {
    pub id: NodeId,
    pub name: Ident,
    pub kind: GenericParamKind,
    pub colon_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind {
    Sort,
    Const { sort: Sort, span: Span },
}

#[derive(Debug, Clone)]
pub struct Term {
    pub id: NodeId,
    pub span: Span,
    pub kind: TermKind,
}

#[derive(Debug, Clone)]
pub enum TermKind {
    Path(Path),
    ParametricPath(Path, Vec<GenericArg>),
    Call(Path, Vec<Term>),
    ParametricCall(Path, Vec<GenericArg>, Vec<Term>),
    /// A number literal; only in problems, as a meta argument (`var(0)`).
    Num(u64),
}

type Rules = Vec<Rule>;

#[derive(Debug, Clone)]
pub struct Rule {
    pub id: NodeId,
    pub name: Ident,
    pub schema_vars: Vec<SchemaVarDecl>,
    pub assumes: Option<Spanned<TermOrSeq>>,
    pub find: Option<Spanned<TermOrSeq>>,
    pub goal_specs: GoalSpecs,
    pub rule_sets: Option<Spanned<Vec<RuleSetRef>>>,
    pub display_name: Option<Spanned<Symbol>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SchemaVarDecl {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub sort: Sort,
}

#[derive(Debug, Clone)]
pub enum TermOrSeq {
    Term(Term),
    Seq(Seq),
}

#[derive(Debug, Clone)]
pub struct Seq {
    pub id: NodeId,
    pub ante: Vec<Term>,
    pub succ: Vec<Term>,
}

#[derive(Debug, Clone)]
pub enum GoalSpecs {
    CloseGoal(NodeId, Span),
    Specs(Vec<GoalSpec>, Span),
}

#[derive(Debug, Clone)]
pub struct GoalSpec {
    pub id: NodeId,
    pub name: Option<Ident>,
    pub replace_with: Option<TermOrSeq>,
    pub add: Option<TermOrSeq>,
    pub span: Span,
}

/// A reference to a ruleset from a rule's `\rulesets(...)` list, optionally with arguments for
/// the ruleset's parameters: `\rulesets(alpha, apply_equation(r))`.
#[derive(Debug, Clone)]
pub struct RuleSetRef {
    pub id: NodeId,
    pub path: Path,
    pub args: Vec<Expr>,
}

pub type RuleSetDecls = Vec<RuleSetDecl>;

/// `[extend|override] name[(params)] [\stage(s)] { [guard g;]* cost }`
#[derive(Debug, Clone)]
pub struct RuleSetDecl {
    pub id: NodeId,
    pub span: Span,
    pub ext: Option<RuleSetExt>,
    pub name: Ident,
    pub params: Vec<CostParam>,
    /// The stage the author asserts for the body (checked after name resolution).
    pub stage: Option<Ident>,
    pub guards: Vec<Guard>,
    pub body: Expr,
}

/// A ruleset declaration that refers to a ruleset of a used theory instead of introducing a new one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuleSetExt {
    /// `extend alpha { ... }`: the cost is added to the inherited cost.
    Extend,
    /// `override alpha { ... }`: the cost replaces the inherited cost.
    Override,
}

/// `guard no_duplicate_app;`: a veto that is not part of the cost value.
#[derive(Debug, Clone)]
pub struct Guard {
    pub id: NodeId,
    pub span: Span,
    pub path: Path,
}

/// An item of a `\costs { ... }` block.
#[derive(Debug, Clone)]
pub struct CostItem {
    pub id: NodeId,
    pub span: Span,
    pub kind: CostItemKind,
}

#[derive(Debug, Clone)]
pub enum CostItemKind {
    /// `const NAME: Ty = expr;`
    Const {
        name: Ident,
        ty: CostTy,
        value: Expr,
    },
    /// `fn name(params) -> Ty { expr }`
    Fn {
        name: Ident,
        params: Vec<CostParam>,
        ret: CostTy,
        body: Expr,
    },
}

/// `name: Ty`, a parameter of a ruleset or a cost function.
#[derive(Debug, Clone)]
pub struct CostParam {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub ty: CostTy,
}

/// A type in the cost language: `Cost`, `Int`, `Float`, `Bool`, `Op`, `Sort`, `Term` or
/// `Term<S>` (a term of sort `S`). The name is checked after parsing, only `arg` is a sort
/// that is resolved as such.
#[derive(Debug, Clone)]
pub struct CostTy {
    pub id: NodeId,
    pub span: Span,
    pub name: Ident,
    pub arg: Option<Sort>,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: NodeId,
    pub span: Span,
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    /// The infinite cost.
    Top,
    /// A constant, a parameter, a local, a builtin or (inside a rule's `\rulesets`) a schema
    /// variable or operator; see `resolve.rs` for the lookup order.
    Path(Path),
    Call(Path, Vec<CallArg>),
    Unary(UnOp, Box<Expr>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// `let name = value; body`. The `NodeId` identifies the binder.
    Let(NodeId, Ident, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum CallArg {
    Expr(Expr),
    /// `x in generator: body`, as in `sum(x in sub_terms(find): body)`.
    Binder(Binder),
}

#[derive(Debug, Clone)]
pub struct Binder {
    pub id: NodeId,
    pub span: Span,
    pub var: Ident,
    pub generator: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Lt,
    Le,
    Eq,
    Ne,
    Ge,
    Gt,
    And,
    Or,
}

pub fn mk_binary(id: NodeId, span: Span, op: BinOp, a: Expr, b: Expr) -> Expr {
    Expr {
        id,
        span,
        kind: ExprKind::Binary(op, Box::new(a), Box::new(b)),
    }
}

/// Negation; numeric literals are folded so `-5000` is a plain `Int(-5000)`.
pub fn mk_neg(id: NodeId, span: Span, e: Expr) -> Expr {
    let kind = match e.kind {
        ExprKind::Int(i) => ExprKind::Int(-i),
        ExprKind::Float(f) => ExprKind::Float(-f),
        other => ExprKind::Unary(
            UnOp::Neg,
            Box::new(Expr {
                id: e.id,
                span: e.span,
                kind: other,
            }),
        ),
    };
    Expr { id, span, kind }
}

pub fn mk_not(id: NodeId, span: Span, e: Expr) -> Expr {
    Expr {
        id,
        span,
        kind: ExprKind::Unary(UnOp::Not, Box::new(e)),
    }
}
