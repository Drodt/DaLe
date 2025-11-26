use std::ops::Range;

use dale_macros::newtype_index;
use dale_util::symbol::Symbol;

newtype_index! {
    /// Identifies an AST node.
    #[orderable]
    #[debug_format = "NodeId({})"]
    pub struct NodeId {}
}

#[derive(Debug, Clone)]
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
    pub theories: Vec<Theory>,
}

#[derive(Debug, Clone)]
pub struct Theory {
    pub id: NodeId,
    pub name: Ident,
    pub items: Vec<Item>,
}

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<Ident>,
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
}

#[derive(Debug, Clone)]
pub struct UseTree {
    pub prefix: Path,
    pub kind: UseTreeKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UseTreeKind {
    Simple(Option<Ident>),
    Nested { items: Vec<UseTree>, span: Span },
    Glob,
}

pub type OperatorDecls = Vec<FunctionDecl>;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub id: NodeId,
    pub modifiers: FunctionModifiers,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    pub where_to_bind: Vec<u8>,
    pub arg_sorts: Vec<Sort>,
    pub sort: Sort,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionModifiers {
    pub rigid: bool,
    pub unique: bool,
    pub skolem: bool,
}

pub type SortDecls = Vec<SortDecl>;

#[derive(Debug, Clone)]
pub struct SortDecl {
    pub id: NodeId,
    pub modifiers: SortModifiers,
    pub span: Span,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    pub extends: Option<(Span, Vec<Sort>)>,
}

#[derive(Debug, Clone, Copy)]
pub struct SortModifiers {
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
    pub where_to_bind: Vec<u8>,
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
    Simple(Ident),
    Parametric(Ident, Vec<GenericArg>),
}

#[derive(Debug, Clone)]
pub enum GenericArg {
    Sort(Sort),
    Const(Term, Span),
}

#[derive(Debug, Clone)]
pub struct GenericParam {
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
    Call(Path, Vec<Term>),
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
    pub rule_sets: Option<Spanned<Vec<Ident>>>,
    pub display_name: Option<Spanned<String>>,
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
    pub name: Option<(String, Span)>,
    pub replace_with: Option<TermOrSeq>,
    pub add: Option<TermOrSeq>,
}
