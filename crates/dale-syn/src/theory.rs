use std::ops::Range;

use dale_util::symbol::Symbol;

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

type Ident = Spanned<Symbol>;

#[derive(Debug, Clone)]
pub struct File {
    pub theories: Vec<Theory>,
}

#[derive(Debug, Clone)]
pub struct Theory {
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
pub enum Item {
    Use(UseTree),
    Operators(OperatorDecls),
    Sorts(SortDecls),
    DataTypes(DataTypeDecls),
    Functions(FunctionDecls),
    Predicates(PredicateDecls),
    Axioms(),
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

pub type OperatorDecls = Spanned<Vec<FunctionDecl>>;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
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

pub type SortDecls = Spanned<Vec<SortDecl>>;

#[derive(Debug, Clone)]
pub struct SortDecl {
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

pub type FunctionDecls = Spanned<Vec<FunctionDecl>>;

pub type PredicateDecls = Spanned<Vec<PredicateDecl>>;

#[derive(Debug, Clone)]
pub struct PredicateDecl {
    pub rigid: bool,
    pub name: Ident,
    pub params: Vec<GenericParam>,
    pub where_to_bind: Vec<u8>,
    pub arg_sorts: Vec<Sort>,
}

#[derive(Debug, Clone)]
pub struct Sort {
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
    pub span: Span,
    pub kind: TermKind,
}

#[derive(Debug, Clone)]
pub enum TermKind {
    Path(Path),
}
