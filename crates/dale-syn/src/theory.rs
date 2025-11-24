use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct File {
    pub theories: Vec<Theory>,
}

#[derive(Debug, Clone)]
pub struct Theory {
    pub name: String,
    pub items: Vec<Item>,
}

pub type Span = Range<usize>;

#[derive(Debug, Clone)]
pub struct Path {
    pub span: Span,
    pub segments: Vec<String>,
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
    Simple(Option<String>),
    Nested { items: Vec<UseTree>, span: Span },
    Glob,
}

pub type OperatorDecls = Spanned<Vec<FunctionDecl>>;

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub rigid: bool,
    pub name: String,
    pub params: Vec<String>,
    pub where_to_bind: Vec<u8>,
    pub arg_sorts: Vec<Sort>,
    pub sort: Sort,
}

pub type SortDecls = Spanned<Vec<SortDecl>>;

#[derive(Debug, Clone)]
pub struct SortDecl {
    pub modifiers: SortModifiers,
    pub span: Span,
    pub name: String,
    pub params: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct SortModifiers {
    pub meta: bool,
    pub top: bool,
}

#[derive(Debug, Clone)]
pub struct DataTypeDecls {}

#[derive(Debug, Clone)]
pub struct FunctionDecls {}

#[derive(Debug, Clone)]
pub struct PredicateDecls {}

#[derive(Debug, Clone)]
pub struct Sort {
    pub span: Span,
    pub kind: SortKind,
}

#[derive(Debug, Clone)]
pub enum SortKind {
    Simple(String),
    Parametric(String, Vec<Sort>),
}
