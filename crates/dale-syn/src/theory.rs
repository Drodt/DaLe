use std::ops::Range;

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

#[derive(Debug, Clone)]
pub struct SortDecls {}

#[derive(Debug, Clone)]
pub struct DataTypeDecls {}

#[derive(Debug, Clone)]
pub struct FunctionDecls {}

#[derive(Debug, Clone)]
pub struct PredicateDecls {}
