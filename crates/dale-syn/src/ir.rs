use dale_macros::newtype_index;
use dale_util::symbol::Symbol;

pub mod lower;

newtype_index! {
  #[orderable]
  #[debug_format = "FileId({})"]
  pub struct FileId {}
}

newtype_index! {
  #[orderable]
  #[debug_format = "IrId({})"]
  pub struct IrId {}
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct DefId {
    pub index: DefIndex,
    pub file: FileId,
}

newtype_index! {
    #[orderable]
    #[debug_format = "DefIndex({})"]
    pub struct DefIndex {
        /// The file root is always assigned index 0 by the AST Map code
        const FILE_DEF_INDEX = 0;
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ItemId {
    pub id: IrId,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub span: Span,
    pub name: Symbol,
}

pub struct Map<'ir> {
    pub file: File<'ir>,
}

#[derive(Debug, Clone)]
pub struct File<'ir> {
    pub id: FileId,
    pub theories: &'ir [Theory<'ir>],
}

#[derive(Debug, Clone)]
pub struct Theory<'ir> {
    pub id: IrId,
    pub name: Ident,
    pub items: &'ir [ItemId],
}

#[derive(Debug, Clone)]
pub struct Path<'ir> {
    pub span: Span,
    pub segments: &'ir [PathSegment],
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    pub id: IrId,
    pub ident: Ident,
}

#[derive(Debug, Clone)]
pub struct Item<'ir> {
    pub id: IrId,
    pub kind: ItemKind<'ir>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ItemKind<'ir> {
    Use(UseTree<'ir>),
    Operators(&'ir [OperatorDecl<'ir>]),
    Sorts(&'ir [SortDecl<'ir>]),
    DataTypes(DataTypeDecls),
    Functions(&'ir [FunctionDecl<'ir>]),
    Predicates(&'ir [PredicateDecl<'ir>]),
    Axioms(),
    Rules(&'ir [Rule<'ir>]),
}

#[derive(Debug, Clone)]
pub struct UseTree<'ir> {
    pub prefix: &'ir Path<'ir>,
    pub kind: UseTreeKind<'ir>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum UseTreeKind<'ir> {
    Simple(Option<Ident>),
    Nested {
        items: &'ir [UseTree<'ir>],
        span: Span,
    },
    Glob,
}

#[derive(Debug, Clone)]
pub struct FunctionDecl<'ir> {
    pub id: IrId,
    pub modifiers: FunctionModifiers,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub where_to_bind: &'ir [u8],
    pub arg_sort_refs: &'ir [SortRef<'ir>],
    pub sort_ref: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionModifiers {
    pub rigid: bool,
    pub unique: bool,
    pub skolem: bool,
}

#[derive(Debug, Clone)]
pub struct OperatorDecl<'ir> {
    pub id: IrId,
    pub rigid: bool,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub where_to_bind: &'ir [u8],
    pub arg_sort_refs: &'ir [SortRef<'ir>],
    pub sort_ref: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone)]
pub struct SortDecl<'ir> {
    pub id: IrId,
    pub modifiers: SortModifiers,
    pub span: Span,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub extends: Option<(Span, &'ir [SortRef<'ir>])>,
}

#[derive(Debug, Clone, Copy)]
pub struct SortModifiers {
    pub meta: bool,
    pub top: bool,
}

#[derive(Debug, Clone)]
pub struct DataTypeDecls {}

#[derive(Debug, Clone)]
pub struct PredicateDecl<'ir> {
    pub id: IrId,
    pub rigid: bool,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub where_to_bind: &'ir [u8],
    pub arg_sort_refs: &'ir [SortRef<'ir>],
}

#[derive(Debug, Clone)]
pub struct SortRef<'ir> {
    pub id: IrId,
    pub span: Span,
    pub name: Ident,
    pub args: &'ir [GenericArg<'ir>],
}

#[derive(Debug, Clone)]
pub enum GenericArg<'ir> {
    Sort(&'ir SortRef<'ir>),
    Const(&'ir Term<'ir>, Span),
}

#[derive(Debug, Clone)]
pub struct GenericParam<'ir> {
    pub name: Ident,
    pub kind: GenericParamKind<'ir>,
    pub colon_span: Option<Span>,
}

#[derive(Debug, Clone)]
pub enum GenericParamKind<'ir> {
    Sort,
    Const { sort: &'ir SortRef<'ir>, span: Span },
}

#[derive(Debug, Clone)]
pub struct Term<'ir> {
    pub id: IrId,
    pub span: Span,
    pub kind: TermKind<'ir>,
}

#[derive(Debug, Clone)]
pub enum TermKind<'ir> {
    Path(Path<'ir>),
    Call(Path<'ir>, &'ir [Term<'ir>]),
}

#[derive(Debug, Clone)]
pub struct Rule<'ir> {
    pub id: IrId,
    pub name: Ident,
    pub schema_vars: &'ir [SchemaVarDecl<'ir>],
    pub assumes: Option<Spanned<TermOrSeq<'ir>>>,
    pub find: Option<Spanned<TermOrSeq<'ir>>>,
    pub goal_specs: GoalSpecs<'ir>,
    pub rule_sets: Option<Spanned<Vec<Ident>>>,
    pub display_name: Option<Spanned<String>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SchemaVarDecl<'ir> {
    pub id: IrId,
    pub span: Span,
    pub name: Ident,
    pub sort: &'ir SortRef<'ir>,
}

#[derive(Debug, Clone)]
pub enum TermOrSeq<'ir> {
    Term(&'ir Term<'ir>),
    Seq(&'ir Seq<'ir>),
}

#[derive(Debug, Clone)]
pub struct Seq<'ir> {
    pub id: IrId,
    pub ante: &'ir [Term<'ir>],
    pub succ: &'ir [Term<'ir>],
}

#[derive(Debug, Clone)]
pub enum GoalSpecs<'ir> {
    CloseGoal(IrId, Span),
    Specs(&'ir [GoalSpec<'ir>], Span),
}

#[derive(Debug, Clone)]
pub struct GoalSpec<'ir> {
    pub id: IrId,
    pub name: Option<(String, Span)>,
    pub replace_with: Option<TermOrSeq<'ir>>,
    pub add: Option<TermOrSeq<'ir>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Res<Id = IrId> {
    Def(DefKind, DefId),
    Local(Id),
    Err,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefKind {
    File,
    Theory,
    Use,
    Operator,
    Sort,
    DataType,
    Pred,
    Fn,
    Rule,
}
