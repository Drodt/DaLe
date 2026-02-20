use std::collections::HashMap;

use dale_index::Idx;
use dale_macros::newtype_index;
use dale_util::symbol::Symbol;

use crate::{check::Sort, resolve::PerNS};

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
}

#[derive(Debug, Clone, Copy)]
pub enum Def<'ir> {
    SortDecl(SortDecl<'ir>),
    OpDecl(OperatorDecl<'ir>),
    FnDecl(FunctionDecl<'ir>),
    PredDecl(PredicateDecl<'ir>),
    SchemaVar(SchemaVarDecl<'ir>),
    GenParam(GenericParam<'ir>),
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
    pub theories: &'ir [Theory<'ir>],
}

#[derive(Debug, Clone, Copy)]
pub struct Theory<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub items: &'ir [ItemId],
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
    RuleSets(&'ir [RuleSetDecl]),
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

#[derive(Debug, Clone, Copy)]
pub struct OperatorDecl<'ir> {
    pub id: DefId,
    pub rigid: bool,
    pub name: Ident,
    pub params: &'ir [GenericParam<'ir>],
    pub where_to_bind: &'ir [u8],
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
    pub where_to_bind: &'ir [u8],
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
}

#[derive(Debug, Clone, Copy)]
pub struct Rule<'ir> {
    pub id: DefId,
    pub name: Ident,
    pub schema_vars: &'ir [SchemaVarDecl<'ir>],
    pub assumes: Option<(Span, TermOrSeq<'ir>)>,
    pub find: Option<(Span, TermOrSeq<'ir>)>,
    pub goal_specs: GoalSpecs<'ir>,
    pub rule_sets: Option<(Span, &'ir [Path<'ir>])>,
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

#[derive(Debug, Clone, Copy)]
pub struct RuleSetDecl {
    pub id: DefId,
    pub name: Ident,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Res {
    Def(DefKind, DefId),
    Err,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    SortParam,
    ConstParam,
    SchemaVar,
    RuleSet,
}
