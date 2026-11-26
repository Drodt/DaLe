use crate::ids::SortIdx;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LogicError {
    /// Wrong number of generic arguments.
    GenericArity {
        expected: usize,
        found: usize,
    },
    /// The i-th generic argument is a sort where a constant is required, or vice versa.
    GenericKind(usize),
    /// The i-th generic argument is a constant term with free bound variables.
    ConstNotClosed(usize),
    /// The i-th generic argument is a constant of the wrong sort.
    ConstSort {
        index: usize,
        expected: SortIdx,
        found: SortIdx,
    },
    MetaArity {
        expected: usize,
        found: usize,
    },
    MetaKind(usize),
    ArgCount {
        expected: usize,
        found: usize,
    },
    /// The sort of the argument is not a subsort of the expected sort.
    ArgSort {
        index: usize,
        expected: SortIdx,
        found: SortIdx,
    },
    /// A variable bound by a binder has a different sort than the binder declares.
    BindSort {
        expected: SortIdx,
        found: SortIdx,
    },
    /// The de Bruijn index does not fit into 32 bits.
    VarIndexTooLarge,
    /// The i-th generic parameter does not occur in the arguments' sorts.
    CannotInfer(usize),
    /// The argument sorts cannot be unified with the parameter sorts of the operator.
    InferMismatch(usize),
    /// The signature declares no bound variable operator.
    NoBoundVarOp,
    /// `instantiate` was called on a term whose operator binds no variable.
    NotABinder,
}

impl std::fmt::Display for LogicError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl std::error::Error for LogicError {}
