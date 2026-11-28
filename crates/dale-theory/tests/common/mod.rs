//! Shared by the tests: loading the propositional part of `RustyDL.theokey` with a few atoms.
//! The proof search tests are in `dale-strategy`.
#![allow(dead_code)]

use std::path::PathBuf;

use dale_logic::*;
use dale_theory::Theory;

pub const ATOMS: usize = 4;

/// `RustyDL.theokey`, a theory `Atoms` with the predicates `p0`..`p3`, and the problem
/// `\\problem { .. <seq> }` that uses both.
pub fn prop_problem(seq: &str) -> Theory {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../dale-syn/examples");
    let atoms: Vec<String> = (0..ATOMS).map(|i| format!("p{i};")).collect();
    let text = format!(
        "\\module RustyDL;\n\\theory Atoms {{\n  \\use RustyDL::Prop::*;\n  \\predicates {{ {} }}\n}}\n\\problem {{\n  \\use RustyDL::Prop::*;\n  \\use Atoms::*;\n  {seq}\n}}\n",
        atoms.join(" ")
    );
    Theory::load_text(dir.join("PropTest.theokey"), text).unwrap_or_else(|e| panic!("{e}"))
}

/// [`prop_problem`] and its sequent.
pub fn prop_goal(seq: &str) -> (Theory, Sequent) {
    let th = prop_problem(seq);
    let s = th.problem.clone().expect("a problem");
    (th, s)
}
