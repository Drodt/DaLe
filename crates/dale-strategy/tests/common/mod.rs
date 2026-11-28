//! Shared by the tests: loading the propositional part of `RustyDL.theokey` with a few atoms,
//! and random propositional formulas with their truth tables.
#![allow(dead_code)]

use std::path::PathBuf;

use dale_logic::Sequent;
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

// ---------------------------------------------------------------- random formulas

/// xorshift64*.
pub struct Rng(pub u64);

impl Rng {
    pub fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545_F491_4F6C_DD1D)
    }
    pub fn below(&mut self, n: u64) -> u64 {
        (self.next() >> 32) % n
    }
}

#[derive(Clone, Debug)]
pub enum F {
    Atom(usize),
    True,
    False,
    Not(Box<F>),
    Bin(&'static str, Box<F>, Box<F>),
}

pub fn random_formula(r: &mut Rng, depth: u32) -> F {
    if depth == 0 {
        return match r.below(10) {
            0 => F::True,
            1 => F::False,
            _ => F::Atom(r.below(ATOMS as u64) as usize),
        };
    }
    match r.below(12) {
        0 => random_formula(r, 0),
        1 | 2 => F::Not(Box::new(random_formula(r, depth - 1))),
        k => {
            let op = ["and", "or", "imp", "equiv"][(k as usize - 3) % 4];
            F::Bin(
                op,
                Box::new(random_formula(r, depth - 1)),
                Box::new(random_formula(r, depth - 1)),
            )
        }
    }
}

impl F {
    /// In the syntax of problem terms.
    pub fn text(&self) -> String {
        match self {
            F::Atom(i) => format!("p{i}"),
            F::True => "true".into(),
            F::False => "false".into(),
            F::Not(a) => format!("not({})", a.text()),
            F::Bin(op, a, b) => format!("{op}({}, {})", a.text(), b.text()),
        }
    }

    pub fn eval(&self, v: u32) -> bool {
        match self {
            F::Atom(i) => v >> i & 1 == 1,
            F::True => true,
            F::False => false,
            F::Not(a) => !a.eval(v),
            F::Bin(op, a, b) => {
                let (a, b) = (a.eval(v), b.eval(v));
                match *op {
                    "and" => a && b,
                    "or" => a || b,
                    "imp" => !a || b,
                    _ => a == b,
                }
            }
        }
    }

    pub fn is_tautology(&self) -> bool {
        (0..1u32 << ATOMS).all(|v| self.eval(v))
    }
}
