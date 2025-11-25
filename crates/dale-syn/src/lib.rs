use dale_util::symbol::{Interner, Symbol};
use lalrpop_util::lalrpop_mod;

pub mod theory;

pub struct ParserCtx {
    interner: Interner<Symbol>,
}

impl ParserCtx {
    pub fn new() -> Self {
        Self {
            interner: Default::default(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    pub fn get_symbol(&self, sym: Symbol) -> &str {
        self.interner.get(sym)
    }
}

lalrpop_mod!(dl_theory);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let mut ctx = ParserCtx::new();
        let theory = dl_theory::TheoryParser::new()
            .parse(
                &mut ctx,
                r#"
      \theory Seq {
        \use int;
        \use fol::{quant::a, rel::*};
        \sorts {}
      }
      "#,
            )
            .unwrap();
        println!("{theory:?}");
        assert_eq!("Seq", ctx.get_symbol(theory.name.node));
        assert_eq!(3, theory.items.len());
    }

    #[test]
    fn rustydl() {
        use std::fs::read_to_string;

        let mut ctx = ParserCtx::new();
        let input = read_to_string("examples/RustyDL.theokey").unwrap();

        let rusty = dl_theory::FileParser::new()
            .parse(&mut ctx, &input)
            .unwrap();

        assert_eq!(6, rusty.theories.len())
    }
}
