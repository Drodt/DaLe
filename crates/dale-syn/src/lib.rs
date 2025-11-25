use lalrpop_util::lalrpop_mod;

pub mod theory;

lalrpop_mod!(dl_theory);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let theory = dl_theory::TheoryParser::new()
            .parse(
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
        assert_eq!("Seq", &theory.name);
        assert_eq!(3, theory.items.len());
    }

    #[test]
    fn rustydl() {
        use std::fs::read_to_string;

        let input = read_to_string("examples/RustyDL.theokey").unwrap();

        let rusty = dl_theory::FileParser::new().parse(&input).unwrap();

        assert_eq!(6, rusty.theories.len())
    }
}
