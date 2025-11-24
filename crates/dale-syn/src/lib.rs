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
        \use 
        \sorts {}
      }
      "#,
            )
            .unwrap();
    }
}
