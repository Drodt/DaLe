use lalrpop_util::{ParseError, lalrpop_mod};

use dale_index::Idx;
use dale_util::symbol::{Interner, Symbol};

use crate::{dl_theory::Token, raw_theory::NodeId};

pub mod check;
mod ctx;
pub mod ir;
pub mod raw_theory;
mod resolve;

pub struct ParserCtx {
    next_id: NodeId,
    interner: Interner<Symbol>,
}

impl Default for ParserCtx {
    fn default() -> Self {
        Self::new()
    }
}

impl ParserCtx {
    pub fn new() -> Self {
        Self {
            next_id: NodeId::ZERO,
            interner: Default::default(),
        }
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    pub fn get_symbol(&self, sym: Symbol) -> &str {
        self.interner.get(sym)
    }

    pub fn id(&mut self) -> NodeId {
        let id = self.next_id;
        self.next_id.increment_by(1);
        id
    }
}

lalrpop_mod!(dl_theory);

fn get_line(input: &str, loc: usize) -> &str {
    let mut start = loc;
    while start > 0 {
        if &input[start - 1..start] == "\n" {
            break;
        }
        start -= 1;
    }
    let mut end = loc + 1;
    while end < input.len() - 1 {
        if &input[end..end + 1] == "\n" {
            break;
        }
        end += 1;
    }

    &input[start..end]
}

pub struct Pos {
    pub line: usize,
    pub column: usize,
}

pub fn loc_to_pos(input: &str, loc: usize) -> Pos {
    let mut line = 1;
    let mut column = 1;
    for c in input.chars().take(loc) {
        match c {
            '\n' => {
                line += 1;
                column = 1;
            }
            _ => {
                column += 1;
            }
        }
    }
    Pos { line, column }
}

pub fn err_report(input: &str, e: ParseError<usize, Token<'_>, &str>) -> String {
    match e {
        ParseError::InvalidToken { location } => {
            let pos = loc_to_pos(input, location);
            let line = get_line(input, location);
            format!(
                "Error at line {}, column {}: Invalid token\n\t{}",
                pos.line, pos.column, line
            )
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let pos = loc_to_pos(input, location);
            let line = get_line(input, location);
            let one_of = expected.join(", ");
            format!(
                "Unexpected end of file at line {}, column {}; expected one of {}\n{}",
                pos.line, pos.column, one_of, line
            )
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let pos = loc_to_pos(input, token.0);
            let line = get_line(input, token.0);
            let one_of = expected.join(", ");
            format!(
                "Unexpected token {} at line {}, column {}; expected one of {}\n{}",
                token.1, pos.line, pos.column, one_of, line
            )
        }
        ParseError::ExtraToken { token } => {
            let pos = loc_to_pos(input, token.0);
            let line = get_line(input, token.0);
            format!(
                "Unexpected token at line {}, column {}; expected end of file\n{}",
                pos.line, pos.column, line
            )
        }
        ParseError::User { error } => error.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use dale_util::arena::DroplessArena;

    use crate::{ctx::Ctx, resolve::resolve};

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

        let rusty = match dl_theory::FileParser::new().parse(&mut ctx, &input) {
            Ok(r) => r,
            Err(e) => panic!("{}", err_report(&input, e)),
        };

        assert_eq!(6, rusty.theories.len());

        let ir_arena = DroplessArena::default();
        let cx = Ctx {
            ir_arena: &ir_arena,
            interner: &ctx.interner,
        };
        let resolver = resolve(cx, &rusty);
        if !resolver.errors.is_empty() {
            let e = &resolver.errors[0];
            let start = e.span().0;
            let pos = loc_to_pos(&input, start);
            panic!(
                "{}:{} '{}': {}",
                pos.line,
                pos.column,
                get_line(&input, start),
                e.format(cx)
            )
        }
        let ir = ir::lower::lower(rusty, cx, resolver);
        check::check_ir(ir);
    }
}
