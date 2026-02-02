use dale_util::{
    arena::DroplessArena,
    symbol::{Interner, Symbol},
};

#[derive(Clone, Copy)]
pub struct Ctx<'cx> {
    pub ir_arena: &'cx DroplessArena,
    pub interner: &'cx Interner<Symbol>,
}

impl<'cx> Ctx<'cx> {}
