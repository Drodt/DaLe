use dale_util::arena::DroplessArena;

use super::*;
use crate::raw_theory as raw;

pub fn lower(file: raw::File) -> Map<'static> {
    Map { file: todo!() }
}

struct LowerCtx {
    arena: DroplessArena,
    current_file_id: FileId,
}

impl LowerCtx {
    fn new() -> Self {
        Self {
            arena: DroplessArena::default(),
            current_file_id: FileId::ZERO,
        }
    }
}

impl<'ir> LowerCtx {
    fn lower_span(&self, span: raw::Span) -> Span {
        Span {
            file: self.current_file_id,
            start: span.start,
            end: span.end,
        }
    }

    fn lower_ident(&self, ident: raw::Ident) -> Ident {
        Ident {
            span: self.lower_span(ident.span),
            name: ident.node,
        }
    }

    fn lower_node_id(&mut self, node_id: raw::NodeId) -> IrId {
        todo!()
    }

    fn lower_theory(&'ir mut self, theory: raw::Theory) -> &'ir Theory<'ir> {
        let id = self.lower_node_id(theory.id);
        let name = self.lower_ident(theory.name);
        let items = self.lower_items(&theory.items);
        self.arena.alloc(Theory { id, name, items })
    }

    fn lower_items(&mut self, items: &[raw::Item]) -> &'ir [ItemId] {
        todo!()
    }
}
