use dale_util::arena::DroplessArena;
use smallvec::{SmallVec, smallvec};

use super::*;
use crate::{
    ctx::Ctx,
    raw_theory::{self as raw, NodeId},
};

pub fn lower(file: raw::File) -> Map<'static> {
    Map { file: todo!() }
}

struct LowerCtx<'ir> {
    cx: Ctx<'ir>,
    arena: &'ir DroplessArena,
    current_file_id: FileId,
}

impl<'ir> LowerCtx<'ir> {
    fn new(cx: Ctx<'ir>) -> Self {
        Self {
            cx,
            arena: cx.ir_arena,
            current_file_id: FileId::ZERO,
        }
    }
}

impl<'ir> LowerCtx<'ir> {
    fn ir_id(&mut self, id: NodeId) -> IrId {
        todo!()
    }

    fn lower_span(&self, span: &raw::Span) -> Span {
        Span {
            file: self.current_file_id,
            start: span.start,
            end: span.end,
        }
    }

    fn lower_ident(&self, ident: &raw::Ident) -> Ident {
        Ident {
            span: self.lower_span(&ident.span),
            name: ident.node,
        }
    }

    fn lower_node_id(&mut self, node_id: raw::NodeId) -> IrId {
        todo!()
    }

    fn lower_theory(&mut self, theory: raw::Theory) -> &'ir Theory<'ir> {
        let id = self.lower_node_id(theory.id);
        let name = self.lower_ident(&theory.name);
        let items = self
            .arena
            .alloc_from_iter(theory.items.iter().flat_map(|x| self.lower_item(x)));
        self.arena.alloc(Theory { id, name, items })
    }

    fn lower_item(&mut self, item: &raw::Item) -> SmallVec<[ItemId; 1]> {
        let mut ids = smallvec![ItemId {
            id: self.ir_id(item.id)
        }];
        if let raw::ItemKind::Use(use_tree) = &item.kind {
            self.lower_item_id_use_tree(use_tree, &mut ids);
        }
        ids
    }

    fn lower_item_id_use_tree(&mut self, tree: &raw::UseTree, vec: &mut SmallVec<[ItemId; 1]>) {
        match &tree.kind {
            raw::UseTreeKind::Simple(_) | raw::UseTreeKind::Glob => {}
            raw::UseTreeKind::Nested { items, .. } => {
                for &(ref nested, id) in items {
                    vec.push(ItemId { id: self.ir_id(id) });
                    self.lower_item_id_use_tree(nested, vec);
                }
            }
        }
    }

    fn lower_path(&mut self, path: raw::Path) -> &'ir Path<'ir> {
        let span = self.lower_span(&path.span);
        let segments = self
            .arena
            .alloc_from_iter(path.segments.iter().map(|s| self.lower_segment(s)));
        self.arena.alloc(Path { span, segments })
    }

    fn lower_segment(&mut self, s: &raw::PathSegment) -> PathSegment {
        let id = self.lower_node_id(s.id);
        let ident = self.lower_ident(&s.ident);
        PathSegment { id, ident }
    }
}
