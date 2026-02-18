use std::collections::HashMap;

use dale_util::arena::DroplessArena;
use smallvec::{SmallVec, smallvec};

use super::*;
use crate::{
    ctx::Ctx,
    raw_theory::{self as raw, NodeId},
    resolve::Resolver,
};

pub fn lower(_file: raw::File) -> Map<'static> {
    Map { file: todo!() }
}

struct LowerCtx<'ir> {
    cx: Ctx<'ir>,
    resolver: Resolver<'ir>,
    arena: &'ir DroplessArena,
    current_file_id: FileId,
    current_ir_id: IrId,
    node_id_to_ir_id: HashMap<NodeId, IrId>,
    items: HashMap<ItemId, &'ir Item<'ir>>,
}

impl<'ir> LowerCtx<'ir> {
    fn new(cx: Ctx<'ir>, resolver: Resolver<'ir>) -> Self {
        Self {
            cx,
            resolver,
            arena: cx.ir_arena,
            current_file_id: FileId::ZERO,
            current_ir_id: IrId::ZERO,
            node_id_to_ir_id: HashMap::default(),
            items: HashMap::default(),
        }
    }
}

impl<'ir> LowerCtx<'ir> {
    fn ir_id(&self, id: NodeId) -> IrId {
        *self.node_id_to_ir_id.get(&id).expect("Ir id")
    }

    fn lower_span(&self, span: &raw::Span) -> Span {
        Span {
            file: self.current_file_id,
            start: span.0,
            end: span.1,
        }
    }

    fn lower_ident(&self, ident: &raw::Ident) -> Ident {
        Ident {
            span: self.lower_span(&ident.span),
            name: ident.node,
        }
    }

    fn lower_node_id(&mut self, node_id: raw::NodeId) -> IrId {
        let id = self.current_ir_id;
        self.current_ir_id.increment_by(1);
        let old = self.node_id_to_ir_id.insert(node_id, id);
        assert!(old.is_none(), "Already created IR id for {:?}", node_id);
        id
    }

    fn lower_theory(&mut self, theory: raw::Theory) -> &'ir Theory<'ir> {
        let id = self.lower_node_id(theory.id);
        let name = self.lower_ident(&theory.name);
        let items = self
            .arena
            .alloc_from_iter(theory.items.iter().flat_map(|x| self.lower_item_refs(x)));
        self.arena.alloc(Theory { id, name, items })
    }

    fn lower_item_refs(&mut self, item: &raw::Item) -> SmallVec<[ItemId; 1]> {
        let mut ids = smallvec![ItemId {
            id: self.lower_node_id(item.id)
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
                    vec.push(ItemId {
                        id: self.lower_node_id(id),
                    });
                    self.lower_item_id_use_tree(nested, vec);
                }
            }
        }
    }

    fn lower_item(&mut self, item: &raw::Item) -> &'ir Item<'ir> {
        let span = self.lower_span(&item.span);
        let id = ItemId {
            id: self.ir_id(item.id),
        };
        let kind = self.lower_item_kind(&item.kind);
        let i = self.arena.alloc(Item { id, span, kind });
        let old = self.items.insert(id, i);
        assert!(old.is_none(), "Already have item for {id:?}");
        i
    }

    fn lower_item_kind(&mut self, kind: &raw::ItemKind) -> ItemKind<'ir> {
        match kind {
            raw::ItemKind::Use(use_tree) => todo!(),
            raw::ItemKind::Operators(function_decls) => todo!(),
            raw::ItemKind::Sorts(sort_decls) => todo!(),
            raw::ItemKind::DataTypes(data_type_decls) => todo!(),
            raw::ItemKind::Functions(function_decls) => todo!(),
            raw::ItemKind::Predicates(predicate_decls) => todo!(),
            raw::ItemKind::Axioms() => todo!(),
            raw::ItemKind::Rules(rules) => todo!(),
            raw::ItemKind::RuleSets(rule_set_decls) => todo!(),
        }
    }

    fn lower_use_tree(
        &mut self,
        tree: &raw::UseTree,
        prefix: &raw::Path,
        id: NodeId,
    ) -> ItemKind<'ir> {
        let path = &tree.prefix;
        let segments = prefix
            .segments
            .iter()
            .chain(path.segments.iter())
            .cloned()
            .collect();

        match tree.kind {
            raw::UseTreeKind::Simple(rename) => {
                let mut ident = tree.ident();

                // First, apply the prefix to the path.
                let mut path = raw::Path {
                    segments,
                    span: path.span,
                };

                let res = self.lower_import_res(id, path.span);
                let path = self.lower_use_path(res, &path);
                let ident = self.lower_ident(&ident);
                ItemKind::Use(path, UseKind::Single(ident))
            }
            raw::UseTreeKind::Nested {
                items: ref trees, ..
            } => todo!(),
            raw::UseTreeKind::Glob => todo!(),
        }
    }

    fn lower_import_res(&self, id: NodeId, span: raw::Span) -> PerNS<Option<Res>> {
        let per_ns = self.resolver.get_import_res(id);
        if per_ns.is_empty() {
            // Propagate the error to all namespaces, just to be sure.
            let err = Some(Res::Err);
            return PerNS {
                op_ns: err,
                theory_ns: err,
                rule_ns: err,
                rule_set_ns: err,
                sort_ns: err,
            };
        }
        per_ns
    }

    fn lower_use_path(&mut self, res: PerNS<Option<Res>>, path: &raw::Path) -> &'ir UsePath<'ir> {
        assert!(!res.is_empty());
        self.arena.alloc(UsePath {
            res,
            segments: self
                .arena
                .alloc_from_iter(path.segments.iter().map(|seg| self.lower_path_segment(seg))),
            span: self.lower_span(&path.span),
        })
    }

    fn lower_path(&mut self, path: raw::Path, id: NodeId) -> &'ir Path<'ir> {
        let span = self.lower_span(&path.span);
        let res = self.resolver.get_res(id).unwrap_or(Res::Err);
        let segments = self
            .arena
            .alloc_from_iter(path.segments.iter().map(|s| self.lower_path_segment(s)));
        self.arena.alloc(Path {
            span,
            segments,
            res,
        })
    }

    fn lower_path_segment(&mut self, s: &raw::PathSegment) -> PathSegment {
        let id = self.lower_node_id(s.id);
        let ident = self.lower_ident(&s.ident);
        let span = self.lower_span(&s.span);
        PathSegment { id, ident, span }
    }
}
