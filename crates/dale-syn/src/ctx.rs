use dale_util::{
    arena::DroplessArena,
    symbol::{Interner, Symbol},
};

use crate::{
    ir::{FileId, LOCAL_FILE, Span},
    raw_theory,
};

#[derive(Clone, Copy)]
pub struct Ctx<'cx> {
    pub ir_arena: &'cx DroplessArena,
    pub interner: &'cx Interner<Symbol>,
    pub files: &'cx FileTable,
}

/// Maps the global offsets of parser spans back to a file and an offset inside of it.
///
/// Every file occupies a range of the global offset space that starts at its `base`. An empty
/// table stands for a single file at base 0.
#[derive(Debug, Clone, Default)]
pub struct FileTable {
    /// `(base, file)`, sorted by `base`.
    entries: Vec<(usize, FileId)>,
}

impl FileTable {
    pub fn new(entries: Vec<(usize, FileId)>) -> Self {
        debug_assert!(entries.windows(2).all(|w| w[0].0 < w[1].0));
        Self { entries }
    }

    pub fn span(&self, raw: raw_theory::Span) -> Span {
        let (base, file) = match self
            .entries
            .partition_point(|(base, _)| *base <= raw.0)
            .checked_sub(1)
        {
            Some(i) => self.entries[i],
            None => (0, LOCAL_FILE),
        };
        Span {
            file,
            start: raw.0 - base,
            end: raw.1.saturating_sub(base).max(raw.0 - base),
        }
    }
}

impl<'cx> Ctx<'cx> {}
