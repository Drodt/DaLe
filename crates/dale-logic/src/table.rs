//! The intern tables of the pool: maps from a key's hash to the `u32` index of the entry with that
//! key. The keys live elsewhere (in the pool's vectors), so the tables only store 4 bytes per
//! entry and the caller supplies hashing and equality.
//!
//! This is a thin wrapper around `hashbrown::HashTable`, which probes groups of control bytes with
//! SIMD and compares a 7-bit tag of the hash first, so most non-matching entries are rejected
//! without touching the entry's data.

use std::sync::Arc;

use hashbrown::HashTable;

/// Combines the running hash `h` with the next value `v`: rotate, xor, multiply (the "Fx" scheme
/// of rustc's hasher, with a 64-bit odd constant derived from the golden ratio).
///
/// It is order-sensitive (`mix(mix(h, a), b) != mix(mix(h, b), a)` in general), which tuples and
/// argument lists need, and costs three cheap instructions per value. Its weakness is that the
/// *low* bits of the result depend only on the low bits of the input; [`InternTable`] therefore
/// rotates the hash before use (see [`scramble`]). It is not a cryptographic or DoS-resistant
/// hash.
#[inline]
pub(crate) fn mix(h: u64, v: u64) -> u64 {
    (h.rotate_left(5) ^ v).wrapping_mul(0x517c_c1b7_2722_0a95)
}

/// `hashbrown` takes the bucket from the low bits of a hash and a 7-bit tag from the top bits.
/// The multiplication in [`mix`] leaves the high bits well mixed and the low bits poorly mixed, so
/// the hash is rotated to move good bits down (rustc's `FxHasher` does the same in `finish`).
#[inline]
fn scramble(hash: u64) -> u64 {
    hash.rotate_left(26)
}

#[derive(Default)]
pub(crate) struct InternTable(HashTable<u32>);

impl InternTable {
    pub fn new() -> Self {
        Self(HashTable::new())
    }

    /// The index of an entry with this hash that satisfies `eq`.
    #[inline]
    pub fn find(&self, hash: u64, mut eq: impl FnMut(u32) -> bool) -> Option<u32> {
        self.0.find(scramble(hash), |&i| eq(i)).copied()
    }

    /// Adds `idx`, whose key must not be in the table yet. `hash_of` computes the hash of any
    /// entry, including `idx`; it is used when the table grows.
    pub fn insert(&mut self, hash: u64, idx: u32, hash_of: impl Fn(u32) -> u64) {
        self.0
            .insert_unique(scramble(hash), idx, |&i| scramble(hash_of(i)));
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn with_capacity(n: usize) -> Self {
        Self(HashTable::with_capacity(n))
    }

    pub fn iter(&self) -> impl Iterator<Item = u32> + '_ {
        self.0.iter().copied()
    }

    /// Forgets all entries with index `>= count` (used when a pool is rolled back).
    pub fn truncate(&mut self, count: u32) {
        self.0.retain(|i| *i < count);
    }
}

/// An intern table that can be forked: a stack of frozen tables shared between the pools forked
/// from each other, plus a private table for new entries. The key of an index is the same in all
/// layers (it is looked up in the pool's arenas), and a key is in at most one layer.
///
/// Lookups check the private table, then the frozen ones from newest to oldest; a miss costs one
/// probe per layer. [`seal`](Self::seal) keeps the number of layers logarithmic by merging
/// neighbouring layers of similar size (only indices are copied, nothing is renumbered).
#[derive(Default)]
pub(crate) struct Interner {
    frozen: Vec<Arc<InternTable>>,
    local: InternTable,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn find(&self, hash: u64, mut eq: impl FnMut(u32) -> bool) -> Option<u32> {
        if let Some(i) = self.local.find(hash, &mut eq) {
            return Some(i);
        }
        self.frozen.iter().rev().find_map(|t| t.find(hash, &mut eq))
    }

    pub fn insert(&mut self, hash: u64, idx: u32, hash_of: impl Fn(u32) -> u64) {
        self.local.insert(hash, idx, hash_of);
    }

    /// Forgets the private entries with index `>= count`. The frozen layers are not touched, so
    /// `count` must be at least the smallest index of the private part.
    pub fn truncate(&mut self, count: u32) {
        self.local.truncate(count);
    }

    /// The number of frozen layers (for tests and statistics).
    pub fn layers(&self) -> usize {
        self.frozen.len()
    }

    /// Freezes the private table and merges layers of similar size.
    pub fn seal(&mut self, hash_of: impl Fn(u32) -> u64) {
        if self.local.len() > 0 {
            self.frozen.push(Arc::new(std::mem::take(&mut self.local)));
        }
        while self.frozen.len() >= 2 {
            let n = self.frozen.len();
            let (a, b) = (&self.frozen[n - 2], &self.frozen[n - 1]);
            if b.len() * 2 < a.len() {
                break;
            }
            let mut m = InternTable::with_capacity(a.len() + b.len());
            for t in [a, b] {
                for i in t.iter() {
                    m.insert(hash_of(i), i, &hash_of);
                }
            }
            self.frozen.truncate(n - 2);
            self.frozen.push(Arc::new(m));
        }
    }

    /// Same frozen layers, empty private table. Call after [`seal`](Self::seal).
    pub fn shallow_clone(&self) -> Self {
        debug_assert_eq!(self.local.len(), 0);
        Self {
            frozen: self.frozen.clone(),
            local: InternTable::new(),
        }
    }
}
