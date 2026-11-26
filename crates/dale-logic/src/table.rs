//! The intern tables of the pool: maps from a key's hash to the `u32` index of the entry with that
//! key. The keys live elsewhere (in the pool's vectors), so the tables only store 4 bytes per
//! entry and the caller supplies hashing and equality.
//!
//! This is a thin wrapper around `hashbrown::HashTable`, which probes groups of control bytes with
//! SIMD and compares a 7-bit tag of the hash first, so most non-matching entries are rejected
//! without touching the entry's data.

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

    /// Forgets all entries with index `>= count` (used when a pool is rolled back).
    pub fn truncate(&mut self, count: u32) {
        self.0.retain(|i| *i < count);
    }
}
