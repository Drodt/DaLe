//! A minimal open-addressing hash table from hashes to `u32` indices. The keys live elsewhere
//! (in the pool's vectors); the caller supplies hashing and equality.

const EMPTY: u32 = u32::MAX;

#[derive(Clone)]
pub(crate) struct RawTable {
    slots: Vec<u32>,
    len: usize,
}

#[inline]
pub(crate) fn mix(h: u64, v: u64) -> u64 {
    (h.rotate_left(5) ^ v).wrapping_mul(0x517c_c1b7_2722_0a95)
}

impl RawTable {
    pub fn new() -> Self {
        Self {
            slots: vec![EMPTY; 16],
            len: 0,
        }
    }

    #[inline]
    fn start(&self, hash: u64) -> usize {
        ((hash ^ (hash >> 29)) as usize) & (self.slots.len() - 1)
    }

    /// `Ok(index)` if an entry with this hash satisfies `eq`; otherwise `Err(slot)`, the slot an
    /// insertion of this key has to use. The slot is only valid until the table is modified.
    #[inline]
    pub fn find(&self, hash: u64, mut eq: impl FnMut(u32) -> bool) -> Result<u32, usize> {
        let mask = self.slots.len() - 1;
        let mut i = self.start(hash);
        loop {
            let s = self.slots[i];
            if s == EMPTY {
                return Err(i);
            }
            if eq(s) {
                return Ok(s);
            }
            i = (i + 1) & mask;
        }
    }

    /// Inserts `idx` at a slot returned by [`find`](Self::find). `hash_of` must already know `idx`.
    pub fn insert_at(&mut self, slot: usize, idx: u32, hash_of: impl Fn(u32) -> u64) {
        debug_assert_eq!(self.slots[slot], EMPTY);
        self.slots[slot] = idx;
        self.len += 1;
        if self.len * 2 > self.slots.len() {
            self.grow(hash_of);
        }
    }

    fn grow(&mut self, hash_of: impl Fn(u32) -> u64) {
        let new_len = self.slots.len() * 2;
        let old = std::mem::replace(&mut self.slots, vec![EMPTY; new_len]);
        for idx in old.into_iter().filter(|&s| s != EMPTY) {
            self.place(idx, hash_of(idx));
        }
    }

    fn place(&mut self, idx: u32, hash: u64) {
        let mask = self.slots.len() - 1;
        let mut i = self.start(hash);
        while self.slots[i] != EMPTY {
            i = (i + 1) & mask;
        }
        self.slots[i] = idx;
    }

    /// Forgets all entries and re-inserts `0..count` (used when a pool is rolled back).
    pub fn reset_to(&mut self, count: u32, hash_of: impl Fn(u32) -> u64) {
        let cap = (count as usize * 4).next_power_of_two().max(16);
        self.slots = vec![EMPTY; cap];
        self.len = count as usize;
        for idx in 0..count {
            self.place(idx, hash_of(idx));
        }
    }
}
