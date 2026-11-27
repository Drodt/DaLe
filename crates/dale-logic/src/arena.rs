//! Append-only storage whose older part can be shared between pools.
//!
//! An [`Arena`] is a frozen prefix of fixed-size chunks (`Arc<[T]>`, shared by all pools forked
//! from each other, never modified) followed by a private, growable tail. Reading an entry is
//! `chunks[id >> SHIFT][id & MASK]` for the prefix and `tail[id - base]` for the tail: two loads
//! and no search, like a `Vec` of chunks.
//!
//! [`seal`](Arena::seal) moves the tail into the shared prefix; [`shallow_clone`](Arena::shallow_clone)
//! then gives a second arena with the same prefix and an empty tail. Both then append independently: ids they hand out from there
//! on overlap (the same id denotes different entries in the two arenas), which is fine as long as
//! nobody mixes ids of the two. The cost of a fork is proportional to the tail, not to the arena.
//!
//! The tail starts at the next chunk boundary, so a fork leaves a gap in the id space (less than
//! one chunk per fork). Gaps are never read.

use std::sync::Arc;

pub(crate) const SHIFT: u32 = 12;
pub(crate) const CHUNK: usize = 1 << SHIFT;
const MASK: u32 = (CHUNK as u32) - 1;

#[derive(Debug)]
pub(crate) struct Arena<T> {
    chunks: Vec<Arc<[T]>>,
    /// The id of the first tail entry; a multiple of `CHUNK`, equal to `chunks.len() * CHUNK`.
    base: u32,
    tail: Vec<T>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self {
            chunks: Vec::new(),
            base: 0,
            tail: Vec::new(),
        }
    }
}

impl<T> Arena<T> {
    /// The id the next entry gets.
    #[inline]
    pub fn next(&self) -> u32 {
        self.base + self.tail.len() as u32
    }

    /// The first id of the private tail. Ids below it belong to the shared prefix.
    #[inline]
    pub fn base(&self) -> u32 {
        self.base
    }

    /// The number of entries (not counting the gaps left by forks).
    pub fn len(&self) -> usize {
        self.chunks.iter().map(|c| c.len()).sum::<usize>() + self.tail.len()
    }

    #[inline]
    pub fn push(&mut self, v: T) -> u32 {
        let id = self.next();
        self.tail.push(v);
        id
    }

    #[inline]
    pub fn get(&self, id: u32) -> &T {
        if id < self.base {
            &self.chunks[(id >> SHIFT) as usize][(id & MASK) as usize]
        } else {
            &self.tail[(id - self.base) as usize]
        }
    }

    /// Drops all entries with id `>= to`. `to` must not be below [`base`](Self::base).
    pub fn truncate(&mut self, to: u32) {
        assert!(to >= self.base, "truncating the shared part of an arena");
        self.tail.truncate((to - self.base) as usize);
    }

    /// Moves the tail into the shared prefix.
    pub fn seal(&mut self) {
        if self.tail.is_empty() {
            return;
        }
        let mut it = std::mem::take(&mut self.tail).into_iter();
        loop {
            let chunk: Arc<[T]> = it.by_ref().take(CHUNK).collect();
            if chunk.is_empty() {
                break;
            }
            self.chunks.push(chunk);
        }
        self.base = (self.chunks.len() as u32) << SHIFT;
    }

    /// The same prefix, an empty tail. Requires an empty tail, i.e. call after [`seal`](Self::seal).
    pub fn shallow_clone(&self) -> Arena<T> {
        debug_assert!(self.tail.is_empty());
        Arena {
            chunks: self.chunks.clone(),
            base: self.base,
            tail: Vec::new(),
        }
    }
}

impl<T: Copy> Arena<T> {
    /// Appends `s` so that it is contiguous *within one chunk*, padding to the next chunk boundary
    /// if it would straddle one. Returns the id of the first element. `s` must not be longer than
    /// a chunk.
    pub fn push_slice(&mut self, s: &[T]) -> u32 {
        assert!(s.len() <= CHUNK, "slice longer than an arena chunk");
        let off = (self.next() & MASK) as usize;
        if off + s.len() > CHUNK {
            let pad = CHUNK - off;
            self.tail.extend(std::iter::repeat_n(s[0], pad));
        }
        let start = self.next();
        self.tail.extend_from_slice(s);
        start
    }

    /// The slice of `len` entries starting at `start`, as returned by [`push_slice`](Self::push_slice).
    #[inline]
    pub fn slice(&self, start: u32, len: usize) -> &[T] {
        if len == 0 {
            return &[];
        }
        if start < self.base {
            let o = (start & MASK) as usize;
            &self.chunks[(start >> SHIFT) as usize][o..o + len]
        } else {
            let o = (start - self.base) as usize;
            &self.tail[o..o + len]
        }
    }
}
