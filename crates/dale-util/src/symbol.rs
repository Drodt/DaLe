use std::collections::HashMap;

use crate::arena::DroplessArena;

#[derive(Default, Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Eq, Hash)]
pub struct Symbol(pub usize);

impl From<usize> for Symbol {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<Symbol> for usize {
    fn from(val: Symbol) -> Self {
        val.0
    }
}

#[derive(Default)]
pub struct Interner<T: From<usize> + Copy> {
    pub arena: DroplessArena,
    pub strings: HashMap<&'static str, T>,
    pub rev_strings: Vec<&'static str>,
}

impl<T: From<usize> + Into<usize> + Copy> Interner<T> {
    pub fn new_with(defaults: &[(&'static str, usize)]) -> Self {
        Interner {
            arena: DroplessArena::default(),
            rev_strings: defaults.iter().map(|i| i.0).collect(),
            strings: HashMap::from_iter(defaults.iter().copied().map(|(s, idx)| (s, T::from(idx)))),
        }
    }

    #[inline]
    pub fn intern(&mut self, string: &str) -> T {
        if let Some(&val) = self.strings.get(string) {
            return val;
        }

        let val = T::from(self.rev_strings.len());

        // SAFETY: we convert from `&str` to `&[u8]`, clone it into the arena,
        // and immediately convert the clone back to `&[u8], all because there
        // is no `inner.arena.alloc_str()` method. This is clearly safe.
        let string: &str =
            unsafe { std::str::from_utf8_unchecked(self.arena.alloc_slice(string.as_bytes())) };

        // SAFETY: we can extend the arena allocation to `'static` because we
        // only access these while the arena is still alive.
        let string: &'static str = unsafe { &*(string as *const str) };
        self.rev_strings.push(string);

        *self.strings.entry(string).or_insert(val)
    }

    // Get the value as a string.
    pub fn get(&self, value: T) -> &str {
        let into: usize = value.into();
        self.rev_strings[into]
    }
}
