use std::{
    alloc::Layout,
    cell::{Cell, RefCell},
    cmp,
    mem::{self, MaybeUninit},
    ptr, slice,
};

struct TypedArenaChunk<T> {
    /// The raw storage for the arena chunk.
    storage: Box<[MaybeUninit<T>]>,
}

impl<T> TypedArenaChunk<T> {
    #[inline]
    unsafe fn new(capacity: usize) -> TypedArenaChunk<T> {
        TypedArenaChunk {
            storage: Box::new_uninit_slice(capacity),
        }
    }

    // Returns a pointer to the first allocated object.
    #[inline]
    fn start(&mut self) -> *mut T {
        self.storage.as_mut_ptr().cast()
    }

    // Returns a pointer to the end of the allocated space.
    #[inline]
    fn end(&mut self) -> *mut T {
        unsafe {
            if mem::size_of::<T>() == 0 {
                // A pointer as large as possible for zero-sized elements.
                !0 as *mut T
            } else {
                self.start().add(self.storage.len())
            }
        }
    }
}

// The arenas start with PAGE-sized chunks, and then each new chunk is twice as
// big as its predecessor, up until we reach HUGE_PAGE-sized chunks, whereupon
// we stop growing. This scales well, from arenas that are barely used up to
// arenas that are used for 100s of MiBs. Note also that the chosen sizes match
// the usual sizes of pages and huge pages on Linux.
const PAGE: usize = 4096;
const HUGE_PAGE: usize = 2 * 1024 * 1024;

/// An arena that can hold objects of multiple different types that impl `Copy`
/// and/or satisfy `!mem::needs_drop`.
pub struct DroplessArena {
    /// A pointer to the start of the free space.
    start: Cell<*mut u8>,

    /// A pointer to the end of free space.
    ///
    /// The allocation proceeds downwards from the end of the chunk towards the
    /// start. (This is slightly simpler and faster than allocating upwards,
    /// see <https://fitzgeraldnick.com/2019/11/01/always-bump-downwards.html>.)
    /// When this pointer crosses the start pointer, a new chunk is allocated.
    end: Cell<*mut u8>,

    /// A vector of arena chunks.
    chunks: RefCell<Vec<TypedArenaChunk<u8>>>,
}

unsafe impl Send for DroplessArena {}

impl Default for DroplessArena {
    #[inline]
    fn default() -> DroplessArena {
        DroplessArena {
            start: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            chunks: Default::default(),
        }
    }
}

impl DroplessArena {
    #[inline(never)]
    #[cold]
    fn grow(&self, additional: usize) {
        unsafe {
            let mut chunks = self.chunks.borrow_mut();
            let mut new_cap;
            if let Some(last_chunk) = chunks.last_mut() {
                // There is no need to update `last_chunk.entries` because that
                // field isn't used by `DroplessArena`.

                // If the previous chunk's len is less than HUGE_PAGE
                // bytes, then this chunk will be least double the previous
                // chunk's size.
                new_cap = last_chunk.storage.len().min(HUGE_PAGE / 2);
                new_cap *= 2;
            } else {
                new_cap = PAGE;
            }
            // Also ensure that this chunk can fit `additional`.
            new_cap = cmp::max(additional, new_cap);

            let mut chunk = TypedArenaChunk::<u8>::new(new_cap);
            self.start.set(chunk.start());
            self.end.set(chunk.end());
            chunks.push(chunk);
        }
    }

    /// Allocates a byte slice with specified layout from the current memory
    /// chunk. Returns `None` if there is no free space left to satisfy the
    /// request.
    #[inline]
    fn alloc_raw_without_grow(&self, layout: Layout) -> Option<*mut u8> {
        let start = self.start.get() as usize;
        let end = self.end.get() as usize;

        let align = layout.align();
        let bytes = layout.size();

        let new_end = end.checked_sub(bytes)? & !(align - 1);
        if start <= new_end {
            let new_end = new_end as *mut u8;
            self.end.set(new_end);
            Some(new_end)
        } else {
            None
        }
    }

    #[inline]
    pub fn alloc_raw(&self, layout: Layout) -> *mut u8 {
        assert!(layout.size() != 0);
        loop {
            if let Some(a) = self.alloc_raw_without_grow(layout) {
                break a;
            }
            // No free space left. Allocate a new chunk to satisfy the request.
            // On failure the grow will panic or abort.
            self.grow(layout.size());
        }
    }

    /// Allocates a slice of objects that are copied into the `DroplessArena`, returning a mutable
    /// reference to it. Will panic if passed a zero-sized type.
    ///
    /// Panics:
    ///
    ///  - Zero-sized types
    ///  - Zero-length slices
    #[inline]
    pub fn alloc_slice<T>(&self, slice: &[T]) -> &mut [T]
    where
        T: Copy,
    {
        assert!(!mem::needs_drop::<T>());
        assert!(mem::size_of::<T>() != 0);
        assert!(!slice.is_empty());

        let mem = self.alloc_raw(Layout::for_value::<[T]>(slice)) as *mut T;

        unsafe {
            mem.copy_from_nonoverlapping(slice.as_ptr(), slice.len());
            slice::from_raw_parts_mut(mem, slice.len())
        }
    }

    #[inline]
    pub fn alloc<T>(&self, object: T) -> &mut T {
        assert!(!mem::needs_drop::<T>());
        assert!(size_of::<T>() != 0);

        let mem = self.alloc_raw(Layout::new::<T>()) as *mut T;

        unsafe {
            // Write into uninitialized memory.
            ptr::write(mem, object);
            &mut *mem
        }
    }

    /// # Safety
    ///
    /// The caller must ensure that `mem` is valid for writes up to `size_of::<T>() * len`, and that
    /// that memory stays allocated and not shared for the lifetime of `self`. This must hold even
    /// if `iter.next()` allocates onto `self`.
    #[inline]
    unsafe fn write_from_iter<T, I: Iterator<Item = T>>(
        &self,
        mut iter: I,
        len: usize,
        mem: *mut T,
    ) -> &mut [T] {
        let mut i = 0;
        // Use a manual loop since LLVM manages to optimize it better for
        // slice iterators
        loop {
            // SAFETY: The caller must ensure that `mem` is valid for writes up to
            // `size_of::<T>() * len`.
            unsafe {
                match iter.next() {
                    Some(value) if i < len => mem.add(i).write(value),
                    Some(_) | None => {
                        // We only return as many items as the iterator gave us, even
                        // though it was supposed to give us `len`
                        return slice::from_raw_parts_mut(mem, i);
                    }
                }
            }
            i += 1;
        }
    }

    #[inline]
    pub fn alloc_from_iter<T, I: IntoIterator<Item = T>>(&self, iter: I) -> &mut [T] {
        // Warning: this function is reentrant: `iter` could hold a reference to `&self` and
        // allocate additional elements while we're iterating.
        let iter = iter.into_iter();
        assert!(size_of::<T>() != 0);
        assert!(!mem::needs_drop::<T>());

        let size_hint = iter.size_hint();

        match size_hint {
            (min, Some(max)) if min == max => {
                // We know the exact number of elements the iterator expects to produce here.
                let len = min;

                if len == 0 {
                    return &mut [];
                }

                let mem = self.alloc_raw(Layout::array::<T>(len).unwrap()) as *mut T;
                // SAFETY: `write_from_iter` doesn't touch `self`. It only touches the slice we just
                // reserved. If the iterator panics or doesn't output `len` elements, this will
                // leave some unallocated slots in the arena, which is fine because we do not call
                // `drop`.
                unsafe { self.write_from_iter(iter, len, mem) }
            }
            (_, _) => outline(
                move || match self.try_alloc_from_iter(iter.map(Ok::<T, ()>)) {
                    Ok(o) => o,
                    Err(_) => unreachable!(),
                },
            ),
        }
    }

    #[inline]
    pub fn try_alloc_from_iter<T, E>(
        &self,
        iter: impl IntoIterator<Item = Result<T, E>>,
    ) -> Result<&mut [T], E> {
        // Despite the similarlty with `alloc_from_iter`, we cannot reuse their fast case, as we
        // cannot know the minimum length of the iterator in this case.
        assert!(size_of::<T>() != 0);

        // Takes care of reentrancy.
        let vec: Result<Vec<T>, E> = iter.into_iter().collect();
        let mut vec = vec?;
        if vec.is_empty() {
            return Ok(&mut []);
        }
        // Move the content to the arena by copying and then forgetting it.
        let len = vec.len();
        Ok(unsafe {
            let start_ptr = self.alloc_raw(Layout::for_value::<[T]>(vec.as_slice())) as *mut T;
            vec.as_ptr().copy_to_nonoverlapping(start_ptr, len);
            vec.set_len(0);
            slice::from_raw_parts_mut(start_ptr, len)
        })
    }
}

/// This calls the passed function while ensuring it won't be inlined into the caller.
#[inline(never)]
#[cold]
fn outline<F: FnOnce() -> R, R>(f: F) -> R {
    f()
}

pub trait ArenaAllocatable<'tcx>: Sized {
    #[allow(clippy::mut_from_ref)]
    fn allocate_on(self, arena: &'tcx DroplessArena) -> &'tcx mut Self;
    #[allow(clippy::mut_from_ref)]
    fn allocate_from_iter(
        arena: &'tcx DroplessArena,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'tcx mut [Self];
}

// Any type that impls `Copy` can be arena-allocated in the `DroplessArena`.
impl<'cx, T: Copy> ArenaAllocatable<'cx> for T {
    #[inline]
    #[allow(clippy::mut_from_ref)]
    fn allocate_on(self, arena: &'cx DroplessArena) -> &'cx mut Self {
        arena.alloc(self)
    }
    #[inline]
    #[allow(clippy::mut_from_ref)]
    fn allocate_from_iter(
        arena: &'cx DroplessArena,
        iter: impl ::std::iter::IntoIterator<Item = Self>,
    ) -> &'cx mut [Self] {
        arena.alloc_from_iter(iter)
    }
}
