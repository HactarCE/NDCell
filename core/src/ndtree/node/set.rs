//! Concurrent `HashSet<Box<T>>` that requires exclusive access to remove
//! elements.

use itertools::Itertools;
use parking_lot::{Mutex, MutexGuard};
use std::collections::HashSet;
use std::hash::Hash;
use std::hash::{BuildHasher, Hasher};

use crate::NodeHasher;

/// Number of "shards" to split the `ShardedBoxedSet` into.
///
/// TODO: Measure performance with different shard counts.
const SHARD_COUNT: usize = 64;

/// Single "shard" of the set.
type Shard<T> = HashSet<Box<T>, NodeHasher>;

/// `HashSet` split into many "shards" to allow multiple threads to access
/// different parts of the set simultaneously with minimal contention.
pub struct ShardedBoxedSet<T: Eq + Hash> {
    shards: Box<[Mutex<Shard<T>>]>,
}
impl<T: Eq + Hash> Drop for ShardedBoxedSet<T> {
    fn drop(&mut self) {
        self.retain(false, |_| false)
    }
}
impl<T: Eq + Hash> ShardedBoxedSet<T> {
    /// Creates an empty `ShardedBoxedSet`.
    pub fn new() -> Self {
        // TODO: when #[feature(const_generics)] stabalizes, replace `shards`
        // with a fixed-size array and derive Default
        Self {
            shards: std::iter::repeat_with(Default::default)
                .take(SHARD_COUNT)
                .collect_vec()
                .into_boxed_slice(),
        }
    }

    /// Returns the shard that should contain the given element. This blocks
    /// until the shard is available, so do NOT hold multiple shards at once and
    /// try to release this guard as soon as possible.
    fn get_shard<'a>(&'a self, elem: &T) -> MutexGuard<'a, Shard<T>> {
        let mut h = NodeHasher::default().build_hasher();
        elem.hash(&mut h);
        // Hash a little extra, so that the hash here is distinct from the one
        // used within the individual shard.
        SHARD_COUNT.hash(&mut h);
        let shard_index = h.finish() as usize % SHARD_COUNT;
        self.shards[shard_index].lock()
    }

    /// Returns a reference the equivalent element in the `ShardedBoxedSet` if
    /// it is already present, or inserts the element and returns a reference to
    /// it otherwise.
    ///
    /// The second element of the return value is `true` if the element was
    /// newly inserted or `false` if the element was already present.
    pub fn get_or_insert<'a>(&'a self, elem: T) -> (&'a T, bool) {
        let mut shard = self.get_shard(&elem);
        let mut already_present = true;
        // This is ugly, but there's just no API yet for inserting a key into a
        // `HashSet` and immediately getting a reference back.
        let ptr = shard
            .get(&elem)
            .map(|boxed| &**boxed as *const T)
            .unwrap_or_else(|| {
                already_present = false;
                // Pin the element on the heap before we take a reference to it.
                let custom_box = Box::new(elem);
                // Extend the lifetime of the reference, which is safe because
                // `boxed` will be moved into `shard`.
                let ptr = &*custom_box as *const T;
                // We should be inserting the element for the first time. (If not,
                // `ptr` would be dangling!)
                assert!(shard.insert(custom_box));
                ptr
            });
        // Because removal requires a `&mut ShardedBoxedSet`, and the element is
        // `Box`ed so it can't move, it is safe to return a `&T` with the
        // lifetime of the `&ShardedBoxedSet`.
        (unsafe { &*ptr }, already_present)
    }

    /// Retains only the elements specified by the predicate.
    ///
    /// In other words, remove all elements `e` such that `f(&e)` returns
    /// `false`.
    ///
    /// If `shrink` is `true`, call `shrink_to_fit()` on each shard after
    /// removing the elements.
    pub fn retain<'a>(&'a mut self, shrink: bool, mut f: impl FnMut(&T) -> bool) {
        for shard in &self.shards[..] {
            let mut shard = shard.lock();
            shard.retain(|elem| f(&elem));
            if shrink {
                shard.shrink_to_fit();
            }
        }
    }

    /// Executes the closure for each element.
    pub fn for_each(&self, mut f: impl FnMut(&T)) {
        for shard in &self.shards[..] {
            for elem in &*shard.lock() {
                f(&elem);
            }
        }
    }
}
