// #[cfg(test)]
// mod tests {
//     /// Only the first member of this struct counts for equality and hashing.
//     struct Pair(usize, usize);
//     impl PartialEq for Pair {
//         fn eq(&self, other: &Self) -> bool {
//             self.0 == other.0
//         }
//     }
//     impl Eq for Pair {}
//     impl std::hash::Hash for Pair {
//         fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//             self.0.hash(state);
//         }
//     }

//     /// Tests that inserting an equal key into a DashSet does NOT update the
//     /// key. This isn't documented by Dashmap, but it's important for the
//     /// NodeCache and is true for now because DashSet uses DashMap which
//     /// ultimately delegates to std::collections::HashMap, which does make this
//     /// promise.
//     #[test]
//     fn test_dashset_insert_eq() {
//         let h = dashmap::DashSet::new();
//         // These values should be kept.
//         assert!(h.insert(Pair(10, 4)));
//         assert!(h.insert(Pair(20, 4)));
//         // These values should not be reachable.
//         assert!(!h.insert(Pair(10, 8)));
//         assert!(!h.insert(Pair(20, 8)));
//         // Check that the first two inserts persisted while the second two did
//         // not do anything.
//         assert_eq!(4, h.get(&Pair(10, 0)).unwrap().1);
//         assert_eq!(4, h.get(&Pair(20, 0)).unwrap().1);
//     }
// }

#[cfg(test)]
mod tests {
    /// Only the first member of this struct counts for equality and hashing.
    struct Pair(usize, usize);
    impl PartialEq for Pair {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
    impl Eq for Pair {}
    impl std::hash::Hash for Pair {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.0.hash(state);
        }
    }

    /// Tests that inserting an equal key into a DashSet does NOT update the
    /// key. This isn't documented by Dashmap, but it's important for the
    /// NodeCache and is true for now because DashSet uses DashMap which
    /// ultimately delegates to std::collections::HashMap, which does make this
    /// promise.
    #[test]
    fn test_dashset_insert_eq() {
        let h = dashmap::DashMap::new();
        // These values should be kept.
        assert!(!h.insert(Pair(10, 4), 100));
        assert!(!h.insert(Pair(20, 4), 200));
        // These values should not be reachable.
        assert!(h.insert(Pair(10, 8), 300));
        assert!(h.insert(Pair(20, 8), 400));
        // Check that the first two inserts persisted while the second two did
        // not do anything.
        assert_eq!(4, h.get(&Pair(10, 0)).unwrap().key().1);
        assert_eq!(4, h.get(&Pair(20, 0)).unwrap().key().1);
    }

    #[test]
    fn test_sizeof_atomic_u8() {
        assert_eq!(1, std::mem::size_of::<std::sync::atomic::AtomicU8>());
    }
}
