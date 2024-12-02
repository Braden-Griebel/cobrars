//! Utility functions for getting hashes
use std::format;
use std::hash::{DefaultHasher, Hash, Hasher};

pub(crate) fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

pub(crate) fn hash_as_hex_string<T: Hash>(t: &T) -> String {
    format!("{:x}", calculate_hash(t))
}