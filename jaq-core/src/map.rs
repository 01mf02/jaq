use alloc::vec::Vec;
use core::hash::Hash;
use fxhash::FxBuildHasher;
use indexmap::IndexMap;

/// A map that preserves the order of its elements.
type FxIndexMap<K, V> = IndexMap<K, V, FxBuildHasher>;

#[derive(Clone, Debug)]
pub struct Map<K, V>(FxIndexMap<K, V>);

impl<K, V> Map<K, V> {
    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.0.keys()
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.0.values()
    }
}

impl<K: Ord, V> Map<K, V> {
    fn sorted_by_key(&self) -> Vec<(&K, &V)> {
        let mut v: Vec<_> = self.0.iter().collect();
        v.sort_by_key(|(k, _v)| *k);
        v
    }
}

impl<K: Eq + Hash, V> Map<K, V> {
    pub fn get(&self, key: &K) -> Option<&V> {
        self.0.get(key)
    }
}

impl<K: Eq + Ord, V: PartialEq> PartialEq for Map<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.sorted_by_key() == other.sorted_by_key()
    }
}

impl<K: Ord, V: PartialOrd> PartialOrd for Map<K, V> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        let l = self.sorted_by_key();
        let r = other.sorted_by_key();
        // TODO: make this nicer
        let kl = l.iter().map(|(k, _v)| k);
        let kr = r.iter().map(|(k, _v)| k);
        let vl = l.iter().map(|(_k, v)| v);
        let vr = r.iter().map(|(_k, v)| v);
        // TODO: remove unwrap
        let bla = kl.cmp(kr).then_with(|| vl.partial_cmp(vr).unwrap());
        Some(bla)
    }
}

impl<K: Eq + Hash, V> core::iter::FromIterator<(K, V)> for Map<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<K, V> IntoIterator for Map<K, V> {
    type Item = (K, V);
    type IntoIter = indexmap::map::IntoIter<K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<K: Eq + Hash, V> core::ops::Add for Map<K, V> {
    type Output = Self;

    fn add(mut self, other: Self) -> Self {
        self.0.extend(other);
        self
    }
}
