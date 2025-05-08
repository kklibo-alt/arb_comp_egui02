use indexmap::{map::Entry, IndexMap, IndexSet};
use keyed_priority_queue::KeyedPriorityQueue;
use std::{
    hash::Hash,
    ops::{AddAssign, SubAssign},
};

use crate::token::TokenId;

pub fn add_to_counts<T>(acc: &mut IndexMap<T, usize>, x: &IndexMap<T, usize>)
where
    T: Hash + Eq + PartialEq + Copy,
{
    x.iter().for_each(|(&key, &count)| {
        acc.entry(key).and_modify(|c| *c += count).or_insert(count);
    })
}

pub fn increment<T>(acc: &mut IndexMap<T, usize>, key: T)
where
    T: Hash + Eq + PartialEq + Copy,
{
    acc.entry(key).and_modify(|c| *c += 1).or_insert(1);
}

pub fn insert_with<K, V, I, F>(acc: &mut IndexMap<K, V>, iterable: I, insert: F)
where
    K: Hash + Eq,
    I: IntoIterator<Item = (K, V)>,
    F: Fn(&mut V, V),
{
    for (key, value) in iterable {
        match acc.entry(key) {
            Entry::Occupied(mut x) => {
                insert(x.get_mut(), value);
            }
            Entry::Vacant(x) => {
                x.insert(value);
            }
        }
    }
}

pub fn remove_with<K, V, I, F>(acc: &mut IndexMap<K, V>, iterable: I, remove: F)
where
    K: Hash + Eq,
    I: IntoIterator<Item = (K, V)>,
    F: Fn(&mut V, V),
{
    for (key, value) in iterable {
        match acc.entry(key) {
            Entry::Occupied(mut x) => {
                remove(x.get_mut(), value);
            }
            Entry::Vacant(_x) => {}
        }
    }
}

pub fn append_to_sets<K, V>(acc: &mut IndexMap<K, IndexSet<V>>, to_add: IndexMap<K, IndexSet<V>>)
where
    K: Hash + Eq + Copy,
    V: Hash + Eq + Copy,
{
    //to_add.into_iter().for_each(|(key, mut set)| {
    //    acc.entry(key).or_default().append(&mut set);
    //});

    insert_with(acc, to_add, |acc, mut other| acc.append(&mut other));
}

pub trait CollectCounts<K, V>: Iterator<Item = (K, V)> {
    fn collect_counts(self) -> IndexMap<K, V>
    where
        K: Eq + Hash,
        V: AddAssign + Default,
        //H: FromIterator<(K,V)>,
        Self: Sized,
    {
        self.fold(IndexMap::new(), |mut map, (k, v)| {
            let e = map.entry(k).or_default();
            *e += v;

            map
        })
    }
}
impl<T, K, V> CollectCounts<K, V> for T where T: Iterator<Item = (K, V)> + ?Sized {}

pub fn increase_priorities<'a, I>(
    acc: &mut KeyedPriorityQueue<(TokenId, TokenId), usize>,
    iterable: I,
) where
    I: Iterator<Item = (&'a (TokenId, TokenId), usize)>,
{
    for (&key, value) in iterable {
        match acc.entry(key) {
            keyed_priority_queue::Entry::Occupied(entry) => {
                let current = *entry.get_priority();
                entry.set_priority(current + value);
            }
            keyed_priority_queue::Entry::Vacant(entry) => {
                entry.set_priority(value);
            }
        }
    }
}

pub fn decrease_priorities<'a, I>(
    acc: &mut KeyedPriorityQueue<(TokenId, TokenId), usize>,
    iterable: I,
) where
    I: Iterator<Item = (&'a (TokenId, TokenId), usize)>,
{
    for (&key, value) in iterable {
        match acc.entry(key) {
            keyed_priority_queue::Entry::Occupied(entry) => {
                let current = *entry.get_priority();
                if current >= value {
                    entry.set_priority(current - value);
                } else {
                    entry.set_priority(0);
                    panic!("temp panic: overdrawn priority");
                }
            }
            keyed_priority_queue::Entry::Vacant(_entry) => {
                panic!("temp panic: decreasing absent priority");
            }
        }
    }
}

#[derive(Debug, Default)]
pub struct MappedSets(pub IndexMap<(TokenId, TokenId), IndexSet<usize>>);

pub struct Lengths<'a> {
    iter: indexmap::map::Iter<'a, (TokenId, TokenId), IndexSet<usize>>,
}

impl<'a> Iterator for Lengths<'a> {
    type Item = (&'a (TokenId, TokenId), usize);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|(key, set)| (key, set.len()))
    }
}

impl MappedSets {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lengths(&self) -> Lengths {
        Lengths {
            iter: self.0.iter(),
        }
    }

    pub fn insert(&mut self, key: (TokenId, TokenId), value: usize) {
        self.0.entry(key).or_default().insert(value);
    }
    pub fn extend<T: IntoIterator<Item = ((TokenId, TokenId), usize)>>(&mut self, iter: T) {
        iter.into_iter().for_each(|(key, value)| {
            self.insert(key, value);
        });
    }
}

impl FromIterator<((TokenId, TokenId), usize)> for MappedSets {
    fn from_iter<T: IntoIterator<Item = ((TokenId, TokenId), usize)>>(iter: T) -> Self {
        let mut x = Self::default();
        x.extend(iter);
        x
    }
}

impl AddAssign for MappedSets {
    fn add_assign(&mut self, rhs: Self) {
        for (key, mut set) in rhs.0 {
            match self.0.entry(key) {
                Entry::Occupied(mut x) => {
                    x.get_mut().append(&mut set);
                }
                Entry::Vacant(x) => {
                    x.insert(set);
                }
            }
        }
    }
}

impl SubAssign for MappedSets {
    fn sub_assign(&mut self, rhs: Self) {
        for (key, set) in rhs.0 {
            match self.0.entry(key) {
                Entry::Occupied(mut entry) => {
                    set.iter().for_each(|item| {
                        entry.get_mut().swap_remove(item);
                    });
                }
                Entry::Vacant(_x) => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_increment() {
        let mut acc = IndexMap::new();
        increment(&mut acc, 1);
        assert_eq!(acc[&1], 1);
        increment(&mut acc, 1);
        assert_eq!(acc[&1], 2);
    }

    #[test]
    fn test_add_to_counts() {
        let mut acc = IndexMap::new();
        add_to_counts(&mut acc, &IndexMap::from([(1, 1), (2, 1)]));
        assert_eq!(acc[&1], 1);
        assert_eq!(acc[&2], 1);
        add_to_counts(&mut acc, &IndexMap::from([(1, 1), (2, 1)]));
        assert_eq!(acc[&1], 2);
        assert_eq!(acc[&2], 2);
    }

    #[test]
    fn test_append_to_sets() {
        let mut acc = IndexMap::new();

        append_to_sets(&mut acc, IndexMap::from([(1, IndexSet::from([2, 3]))]));
        append_to_sets(&mut acc, IndexMap::from([(1, IndexSet::from([4, 5]))]));
        append_to_sets(&mut acc, IndexMap::from([(2, IndexSet::from([6, 7]))]));

        assert_eq!(acc[&1], IndexSet::from([2, 3, 4, 5]));
        assert_eq!(acc[&2], IndexSet::from([6, 7]));
        assert_eq!(acc.len(), 2);
    }

    #[test]
    fn test_append_to_sets_empty_maps() {
        // Added by Claude 3.7 Sonnet: Test handling of empty maps
        // Empty acc + empty to_add
        let mut empty_acc = IndexMap::<i32, IndexSet<i32>>::new();
        append_to_sets(&mut empty_acc, IndexMap::new());
        assert_eq!(empty_acc.len(), 0);

        // Empty acc + non-empty to_add
        let mut acc = IndexMap::new();
        append_to_sets(&mut acc, IndexMap::from([(1, IndexSet::from([10, 20]))]));
        assert_eq!(acc[&1], IndexSet::from([10, 20]));
        assert_eq!(acc.len(), 1);

        // Non-empty acc + empty to_add
        let mut acc = IndexMap::from([(1, IndexSet::from([10, 20]))]);
        append_to_sets(&mut acc, IndexMap::new());
        assert_eq!(acc[&1], IndexSet::from([10, 20]));
        assert_eq!(acc.len(), 1);
    }

    #[test]
    fn test_append_to_sets_duplicates() {
        // Added by Claude 3.7 Sonnet: Test deduplication of values
        // Test with overlapping values (should be unique in result)
        let mut acc = IndexMap::from([(1, IndexSet::from([10, 20, 30]))]);
        append_to_sets(
            &mut acc,
            IndexMap::from([(1, IndexSet::from([20, 30, 40]))]),
        );
        assert_eq!(acc[&1], IndexSet::from([10, 20, 30, 40]));
        assert_eq!(acc[&1].len(), 4); // No duplicates
    }

    #[test]
    fn test_append_to_sets_multiple_operations() {
        // Added by Claude 3.7 Sonnet: Test multiple append operations across different keys
        let mut acc = IndexMap::new();

        // Multiple append operations
        append_to_sets(
            &mut acc,
            IndexMap::from([(1, IndexSet::from([10, 20])), (2, IndexSet::from([30, 40]))]),
        );

        append_to_sets(
            &mut acc,
            IndexMap::from([(2, IndexSet::from([50, 60])), (3, IndexSet::from([70, 80]))]),
        );

        append_to_sets(
            &mut acc,
            IndexMap::from([
                (1, IndexSet::from([90])),
                (3, IndexSet::from([100])),
                (4, IndexSet::from([110])),
            ]),
        );

        assert_eq!(acc[&1], IndexSet::from([10, 20, 90]));
        assert_eq!(acc[&2], IndexSet::from([30, 40, 50, 60]));
        assert_eq!(acc[&3], IndexSet::from([70, 80, 100]));
        assert_eq!(acc[&4], IndexSet::from([110]));
        assert_eq!(acc.len(), 4);
    }

    #[test]
    fn test_append_to_sets_string_keys() {
        // Added by Claude 3.7 Sonnet: Test with string keys and values
        let mut acc = IndexMap::new();
        append_to_sets(
            &mut acc,
            IndexMap::from([
                ("fruits", IndexSet::from(["apple", "banana"])),
                ("vegetables", IndexSet::from(["carrot", "potato"])),
            ]),
        );

        append_to_sets(
            &mut acc,
            IndexMap::from([
                ("fruits", IndexSet::from(["orange", "grape"])),
                ("grains", IndexSet::from(["wheat", "rice"])),
            ]),
        );

        assert_eq!(
            acc[&"fruits"],
            IndexSet::from(["apple", "banana", "orange", "grape"])
        );
        assert_eq!(acc[&"vegetables"], IndexSet::from(["carrot", "potato"]));
        assert_eq!(acc[&"grains"], IndexSet::from(["wheat", "rice"]));
        assert_eq!(acc.len(), 3);
    }

    #[test]
    fn test_append_to_sets_large_sets() {
        // Added by Claude 3.7 Sonnet: Test with large sets and boundary conditions
        // Test with large sets
        let mut acc = IndexMap::new();
        let mut large_set1 = IndexSet::new();
        let mut large_set2 = IndexSet::new();

        for i in 0..100 {
            large_set1.insert(i);
        }

        for i in 50..150 {
            large_set2.insert(i);
        }

        append_to_sets(&mut acc, IndexMap::from([(1, large_set1)]));
        append_to_sets(&mut acc, IndexMap::from([(1, large_set2)]));

        assert_eq!(acc[&1].len(), 150); // 0-149 with no duplicates
        assert!(acc[&1].contains(&0));
        assert!(acc[&1].contains(&75));
        assert!(acc[&1].contains(&149));
    }

    #[test]
    fn test_collect_counts() {
        let counts = vec![("a", 1), ("b", 1), ("a", 1), ("b", 3)]
            .into_iter()
            .collect_counts();
        assert_eq!(counts.get(&"a"), Some(&2));
        assert_eq!(counts.get(&"b"), Some(&4));
        assert_eq!(counts.get(&"c"), None);
    }
}
