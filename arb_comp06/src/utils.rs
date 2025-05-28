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
                    entry.remove();
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
                    if entry.get().is_empty() {
                        entry.swap_remove();
                    }
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
}
