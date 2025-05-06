use indexmap::{IndexMap, IndexSet};
use std::{hash::Hash, ops::AddAssign};

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

pub fn append_to_sets<K, V>(acc: &mut IndexMap<K, IndexSet<V>>, to_add: IndexMap<K, IndexSet<V>>)
where
    K: Hash + Eq + Copy,
    V: Hash + Eq + Copy,
{
    to_add.into_iter().for_each(|(key, mut set)| {
        acc.entry(key).or_default().append(&mut set);
    });
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
    fn test_collect_counts() {
        let counts = vec![("a", 1), ("b", 1), ("a", 1), ("b", 3)]
            .into_iter()
            .collect_counts();
        assert_eq!(counts.get(&"a"), Some(&2));
        assert_eq!(counts.get(&"b"), Some(&4));
        assert_eq!(counts.get(&"c"), None);
    }
}
