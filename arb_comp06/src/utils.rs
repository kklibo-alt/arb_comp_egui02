use indexmap::IndexMap;
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
    fn test_collect_counts() {
        let counts = vec![("a", 1), ("b", 1), ("a", 1), ("b", 3)]
            .into_iter()
            .collect_counts();
        assert_eq!(counts.get(&"a"), Some(&2));
        assert_eq!(counts.get(&"b"), Some(&4));
        assert_eq!(counts.get(&"c"), None);
    }
}
