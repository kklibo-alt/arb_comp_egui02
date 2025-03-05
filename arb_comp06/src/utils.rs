use indexmap::IndexMap;
use std::hash::Hash;

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
