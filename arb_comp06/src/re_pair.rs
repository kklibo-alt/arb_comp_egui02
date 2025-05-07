use crate::recode::{condense, expand, to_bytes, to_ids};
use crate::token::{Token, TokenId};
use crate::utils::{insert_with, remove_with, CollectCounts, MappedSets};
use indexmap::{IndexMap, IndexSet};
use keyed_priority_queue::KeyedPriorityQueue;

pub struct RePair {
    ids_to_tokens: IndexMap<TokenId, Token>,
    tokens_to_ids: IndexMap<Token, TokenId>,
}

impl RePair {
    fn add_id(&mut self, id: TokenId, token: Token) {
        self.ids_to_tokens.insert(id, token);
        self.tokens_to_ids.insert(token, id);
    }

    /// For each token id pair in `ids`: record the index of its first element.
    fn find_id_pairs(ids: &[TokenId]) -> IndexMap<(TokenId, TokenId), IndexSet<usize>> {
        let mut pair_locations = IndexMap::new();

        ids.windows(2).enumerate().for_each(|(i, ids)| {
            pair_locations
                .entry((ids[0], ids[1]))
                .or_insert(IndexSet::new())
                .insert(i);
        });

        pair_locations
    }

    //note: using TokenId of usize::MAX to indicate empty index (refine/replace?)
    fn get_prev_id(ids: &[TokenId], index: usize) -> Option<(TokenId, usize)> {
        for (i, &id) in ids.iter().enumerate().take(index).rev() {
            if id != TokenId(usize::MAX) {
                return Some((id, i));
            };
        }
        None
    }

    fn get_next_id(ids: &[TokenId], index: usize) -> Option<(TokenId, usize)> {
        for (i, &id) in ids.iter().enumerate().skip(index + 1) {
            if id != TokenId(usize::MAX) {
                return Some((id, i));
            };
        }
        None
    }

    fn replace_pair(
        id0: TokenId,
        id1: TokenId,
        locations: IndexSet<usize>,
        pattern: &mut [TokenId],
        replacement: TokenId,
    ) -> (MappedSets, MappedSets) {
        let mut added_pair_locations = MappedSets::new();
        let mut removed_pair_locations = MappedSets::new();

        for index0 in locations {
            assert_eq!(Some(&id0), pattern.get(index0));

            let (token_id1, index1) = Self::get_next_id(pattern, index0).unwrap();
            assert_eq!(id1, token_id1);

            let prev_token = Self::get_prev_id(pattern, index0);
            let next_token = Self::get_next_id(pattern, index1);

            if let Some((prev_id, prev_index)) = prev_token {
                removed_pair_locations.insert((prev_id, id0), prev_index);
                added_pair_locations.insert((prev_id, replacement), prev_index);
            }

            if let Some((next_id, _next_index)) = next_token {
                removed_pair_locations.insert((id1, next_id), index1);
                added_pair_locations.insert((replacement, next_id), index0);
            }

            *pattern.get_mut(index0).unwrap() = replacement;
            *pattern.get_mut(index1).unwrap() = TokenId(usize::MAX);
        }

        (added_pair_locations, removed_pair_locations)
    }

    // first attempt: ignore token repetition block overcounting for now
    pub fn new(data: &[&[u8]]) -> Self {
        let mut re_pair = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
        };

        (0..=u8::MAX).for_each(|x| re_pair.add_id(TokenId(x as usize), Token::Byte(x)));

        let mut patterns = data.iter().map(|x| re_pair.encode(x)).collect::<Vec<_>>();

        let mut pair_locations_in_sequences = patterns
            .iter()
            .map(|pattern| Self::find_id_pairs(pattern))
            .collect::<Vec<_>>();

        let pair_counts = pair_locations_in_sequences
            .iter()
            .flatten()
            .map(|(pair, locations)| (*pair, locations.len()))
            .collect_counts();

        let mut pair_occurrences: KeyedPriorityQueue<(TokenId, TokenId), usize> =
            pair_counts.into_iter().collect();

        while let Some(((id0, id1), count)) = pair_occurrences.pop() {
            if count < 2 {
                break;
            }
            let new_id = TokenId(re_pair.ids_to_tokens.len());
            re_pair.add_id(new_id, Token::Merge(id0, id1));

            let mut added_pair_counts = IndexMap::<(TokenId, TokenId), usize>::new();
            let mut removed_pair_counts = IndexMap::<(TokenId, TokenId), usize>::new();

            for (pattern, pair_locations) in patterns
                .iter_mut()
                .zip(pair_locations_in_sequences.iter_mut())
            {
                let locations = pair_locations.swap_remove(&(id0, id1)).unwrap_or_default();
                let (added_pair_locations, removed_pair_locations) =
                    Self::replace_pair(id0, id1, locations, pattern, new_id);

                fn add(acc: &mut usize, x: usize) {
                    *acc += x;
                }

                fn insert_set(acc: &mut IndexSet<usize>, mut other: IndexSet<usize>) {
                    acc.append(&mut other);
                }

                fn remove_set(acc: &mut IndexSet<usize>, other: IndexSet<usize>) {
                    other.iter().for_each(|x| {
                        acc.swap_remove(x);
                    });
                }

                let added_pair_lengths_iter = added_pair_locations
                    .0
                    .iter()
                    .map(|(&pair, locations)| (pair, locations.len()));

                let removed_pair_lengths_iter = removed_pair_locations
                    .0
                    .iter()
                    .map(|(&pair, locations)| (pair, locations.len()));

                insert_with(&mut removed_pair_counts, removed_pair_lengths_iter, add);
                insert_with(&mut added_pair_counts, added_pair_lengths_iter, add);

                insert_with(pair_locations, added_pair_locations.0, insert_set);
                remove_with(pair_locations, removed_pair_locations.0, remove_set);
            }

            added_pair_counts.iter().for_each(|(new_pair, new_count)| {
                if let Some(&count) = pair_occurrences.get_priority(new_pair) {
                    pair_occurrences
                        .set_priority(new_pair, new_count + count)
                        .unwrap();
                } else {
                    pair_occurrences.push(*new_pair, *new_count);
                };
            });

            removed_pair_counts
                .iter()
                .for_each(|(removed_pair, removed_count)| {
                    let count = pair_occurrences.get_priority(removed_pair).unwrap();
                    assert!(count >= removed_count);
                    pair_occurrences
                        .set_priority(removed_pair, count - *removed_count)
                        .unwrap();
                });
        }

        re_pair
    }

    pub fn encode(&self, data: &[u8]) -> Vec<TokenId> {
        let pattern = to_ids(data, &self.tokens_to_ids);
        let merge_if = |id0, id1| self.tokens_to_ids.get(&Token::Merge(id0, id1)).copied();

        condense(pattern, merge_if)
    }

    pub fn decode(&self, data: Vec<TokenId>) -> Vec<u8> {
        let mut result = data;

        result = expand(result, &self.ids_to_tokens);

        to_bytes(&result, &self.ids_to_tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_re_pair() {
        let re_pair = RePair::new(&[]);
        assert_eq!(
            re_pair.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        assert_eq!(
            re_pair.decode(vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]),
            vec![0x61, 0x62, 0x63]
        );

        let re_pair = RePair::new(&[&[0x61, 0x62, 0x63], &[0x64, 0x65, 0x66]]);
        assert_eq!(
            re_pair.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        assert_eq!(
            re_pair.decode(vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]),
            vec![0x61, 0x62, 0x63]
        );

        let re_pair = RePair::new(&[
            &[0x61, 0x62, 0x63],
            &[0x64, 0x65, 0x66],
            &[0x61, 0x62, 0x63],
        ]);
        assert_eq!(re_pair.encode(&[0x61, 0x62, 0x63]), vec![TokenId(257)]);
        assert_eq!(re_pair.decode(vec![TokenId(257)]), vec![0x61, 0x62, 0x63]);

        /*
        let re_pair = RePair::new(&[&[1, 2, 3, 2, 3, 4], &[1, 2, 3, 1, 2, 3]]);
        assert_eq!(
            re_pair.encode(&[1, 2, 3, 2, 3, 4]),
            vec![TokenId(257), TokenId(256), TokenId(4)]
        );
        assert_eq!(
            re_pair.decode(vec![TokenId(257), TokenId(256), TokenId(4)]),
            vec![1, 2, 3, 2, 3, 4]
        );
        */
    }
}
