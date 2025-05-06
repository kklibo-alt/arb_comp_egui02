use crate::recode::{condense, expand, to_bytes, to_ids};
use crate::token::{find_most_common_duplicate_id_pair, merge, Token, TokenId};
use crate::utils::CollectCounts;
use indexmap::{IndexMap, IndexSet};
use keyed_priority_queue::KeyedPriorityQueue;

pub struct Bpe {
    ids_to_tokens: IndexMap<TokenId, Token>,
    tokens_to_ids: IndexMap<Token, TokenId>,
}

impl Bpe {
    fn add_id(&mut self, id: TokenId, token: Token) {
        self.ids_to_tokens.insert(id, token);
        self.tokens_to_ids.insert(token, id);
    }

    pub fn ids_to_tokens(&self) -> &IndexMap<TokenId, Token> {
        &self.ids_to_tokens
    }

    pub fn tokens_to_ids(&self) -> &IndexMap<Token, TokenId> {
        &self.tokens_to_ids
    }

    pub fn new(data: &[&[u8]]) -> Self {
        //temp
        return Self::new_faster(data);
        let mut bpe = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
        };

        (0..=u8::MAX).for_each(|x| bpe.add_id(TokenId(x as usize), Token::Byte(x)));

        let mut patterns = data.iter().map(|x| bpe.encode(x)).collect::<Vec<_>>();

        while let Some(((id0, id1), _count)) = find_most_common_duplicate_id_pair(patterns.iter()) {
            let new_id = bpe.ids_to_tokens.len();
            bpe.add_id(TokenId(new_id), Token::Merge(id0, id1));

            let merge_if = |current_id, next_id| {
                if current_id == id0 && next_id == id1 {
                    Some(TokenId(new_id))
                } else {
                    None
                }
            };

            patterns = patterns
                .iter()
                .map(|pattern| merge(pattern.iter().copied(), merge_if))
                .collect();
        }

        bpe
    }

    // first attempt: ignore token repetition block overcounting for now
    pub fn new_faster(data: &[&[u8]]) -> Self {
        let mut bpe = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
        };

        (0..=u8::MAX).for_each(|x| bpe.add_id(TokenId(x as usize), Token::Byte(x)));

        let mut patterns = data.iter().map(|x| bpe.encode(x)).collect::<Vec<_>>();

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

        let mut pair_locations_in_sequences = patterns
            .iter()
            .map(|pattern| find_id_pairs(pattern))
            .collect::<Vec<_>>();

        let pair_counts = pair_locations_in_sequences
            .iter()
            .flatten()
            .map(|(pair, locations)| (*pair, locations.len()))
            .collect_counts();

        let mut pair_occurrences: KeyedPriorityQueue<(TokenId, TokenId), usize> =
            pair_counts.into_iter().collect();

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

        #[derive(Debug)]
        struct ReplacePairEffects {
            new_pair_locations: IndexSet<usize>,
            new_pair_count: usize,
            removed_pair_locations: IndexMap<(TokenId, TokenId), IndexSet<usize>>,
            removed_pair_counts: IndexMap<(TokenId, TokenId), usize>,
        }

        fn replace_pair(
            id0: TokenId,
            id1: TokenId,
            locations: IndexSet<usize>,
            pattern: &mut [TokenId],
            replacement: TokenId,
        ) -> ReplacePairEffects {
            let mut new_pair_locations = IndexSet::new();
            let mut new_pair_count = 0;
            let mut removed_pair_locations = IndexMap::<_, IndexSet<usize>>::new();
            let mut removed_pair_counts = IndexMap::new();

            for index0 in locations {
                assert_eq!(Some(&id0), pattern.get(index0));

                let (token_id1, index1) = get_next_id(&pattern, index0).unwrap();
                assert_eq!(id1, token_id1);

                let prev_token = get_prev_id(pattern, index0);
                let next_token = get_next_id(pattern, index1);

                if let Some((prev_id, prev_index)) = prev_token {
                    let pair = (prev_id, id0);
                    removed_pair_locations
                        .entry(pair)
                        .or_default()
                        .insert(prev_index);
                    *removed_pair_counts.entry(pair).or_default() += 1;
                }

                if let Some((next_id, next_index)) = next_token {
                    let pair = (id1, next_id);
                    removed_pair_locations
                        .entry(pair)
                        .or_default()
                        .insert(index1);
                    *removed_pair_counts.entry(pair).or_default() += 1;
                }

                

            }

            ReplacePairEffects {
                new_pair_locations,
                new_pair_count,
                removed_pair_locations,
                removed_pair_counts,
            }
        }

        while let Some(((id0, id1), count)) = pair_occurrences.pop() {
            if count < 2 {
                break;
            }
            let new_id = bpe.ids_to_tokens.len();
            bpe.add_id(TokenId(new_id), Token::Merge(id0, id1));

            let mut new_pair_locations_in_sequences = Vec::new();
            let mut new_pair_count = 0;

            for (pattern, pair_locations) in patterns
                .iter_mut()
                .zip(pair_locations_in_sequences.iter_mut())
            {
                let mut deregister_pair = |pair: &(TokenId, TokenId),
                                           first_index: usize,
                                           pair_locations: &mut IndexMap<
                    (TokenId, TokenId),
                    IndexSet<usize>,
                >| {
                    dbg!(pair);
                    assert!(pair_locations
                        .get_mut(pair)
                        .unwrap()
                        .swap_remove(&first_index));
                    if pair_locations.get(pair).unwrap().is_empty() {
                        assert!(pair_locations.swap_remove(pair).is_some());
                    }

                    let pair_count = *pair_occurrences.get_priority(pair).unwrap();
                    assert!(pair_count > 0);
                    if pair_count == 1 {
                        pair_occurrences.remove(pair);
                    } else {
                        pair_occurrences.set_priority(pair, pair_count - 1).unwrap();
                    }
                };

                // for the target pair: don't remove from pair_occurrences: it's already gone
                let mut deregister_target_pair = |pair: &(TokenId, TokenId),
                                                  first_index: usize,
                                                  pair_locations: &mut IndexMap<
                    (TokenId, TokenId),
                    IndexSet<usize>,
                >| {
                    assert!(pair_locations
                        .get_mut(pair)
                        .unwrap()
                        .swap_remove(&first_index));
                    if pair_locations.get(pair).unwrap().is_empty() {
                        assert!(pair_locations.swap_remove(pair).is_some());
                    }
                };

                let mut new_pair_locations = IndexSet::<usize>::new();

                println!("{pair_locations:?}");

                if !pair_locations.contains_key(&(id0, id1)) {
                    continue;
                }

                for index in pair_locations.get(&(id0, id1)).unwrap().clone() {
                    // for (temp?) borrow checker workaround: ensure entries in non-updating clone
                    // for this loop are still in live set
                    if !pair_locations.get(&(id0, id1)).unwrap().contains(&index) {
                        continue;
                    }
                    dbg!((id0, id1));

                    let prev_id = get_prev_id(pattern, index);
                    //let first_id = *pattern.get(index).unwrap();
                    let (second_id, second_index) = get_next_id(pattern, index).unwrap();
                    let next_id = get_next_id(pattern, second_index);

                    if let Some((id, i)) = prev_id {
                        deregister_pair(&(id, id0), i, pair_locations);
                    }

                    deregister_target_pair(&(id0, id1), index, pair_locations);

                    if let Some((id, i)) = next_id {
                        deregister_pair(&(id1, id), second_index, pair_locations);
                    }

                    *pattern.get_mut(index).unwrap() = TokenId(new_id);
                    *pattern.get_mut(second_index).unwrap() = TokenId(usize::MAX);

                    assert!(new_pair_locations.insert(index));
                }

                new_pair_count += new_pair_locations.len();
                new_pair_locations_in_sequences.push(new_pair_locations);
            }

            pair_occurrences.push((id0, id1), new_pair_count);

            //add new_pair_locations_in_sequences
            for (locations, new_locations) in pair_locations_in_sequences
                .iter_mut()
                .zip(new_pair_locations_in_sequences.into_iter())
            {
                println!("{:?}", (id0, id1));
                println!("{:?}", (&locations, &new_locations));
                assert!(locations.insert((id0, id1), new_locations).is_none());
            }

            /*
            replace all pair occurrences with merge token:
            for each occurrence
                identify overlapping pairs (usually 2, -1 for each edge hit)
                remove (up to)3 pairs: replaced + neighbors
                    fn: remove occurrence from pair priority queue (remove + decrement)
                insert new token (+ update info for next/prev token as needed)
            insert these new pairs into priority queue
            */
        }

        bpe
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
    fn test_bpe() {
        let bpe = Bpe::new(&[]);
        assert_eq!(
            bpe.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        assert_eq!(
            bpe.decode(vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]),
            vec![0x61, 0x62, 0x63]
        );

        let bpe = Bpe::new(&[&[0x61, 0x62, 0x63], &[0x64, 0x65, 0x66]]);
        assert_eq!(
            bpe.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        assert_eq!(
            bpe.decode(vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]),
            vec![0x61, 0x62, 0x63]
        );

        let bpe = Bpe::new(&[
            &[0x61, 0x62, 0x63],
            &[0x64, 0x65, 0x66],
            &[0x61, 0x62, 0x63],
        ]);
        assert_eq!(bpe.encode(&[0x61, 0x62, 0x63]), vec![TokenId(257)]);
        assert_eq!(bpe.decode(vec![TokenId(257)]), vec![0x61, 0x62, 0x63]);

        let bpe = Bpe::new(&[&[1, 2, 3, 2, 3, 4], &[1, 2, 3, 1, 2, 3]]);
        assert_eq!(
            bpe.encode(&[1, 2, 3, 2, 3, 4]),
            vec![TokenId(257), TokenId(256), TokenId(4)]
        );
        assert_eq!(
            bpe.decode(vec![TokenId(257), TokenId(256), TokenId(4)]),
            vec![1, 2, 3, 2, 3, 4]
        );
    }
}
