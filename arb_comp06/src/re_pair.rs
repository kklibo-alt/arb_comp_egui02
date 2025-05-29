use crate::recode::{condense, expand, to_bytes, to_ids};
use crate::token::{Token, TokenId};
use crate::utils::{decrease_priorities, increase_priorities, MappedSets};
use indexmap::{IndexMap, IndexSet};
use keyed_priority_queue::KeyedPriorityQueue;

pub struct RePair {
    ids_to_tokens: IndexMap<TokenId, Token>,
    tokens_to_ids: IndexMap<Token, TokenId>,
    pub init_in_progress: Option<InitInProgress>,
}

pub struct InitInProgress {
    patterns: Vec<Vec<TokenId>>,
    pair_locations_in_patterns: Vec<MappedSets>,
    pair_counts: KeyedPriorityQueue<(TokenId, TokenId), usize>,
}

impl RePair {
    fn add_id(&mut self, id: TokenId, token: Token) {
        self.ids_to_tokens.insert(id, token);
        self.tokens_to_ids.insert(token, id);
    }

    /// For each token id pair in `ids`: record the index of its first element.
    fn record_id_pairs(ids: &[TokenId]) -> MappedSets {
        ids.windows(2)
            .enumerate()
            .map(|(i, ids)| ((ids[0], ids[1]), i))
            .collect()
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

    /// Note: when this function is called, this pair has already been removed from
    /// pair locations and counts.
    fn replace_pair(
        id0: TokenId,
        id1: TokenId,
        mut locations: IndexSet<usize>,
        pattern: &mut [TokenId],
        replacement: TokenId,
    ) -> (MappedSets, MappedSets) {
        let mut added_pair_locations = MappedSets::new();
        let mut removed_pair_locations = MappedSets::new();

        while let Some(index0) = locations.pop() {
            assert_eq!(Some(&id0), pattern.get(index0));

            let (token_id1, index1) = Self::get_next_id(pattern, index0).unwrap();
            assert_eq!(id1, token_id1);

            let prev_token = Self::get_prev_id(pattern, index0);
            let next_token = Self::get_next_id(pattern, index1);

            if let Some((prev_id, prev_index)) = prev_token {
                if (prev_id, id0) == (id0, id1) {
                    assert!(locations.swap_remove(&prev_index));
                } else {
                    removed_pair_locations.insert((prev_id, id0), prev_index);
                }
                added_pair_locations.insert((prev_id, replacement), prev_index);
            }

            if let Some((next_id, _next_index)) = next_token {
                if (id1, next_id) == (id0, id1) {
                    assert!(locations.swap_remove(&index1));
                } else {
                    removed_pair_locations.insert((id1, next_id), index1);
                }

                added_pair_locations.insert((replacement, next_id), index0);
            }

            *pattern.get_mut(index0).unwrap() = replacement;
            *pattern.get_mut(index1).unwrap() = TokenId(usize::MAX);
        }

        (added_pair_locations, removed_pair_locations)
    }

    pub fn new(data: &[&[u8]]) -> Self {
        let mut re_pair = Self::new_iterative(data);

        while re_pair.init_in_progress.is_some() {
            re_pair.init_step(None::<fn(usize)>);
        }

        re_pair
    }

    pub fn new_iterative(data: &[&[u8]]) -> Self {
        let mut re_pair = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
            init_in_progress: None,
        };

        (0..=u8::MAX).for_each(|x| re_pair.add_id(TokenId(x as usize), Token::Byte(x)));

        let patterns: Vec<Vec<TokenId>> = data.iter().map(|x| re_pair.encode(x)).collect();

        let pair_locations_in_patterns: Vec<MappedSets> = patterns
            .iter()
            .map(|pattern| Self::record_id_pairs(pattern))
            .collect();

        let mut pair_counts: KeyedPriorityQueue<(TokenId, TokenId), usize> =
            KeyedPriorityQueue::new();

        increase_priorities(
            &mut pair_counts,
            pair_locations_in_patterns.iter().flat_map(|x| x.lengths()),
        );

        re_pair.init_in_progress = Some(InitInProgress {
            patterns,
            pair_locations_in_patterns,
            pair_counts,
        });
        re_pair
    }

    // first attempt: ignore token repetition block overcounting for now
    pub fn init_step(&mut self, new_id_callback: Option<impl Fn(usize)>) {
        if let Some(mut init_in_progress) = self.init_in_progress.take() {
            let patterns = &mut init_in_progress.patterns;
            let pair_locations_in_patterns = &mut init_in_progress.pair_locations_in_patterns;
            let pair_counts = &mut init_in_progress.pair_counts;

            if let Some(((id0, id1), count)) = pair_counts.pop() {
                if count >= 2 {
                    let new_id = TokenId(self.ids_to_tokens.len());
                    self.add_id(new_id, Token::Merge(id0, id1));

                    for (pattern, pair_locations) in patterns
                        .iter_mut()
                        .zip(pair_locations_in_patterns.iter_mut())
                    {
                        if let Some(locations) = pair_locations.0.swap_remove(&(id0, id1)) {
                            let (added_pair_locations, removed_pair_locations) =
                                Self::replace_pair(id0, id1, locations, pattern, new_id);

                            increase_priorities(pair_counts, added_pair_locations.lengths());
                            decrease_priorities(pair_counts, removed_pair_locations.lengths());

                            *pair_locations += added_pair_locations;
                            *pair_locations -= removed_pair_locations;
                        }
                    }
                    self.init_in_progress = Some(init_in_progress);
                }
            }
        }
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

        let re_pair = RePair::new(&[&[1, 2, 3, 2, 3, 4], &[1, 2, 3, 1, 2, 3]]);
        assert_eq!(
            re_pair.encode(&[1, 2, 3, 2, 3, 4]),
            vec![TokenId(257), TokenId(256), TokenId(4)]
        );
        assert_eq!(
            re_pair.decode(vec![TokenId(257), TokenId(256), TokenId(4)]),
            vec![1, 2, 3, 2, 3, 4]
        );
    }

    #[test]
    fn test_repeating_zeros() {
        let re_pair = RePair::new(&[&[0, 0, 0]]);
        assert_eq!(
            re_pair.encode(&[0, 0, 0, 0]),
            vec![TokenId(256), TokenId(256),]
        );
    }

    #[test]
    #[ignore = "doesn't work yet"]
    fn test_repeating_blocks() {
        let re_pair = RePair::new(&[&[1, 2, 0, 0, 0, 1, 2, 0, 0, 0, 1, 2]]);
        assert_eq!(
            re_pair.encode(&[1, 2, 0, 0]),
            vec![TokenId(256), TokenId(257),]
        );
    }
}
