use crate::recode::{condense, expand, to_bytes, to_ids};
use crate::token::{find_most_common_duplicate_id_pair, merge, Token, TokenId};
use indexmap::IndexMap;
use keyed_priority_queue::KeyedPriorityQueue;
use std::collections::{HashMap, HashSet};

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

    pub fn new_faster(data: &[&[u8]]) -> Self {
        let mut bpe = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
        };

        // Initialize the vocabulary with all possible bytes
        (0..=u8::MAX).for_each(|x| bpe.add_id(TokenId(x as usize), Token::Byte(x)));

        // Convert input data to patterns of TokenIds
        let mut patterns: Vec<Vec<TokenId>> = data.iter().map(|x| bpe.encode(x)).collect();

        // Map from pair to its count and the set of (pattern_idx, position) where it occurs
        type Pair = (TokenId, TokenId);
        type Occurrence = (usize, usize); // (pattern_idx, position)

        // Helper to count pairs and track their positions
        fn collect_pairs(
            patterns: &Vec<Vec<TokenId>>,
        ) -> (HashMap<Pair, usize>, HashMap<Pair, HashSet<Occurrence>>) {
            let mut counts = HashMap::new();
            let mut positions = HashMap::new();
            for (pat_idx, pattern) in patterns.iter().enumerate() {
                let mut i = 0;
                while i + 1 < pattern.len() {
                    let pair = (pattern[i], pattern[i + 1]);
                    *counts.entry(pair).or_insert(0) += 1;
                    positions.entry(pair).or_insert_with(HashSet::new).insert((pat_idx, i));
                    i += 1;
                }
            }
            (counts, positions)
        }

        let (mut pair_counts, mut pair_positions) = collect_pairs(&patterns);
        let mut queue = KeyedPriorityQueue::new();
        for (pair, count) in &pair_counts {
            if *count > 1 {
                queue.push(*pair, *count);
            }
        }

        while let Some((pair, _count)) = queue.pop() {
            // Only merge if the pair still exists and is frequent
            let count = pair_counts.get(&pair).copied().unwrap_or(0);
            if count < 2 {
                continue;
            }
            let (id0, id1) = pair;
            let new_id = TokenId(bpe.ids_to_tokens.len());
            bpe.add_id(new_id, Token::Merge(id0, id1));

            // All positions where this pair occurs
            let mut occs = match pair_positions.remove(&pair) {
                Some(x) => x,
                None => continue,
            };
            // To avoid double-replacement, sort and process right-to-left
            let mut occs_vec: Vec<_> = occs.into_iter().collect();
            occs_vec.sort_by_key(|&(pat_idx, pos)| (pat_idx, std::cmp::Reverse(pos)));
            // Collect affected pattern indices before consuming occs_vec
            let affected: HashSet<_> = occs_vec.iter().map(|(pat_idx, _)| *pat_idx).collect();
            for (pat_idx, pos) in occs_vec {
                let pattern = &mut patterns[pat_idx];
                // Check if the pair is still present at this position
                if pos + 1 >= pattern.len() || pattern[pos] != id0 || pattern[pos + 1] != id1 {
                    continue;
                }
                // Replace pair with new_id
                pattern.splice(pos..=pos + 1, [new_id]);
            }
            // Recompute pairs only in affected patterns
            for &pat_idx in &affected {
                let pattern = &patterns[pat_idx];
                let mut i = 0;
                while i + 1 < pattern.len() {
                    let pair = (pattern[i], pattern[i + 1]);
                    *pair_counts.entry(pair).or_insert(0) += 1;
                    pair_positions.entry(pair).or_insert_with(HashSet::new).insert((pat_idx, i));
                    if let Some(count) = pair_counts.get(&pair) {
                        if *count > 1 {
                            queue.push(pair, *count);
                        }
                    }
                    i += 1;
                }
            }
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
