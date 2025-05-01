use crate::recode::{condense, expand, to_bytes, to_ids};
use crate::token::{find_most_common_duplicate_id_pair, merge, Token, TokenId};
use indexmap::IndexMap;
use keyed_priority_queue::{KeyedPriorityQueue, Entry};
use std::collections::HashMap;

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

    // faster implementation using keyed_priority_queue for the Re-Pair algorithm
    pub fn new_faster(data: &[&[u8]]) -> Self {
        let mut bpe = Self {
            ids_to_tokens: IndexMap::new(),
            tokens_to_ids: IndexMap::new(),
        };

        // Initialize with byte tokens
        (0..=u8::MAX).for_each(|x| bpe.add_id(TokenId(x as usize), Token::Byte(x)));

        // Convert all data to token IDs
        let mut patterns = data.iter().map(|x| bpe.encode(x)).collect::<Vec<_>>();
        if patterns.is_empty() {
            return bpe;
        }

        // Build initial pair counts using a IndexMap to maintain insertion order
        let mut pair_locations: IndexMap<(TokenId, TokenId), Vec<(usize, usize)>> = IndexMap::new();
        let mut all_pairs = KeyedPriorityQueue::new();
        
        // Track valid positions in each pattern
        let mut valid_positions: Vec<Vec<bool>> = patterns
            .iter()
            .map(|pattern| vec![true; pattern.len()])
            .collect();

        // First pass: collect all pairs and their locations
        for (pattern_idx, pattern) in patterns.iter().enumerate() {
            if pattern.len() < 2 {
                continue;
            }

            for i in 0..pattern.len() - 1 {
                let pair = (pattern[i], pattern[i + 1]);
                pair_locations
                    .entry(pair)
                    .or_default()
                    .push((pattern_idx, i));
            }
        }

        // Initialize the priority queue with pair counts
        for (pair, locations) in &pair_locations {
            if locations.len() > 1 {
                all_pairs.push(*pair, locations.len());
            }
        }

        // Main loop: find the most frequent pair and merge it
        while let Some((pair, count)) = all_pairs.pop() {
            if count <= 1 {
                break;
            }

            let (id0, id1) = pair;
            let new_id = TokenId(bpe.ids_to_tokens.len());
            bpe.add_id(new_id, Token::Merge(id0, id1));

            // Apply the merge to all patterns
            let mut locations_to_update = Vec::new();
            let locations = pair_locations.remove(&pair).unwrap_or_default();

            for &(pattern_idx, pos) in &locations {
                // Skip if positions are no longer valid or don't match the pair
                if pos + 1 >= patterns[pattern_idx].len() || 
                   !valid_positions[pattern_idx][pos] ||
                   !valid_positions[pattern_idx][pos + 1] ||
                   patterns[pattern_idx][pos] != id0 ||
                   patterns[pattern_idx][pos + 1] != id1 {
                    continue;
                }

                // Apply the merge: replace first token with new_id and mark second token as invalid
                patterns[pattern_idx][pos] = new_id;
                valid_positions[pattern_idx][pos + 1] = false;

                // Collect positions that need updating
                if pos > 0 && valid_positions[pattern_idx][pos - 1] {
                    locations_to_update.push((pattern_idx, pos - 1));
                }
                if pos + 2 < patterns[pattern_idx].len() && valid_positions[pattern_idx][pos + 2] {
                    locations_to_update.push((pattern_idx, pos));
                }
            }

            // Update frequencies of affected pairs
            for (pattern_idx, pos) in locations_to_update {
                // Find the next valid position after pos
                let next_pos = pos + 1;
                let next_valid_pos = if next_pos < valid_positions[pattern_idx].len() && valid_positions[pattern_idx][next_pos] {
                    next_pos
                } else if next_pos + 1 < valid_positions[pattern_idx].len() && valid_positions[pattern_idx][next_pos + 1] {
                    next_pos + 1
                } else {
                    continue; // No valid next position
                };

                let left_id = patterns[pattern_idx][pos];
                let right_id = patterns[pattern_idx][next_valid_pos];
                let new_pair = (left_id, right_id);

                // Update pair locations
                if let Some(locations) = pair_locations.get_mut(&new_pair) {
                    // Add this position to locations
                    locations.push((pattern_idx, pos));
                    
                    // Update count in priority queue
                    match all_pairs.entry(new_pair) {
                        Entry::Occupied(_) => {
                            all_pairs.set_priority(&new_pair, locations.len());
                        }
                        Entry::Vacant(entry) => {
                            if locations.len() > 1 {
                                entry.set_priority(locations.len());
                            }
                        }
                    }
                } else {
                    // Add new pair
                    let mut new_locations = Vec::new();
                    new_locations.push((pattern_idx, pos));
                    let count = new_locations.len();
                    pair_locations.insert(new_pair, new_locations);
                    
                    if count > 1 {
                        all_pairs.push(new_pair, count);
                    }
                }
            }
        }

        // Compress the patterns by removing invalid positions
        let final_patterns: Vec<Vec<TokenId>> = patterns
            .iter()
            .enumerate()
            .map(|(i, pattern)| {
                pattern
                    .iter()
                    .zip(valid_positions[i].iter())
                    .filter_map(|(token, &valid)| if valid { Some(*token) } else { None })
                    .collect()
            })
            .collect();

        // Update the patterns for any final processing
        patterns = final_patterns;

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

    #[test]
    fn test_bpe_faster() {
        // Test empty data
        let bpe = Bpe::new_faster(&[]);
        assert_eq!(
            bpe.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        
        // Test simple data
        let bpe = Bpe::new_faster(&[&[0x61, 0x62, 0x63], &[0x64, 0x65, 0x66]]);
        assert_eq!(
            bpe.encode(&[0x61, 0x62, 0x63]),
            vec![TokenId(0x61), TokenId(0x62), TokenId(0x63)]
        );
        
        // Test repeating patterns
        let bpe = Bpe::new_faster(&[
            &[0x61, 0x62, 0x63],
            &[0x64, 0x65, 0x66],
            &[0x61, 0x62, 0x63],
        ]);
        assert_eq!(bpe.encode(&[0x61, 0x62, 0x63]), vec![TokenId(257)]);
        assert_eq!(bpe.decode(vec![TokenId(257)]), vec![0x61, 0x62, 0x63]);
        
        // Test complex pattern
        let bpe = Bpe::new_faster(&[&[1, 2, 3, 2, 3, 4], &[1, 2, 3, 1, 2, 3]]);
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
