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

        // Return early if no data to process
        if data.is_empty() {
            return bpe;
        }

        // Convert data to token IDs
        let mut patterns = data.iter().map(|x| bpe.encode(x)).collect::<Vec<_>>();
        
        // Track valid positions and initialize pair tracking structures
        let mut valid_positions = patterns.iter().map(|p| vec![true; p.len()]).collect::<Vec<_>>();
        let mut pair_locations: IndexMap<(TokenId, TokenId), Vec<(usize, usize)>> = IndexMap::new();
        let mut all_pairs = KeyedPriorityQueue::new();

        // Collect initial pairs and their occurrences
        for (pattern_idx, pattern) in patterns.iter().enumerate() {
            for i in 0..pattern.len().saturating_sub(1) {
                let pair = (pattern[i], pattern[i + 1]);
                pair_locations.entry(pair).or_default().push((pattern_idx, i));
            }
        }

        // Add pairs with multiple occurrences to the priority queue
        for (pair, locations) in &pair_locations {
            if locations.len() > 1 {
                all_pairs.push(*pair, locations.len());
            }
        }

        // Main BPE loop: iteratively merge the most frequent pair
        while let Some((pair, count)) = all_pairs.pop() {
            if count <= 1 {
                break;
            }

            // Create new token from the pair
            let (id0, id1) = pair;
            let new_id = TokenId(bpe.ids_to_tokens.len());
            bpe.add_id(new_id, Token::Merge(id0, id1));

            // Process all occurrences of this pair
            let locations = pair_locations.remove(&pair).unwrap_or_default();
            let mut positions_to_update = Vec::new();
            let mut overlapping_pairs: IndexMap<(TokenId, TokenId), Vec<(usize, usize)>> = IndexMap::new();

            // Apply merges and track affected positions
            for &(pattern_idx, pos) in &locations {
                // Skip invalid positions
                if pos + 1 >= patterns[pattern_idx].len() || 
                   !valid_positions[pattern_idx][pos] ||
                   !valid_positions[pattern_idx][pos + 1] ||
                   patterns[pattern_idx][pos] != id0 ||
                   patterns[pattern_idx][pos + 1] != id1 {
                    continue;
                }

                // Track overlapping pairs
                if pos > 0 && valid_positions[pattern_idx][pos - 1] {
                    let prev_pair = (patterns[pattern_idx][pos - 1], id0);
                    overlapping_pairs.entry(prev_pair).or_default().push((pattern_idx, pos - 1));
                }
                
                if pos + 2 < patterns[pattern_idx].len() && valid_positions[pattern_idx][pos + 2] {
                    let next_pair = (id1, patterns[pattern_idx][pos + 2]);
                    overlapping_pairs.entry(next_pair).or_default().push((pattern_idx, pos + 1));
                }

                // Apply the merge
                patterns[pattern_idx][pos] = new_id;
                valid_positions[pattern_idx][pos + 1] = false;

                // Track positions that need new pairs to be formed
                if pos > 0 && valid_positions[pattern_idx][pos - 1] {
                    positions_to_update.push((pattern_idx, pos - 1));
                }
                if pos + 2 < patterns[pattern_idx].len() && valid_positions[pattern_idx][pos + 2] {
                    positions_to_update.push((pattern_idx, pos));
                }
            }

            // Decrement counts of overlapping pairs
            for (overlap_pair, positions) in overlapping_pairs {
                if let Some(existing_locations) = pair_locations.get_mut(&overlap_pair) {
                    // Remove overlapping positions
                    for &pos in &positions {
                        if let Some(idx) = existing_locations.iter().position(|&p| p == pos) {
                            existing_locations.swap_remove(idx);
                        }
                    }
                    
                    // Update queue or remove pair if needed
                    if existing_locations.is_empty() {
                        pair_locations.remove(&overlap_pair);
                        all_pairs.remove(&overlap_pair);
                    } else {
                        let new_count = existing_locations.len();
                        if new_count <= 1 {
                            all_pairs.remove(&overlap_pair);
                        } else if all_pairs.get_priority(&overlap_pair).is_some() {
                            all_pairs.set_priority(&overlap_pair, new_count);
                        }
                    }
                }
            }

            // Form new pairs after merges
            for (pattern_idx, pos) in positions_to_update {
                // Find the next valid position
                let next_valid_pos = if pos + 1 < valid_positions[pattern_idx].len() && valid_positions[pattern_idx][pos + 1] {
                    pos + 1
                } else if pos + 2 < valid_positions[pattern_idx].len() && valid_positions[pattern_idx][pos + 2] {
                    pos + 2
                } else {
                    continue;
                };

                // Create new pair
                let new_pair = (patterns[pattern_idx][pos], patterns[pattern_idx][next_valid_pos]);
                
                // Update pair locations and priority queue
                if let Some(locations) = pair_locations.get_mut(&new_pair) {
                    locations.push((pattern_idx, pos));
                    
                    let new_count = locations.len();
                    if new_count > 1 {
                        match all_pairs.entry(new_pair) {
                            Entry::Occupied(_) => { all_pairs.set_priority(&new_pair, new_count); },
                            Entry::Vacant(entry) => { entry.set_priority(new_count); },
                        }
                    }
                } else {
                    let mut locations = Vec::new();
                    locations.push((pattern_idx, pos));
                    pair_locations.insert(new_pair, locations);
                }
            }
        }

        // Create final patterns by removing invalid positions
        let patterns = patterns
            .iter()
            .enumerate()
            .map(|(i, pattern)| {
                pattern
                    .iter()
                    .zip(valid_positions[i].iter())
                    .filter_map(|(&token, &valid)| valid.then_some(token))
                    .collect()
            })
            .collect::<Vec<Vec<TokenId>>>();

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
