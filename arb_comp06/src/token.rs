use crate::pairs::ToPairs;
use crate::utils::{add_to_counts, increment};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TokenId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Token {
    Byte(u8),
    Merge(TokenId, TokenId),
}

/// Count pairs of adjacent `TokenIds` in `ids`.
fn count_id_pairs(ids: &[TokenId]) -> IndexMap<(TokenId, TokenId), usize> {
    let mut counts = IndexMap::new();

    let mut previous_id = Option::<TokenId>::None;
    let mut skip = false;

    for &id in ids {
        if let Some(prev) = previous_id {
            if prev == id {
                // In a contiguous subsequence of the same TokenId, skip every second pair.
                // This prevents overlapping instances from being counted.
                //
                // If this was not done, the pair count would be higher than the number of
                // replacements that would actually happen if the all pairs were replaced
                // by a new merge token.
                if !skip {
                    increment(&mut counts, (prev, id));
                }
                skip = !skip;
            } else {
                increment(&mut counts, (prev, id));
                // Any contiguous subsequence has ended, so reset the skip flag.
                skip = false;
            }
        }
        previous_id = Some(id);
    }

    counts
}

pub fn find_most_common_duplicate_id_pair<'a>(
    patterns: impl IntoIterator<Item = &'a Vec<TokenId>>,
) -> Option<((TokenId, TokenId), usize)> {
    let mut counts = IndexMap::new();

    for ids in patterns {
        add_to_counts(&mut counts, &count_id_pairs(ids));
    }

    counts
        .into_iter()
        .max_by_key(|&(_, count)| count)
        .filter(|&(_, count)| count > 1)
}

pub fn merge(
    iter: impl Iterator<Item = TokenId>,
    merge_if: impl Fn(TokenId, TokenId) -> Option<TokenId>,
) -> Vec<TokenId> {
    let mut result = vec![];

    let mut pairs = iter.pairs();
    while let Some((id0, id1)) = pairs.next() {
        if let Some(merged) = merge_if(id0, id1) {
            result.push(merged);
            pairs.next();
        } else {
            result.push(id0);
        }
    }

    if let Some(id) = pairs.final_item() {
        result.push(id);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_count_id_pairs() {
        let pattern = vec![];
        let result = count_id_pairs(&pattern);
        assert_eq!(result, IndexMap::<(TokenId, TokenId), usize>::new());

        let pattern = vec![TokenId(0)];
        let result = count_id_pairs(&pattern);
        assert_eq!(result, IndexMap::<(TokenId, TokenId), usize>::new());

        let pattern = vec![TokenId(0), TokenId(1)];
        let result = count_id_pairs(&pattern);
        assert_eq!(result, IndexMap::from([((TokenId(0), TokenId(1)), 1)]));

        let pattern = vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)];
        let result = count_id_pairs(&pattern);
        assert_eq!(
            result,
            IndexMap::from([
                ((TokenId(0), TokenId(1)), 1),
                ((TokenId(1), TokenId(2)), 1),
                ((TokenId(2), TokenId(3)), 1)
            ])
        );

        let pattern = vec![TokenId(0), TokenId(1), TokenId(0), TokenId(1)];
        let result = count_id_pairs(&pattern);
        assert_eq!(
            result,
            IndexMap::from([((TokenId(0), TokenId(1)), 2), ((TokenId(1), TokenId(0)), 1)])
        );

        let pattern = vec![TokenId(0), TokenId(0), TokenId(0)];
        let result = count_id_pairs(&pattern);
        assert_eq!(result, IndexMap::from([((TokenId(0), TokenId(0)), 1)]));

        let pattern = vec![TokenId(0), TokenId(0), TokenId(0), TokenId(0)];
        let result = count_id_pairs(&pattern);
        assert_eq!(result, IndexMap::from([((TokenId(0), TokenId(0)), 2)]));

        let pattern = vec![
            TokenId(1),
            TokenId(0),
            TokenId(0),
            TokenId(0),
            TokenId(1),
            TokenId(0),
            TokenId(0),
            TokenId(0),
            TokenId(1),
        ];
        let result = count_id_pairs(&pattern);
        assert_eq!(
            result,
            IndexMap::from([
                ((TokenId(1), TokenId(0)), 2),
                ((TokenId(0), TokenId(1)), 2),
                ((TokenId(0), TokenId(0)), 2)
            ])
        );
    }

    #[test]
    fn test_find_most_common_duplicate_id_pair() {
        let patterns = vec![];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, None);

        let patterns = vec![vec![]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, None);

        let patterns = vec![vec![TokenId(0)]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, None);

        let patterns = vec![vec![TokenId(0), TokenId(1)]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, None);

        let patterns = vec![vec![TokenId(0), TokenId(1)], vec![TokenId(0), TokenId(1)]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, Some(((TokenId(0), TokenId(1)), 2)));

        let patterns = vec![vec![TokenId(0), TokenId(1), TokenId(0), TokenId(1)], vec![]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, Some(((TokenId(0), TokenId(1)), 2)));

        let patterns = vec![vec![TokenId(0), TokenId(1), TokenId(0)], vec![TokenId(1)]];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, None);

        let patterns = vec![
            vec![TokenId(0), TokenId(1), TokenId(2)],
            vec![TokenId(1), TokenId(2)],
            vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)],
        ];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, Some(((TokenId(1), TokenId(2)), 3)));

        // when tied for max, returns last added (Iterator::max behavior)
        let patterns = vec![
            vec![TokenId(0), TokenId(1), TokenId(2)],
            vec![TokenId(1), TokenId(2)],
            vec![TokenId(0), TokenId(1)],
        ];
        let result = find_most_common_duplicate_id_pair(&patterns);
        assert_eq!(result, Some(((TokenId(1), TokenId(2)), 2)));
    }

    #[test]
    fn test_merge() {
        let merge_tester = |pattern: &[TokenId], id0: TokenId, id1: TokenId, merged: TokenId| {
            let f = |current_id, next_id| {
                if current_id == id0 && next_id == id1 {
                    Some(merged)
                } else {
                    None
                }
            };
            merge(pattern.iter().copied(), f)
        };

        let result = merge_tester(&vec![], TokenId(0), TokenId(1), TokenId(3));
        assert_eq!(result, vec![]);

        let pattern = vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)];

        let result = merge_tester(&pattern, TokenId(0), TokenId(1), TokenId(4));
        assert_eq!(result, vec![TokenId(4), TokenId(2), TokenId(3)]);

        let result = merge_tester(&pattern, TokenId(1), TokenId(2), TokenId(4));
        assert_eq!(result, vec![TokenId(0), TokenId(4), TokenId(3)]);

        let result = merge_tester(&pattern, TokenId(1), TokenId(0), TokenId(4));
        assert_eq!(result, vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)]);

        let result = merge_tester(&pattern, TokenId(0), TokenId(5), TokenId(4));
        assert_eq!(result, pattern);

        let result = merge_tester(
            &[TokenId(0), TokenId(0), TokenId(0), TokenId(0)],
            TokenId(0),
            TokenId(0),
            TokenId(1),
        );
        assert_eq!(result, vec![TokenId(1), TokenId(1),]);
    }
}
