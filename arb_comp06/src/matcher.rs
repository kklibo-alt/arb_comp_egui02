use crate::token::TokenId;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Matched {
    Same(Vec<TokenId>),
    Diff(Vec<TokenId>, Vec<TokenId>),
}

fn find_next(target: TokenId, start_index: usize, search_in: &[TokenId]) -> Option<usize> {
    for (i, &id) in search_in.iter().enumerate().skip(start_index) {
        if target == id {
            return Some(i);
        }
    }
    None
}

fn next_match_point(
    start_index0: usize,
    start_index1: usize,
    ids0: &[TokenId],
    ids1: &[TokenId],
) -> Option<(usize, usize)> {
    let mut index0 = start_index0;
    let mut index1 = start_index1;

    loop {
        if index0 >= ids0.len() {
            return None;
        }
        if index1 >= ids1.len() {
            return None;
        }

        let next0 = find_next(ids1[index1], index0, ids0);
        let next1 = find_next(ids0[index0], index1, ids1);
        match (next0, next1) {
            (Some(next0), Some(next1)) => {
                if next0 <= next1 {
                    return Some((next0, index1));
                } else {
                    return Some((index0, next1));
                }
            }
            (Some(next0), None) => {
                return Some((next0, index1));
            }
            (None, Some(next1)) => {
                return Some((index0, next1));
            }
            (None, None) => {
                index0 += 1;
                index1 += 1;
            }
        }
    }
}

fn consume_to_diff(
    ids0: &[TokenId],
    start0: usize,
    end0: usize,
    ids1: &[TokenId],
    start1: usize,
    end1: usize,
) -> Option<Matched> {
    let ids0 = ids0[start0..end0].to_vec();
    let ids1 = ids1[start1..end1].to_vec();

    if ids0.is_empty() && ids1.is_empty() {
        None
    } else {
        Some(Matched::Diff(ids0, ids1))
    }
}

fn consume_to_same(
    ids0: &[TokenId],
    start0: &mut usize,
    ids1: &[TokenId],
    start1: &mut usize,
) -> Matched {
    let mut ids = vec![];

    while (*start0 < ids0.len()) && (*start1 < ids1.len()) && (ids0[*start0] == ids1[*start1]) {
        ids.push(ids0[*start0]);
        *start0 += 1;
        *start1 += 1;
    }

    Matched::Same(ids)
}

pub fn greedy00(ids0: &[TokenId], ids1: &[TokenId]) -> Vec<Matched> {
    let mut matches = vec![];

    let mut index0 = 0;
    let mut index1 = 0;

    while let Some((next0, next1)) = next_match_point(index0, index1, ids0, ids1) {
        if let Some(m) = consume_to_diff(ids0, index0, next0, ids1, index1, next1) {
            matches.push(m);
        }

        index0 = next0;
        index1 = next1;

        let m = consume_to_same(ids0, &mut index0, ids1, &mut index1);
        matches.push(m);
    }

    if let Some(m) = consume_to_diff(ids0, index0, ids0.len(), ids1, index1, ids1.len()) {
        matches.push(m);
    }

    matches
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_next() {
        let ids = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        assert_eq!(find_next(TokenId(4), 0, &ids), Some(4));
        assert_eq!(find_next(TokenId(4), 5, &ids), None);
        assert_eq!(find_next(TokenId(10), 0, &ids), None);
    }

    #[test]
    fn test_next_match_point_01() {
        let ids0 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        let ids1 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        assert_eq!(next_match_point(0, 0, &ids0, &ids1), Some((0, 0)));
        assert_eq!(next_match_point(0, 1, &ids0, &ids1), Some((1, 1)));
        assert_eq!(next_match_point(0, 2, &ids0, &ids1), Some((2, 2)));
        assert_eq!(next_match_point(0, 3, &ids0, &ids1), Some((3, 3)));
        assert_eq!(next_match_point(0, 4, &ids0, &ids1), Some((4, 4)));
        assert_eq!(next_match_point(0, 5, &ids0, &ids1), Some((5, 5)));
        assert_eq!(next_match_point(0, 6, &ids0, &ids1), Some((6, 6)));
        assert_eq!(next_match_point(0, 7, &ids0, &ids1), None);
    }

    #[test]
    fn test_next_match_point_02() {
        let ids0 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        let ids1 = vec![
            TokenId(3),
            TokenId(4),
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(5),
            TokenId(6),
        ];
        assert_eq!(next_match_point(0, 0, &ids0, &ids1), Some((0, 2)));
        assert_eq!(next_match_point(1, 3, &ids0, &ids1), Some((1, 3)));
        assert_eq!(next_match_point(2, 4, &ids0, &ids1), Some((2, 4)));
        assert_eq!(next_match_point(3, 5, &ids0, &ids1), Some((5, 5)));
        assert_eq!(next_match_point(6, 6, &ids0, &ids1), Some((6, 6)));
        assert_eq!(next_match_point(7, 7, &ids0, &ids1), None);
    }

    #[test]
    fn test_greedy00_01() {
        let ids0 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        let ids1 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        let matches = greedy00(&ids0, &ids1);
        assert_eq!(
            matches,
            vec![Matched::Same(vec![
                TokenId(0),
                TokenId(1),
                TokenId(2),
                TokenId(3),
                TokenId(4),
                TokenId(5),
                TokenId(6)
            ])]
        );
    }

    #[test]
    fn test_greedy00_02() {
        let ids0 = vec![
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(3),
            TokenId(4),
            TokenId(5),
            TokenId(6),
        ];
        let ids1 = vec![
            TokenId(3),
            TokenId(4),
            TokenId(0),
            TokenId(1),
            TokenId(2),
            TokenId(7),
            TokenId(5),
            TokenId(6),
        ];
        let matches = greedy00(&ids0, &ids1);
        assert_eq!(
            matches,
            vec![
                Matched::Diff(vec![], vec![TokenId(3), TokenId(4)]),
                Matched::Same(vec![TokenId(0), TokenId(1), TokenId(2)]),
                Matched::Diff(vec![TokenId(3), TokenId(4)], vec![TokenId(7)]),
                Matched::Same(vec![TokenId(5), TokenId(6)])
            ]
        );
    }

    #[test]
    fn test_greedy00_03() {
        let ids0 = vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)];
        let ids1 = vec![TokenId(4), TokenId(5), TokenId(6)];
        let matches = greedy00(&ids0, &ids1);
        assert_eq!(
            matches,
            vec![Matched::Diff(
                vec![TokenId(0), TokenId(1), TokenId(2), TokenId(3)],
                vec![TokenId(4), TokenId(5), TokenId(6)]
            )]
        );
    }

    #[test]
    fn test_greedy00_04() {
        let ids0 = vec![];
        let ids1 = vec![];
        let matches = greedy00(&ids0, &ids1);
        assert_eq!(matches, vec![]);
    }
}
