use crate::token::{self, Token, TokenId};
use indexmap::{IndexMap, IndexSet};

pub fn condense(
    pattern: Vec<TokenId>,
    merge_if: impl Fn(TokenId, TokenId) -> Option<TokenId>,
) -> Vec<TokenId> {
    let mut result = pattern;

    loop {
        let merged = token::merge(result.iter().copied(), &merge_if);
        if result.len() == merged.len() {
            break;
        }
        result = merged;
    }

    result
}

pub fn condense_and_collect(
    pattern: Vec<TokenId>,
    merge_if: impl Fn(TokenId, TokenId) -> Option<TokenId>,
) -> Vec<TokenId> {
    let mut result = pattern;
    let mut meta_result = vec![];

    loop {
        let merged = token::merge(result.iter().copied(), &merge_if);
        if result.len() == merged.len() {
            break;
        }
        meta_result.extend(merged.clone());
        result = merged;
    }

    meta_result
}

pub fn expand(pattern: Vec<TokenId>, ids_to_tokens: &IndexMap<TokenId, Token>) -> Vec<TokenId> {
    let mut result = pattern;

    loop {
        let mut unmerged = vec![];

        for id in result.iter() {
            match ids_to_tokens.get(id) {
                Some(Token::Merge(id0, id1)) => {
                    unmerged.push(*id0);
                    unmerged.push(*id1);
                }
                Some(Token::Byte(_)) => unmerged.push(*id),
                None => panic!("TokenId not in encoded set"),
            }
        }

        if result.len() == unmerged.len() {
            break;
        }
        result = unmerged;
    }

    result
}

pub fn range(
    pattern: Vec<TokenId>,
    ids_to_tokens: &IndexMap<TokenId, Token>,
    tokens_to_ids: &IndexMap<Token, TokenId>,
) -> IndexSet<TokenId> {
    let mut result = IndexSet::new();

    let expanded = expand(pattern.clone(), ids_to_tokens);
    result.extend(expanded.clone());
    result.extend(condense_and_collect(expanded, |id0, id1| {
        tokens_to_ids.get(&Token::Merge(id0, id1)).copied()
    }));
    result
}

pub fn to_ids(data: &[u8], tokens_to_ids: &IndexMap<Token, TokenId>) -> Vec<TokenId> {
    let result: Vec<TokenId> = data
        .iter()
        .map(|&x| {
            *tokens_to_ids
                .get(&Token::Byte(x))
                .expect("all byte values should be encoded")
        })
        .collect();
    result
}

pub fn to_bytes(data: &[TokenId], ids_to_tokens: &IndexMap<TokenId, Token>) -> Vec<u8> {
    let result = data;
    result
        .iter()
        .map(|id| match ids_to_tokens.get(id) {
            Some(Token::Byte(b)) => *b,
            _ => panic!("internal decoding error"),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use rand::RngCore;

    use super::*;
    use crate::bpe::Bpe;
    use crate::test_utils::print_tokens;

    #[test]
    fn test1() {
        let bpe = Bpe::new(&[&[1, 2, 1, 2, 3, 4, 5, 6]]);
        let range = range(
            bpe.encode(&[1, 2, 3, 4, 5, 6]),
            &bpe.ids_to_tokens(),
            &bpe.tokens_to_ids(),
        );
        println!("{:?}", range);
    }

    fn f(pattern1: Vec<TokenId>, pattern2: Vec<TokenId>, bpe: &Bpe) -> Vec<TokenId> {
        let range = range(pattern2.clone(), &bpe.ids_to_tokens(), &bpe.tokens_to_ids());
        println!("{:?}", range);

        let range_vec = range.iter().copied().collect::<Vec<_>>();
        print_tokens(range_vec, bpe, |_id| false);

        let merge_if = |id0: TokenId, id1: TokenId| -> Option<TokenId> {
            bpe.tokens_to_ids()
                .get(&Token::Merge(id0, id1))
                .filter(|_| range.contains(&id0) == range.contains(&id1))
                .copied()
        };

        //            let merge_if = |id0: &TokenId, id1: &TokenId| -> bool { true };

        let e = expand(pattern1.clone(), &bpe.ids_to_tokens());
        println!("{:?}", e);

        let c = condense(e.clone(), merge_if);
        println!("{:?}", c);

        print_tokens(pattern1.clone(), &bpe, |id| !range.contains(id));
        print_tokens(e, &bpe, |id| !range.contains(id));
        print_tokens(c.clone(), &bpe, |id| !range.contains(id));

        c
    }

    #[test]
    fn test2() {
        let mut pattern1 = [0u8; 24];
        rand::thread_rng().fill_bytes(&mut pattern1);
        let pattern1 = &pattern1;

        let mut pattern2 = pattern1.clone();
        rand::thread_rng().fill_bytes(&mut pattern2[8..16]);
        let _pattern2 = &pattern2;

        let pattern1 = &[
            235, 4, 39, 149, 209, 252, 162, 130, 117, 122, 38, 174, 226, 121, 100, 248, 135, 230,
            143, 77, 249, 132, 163, 72,
        ];
        let pattern2 = &[
            235, 4, 39, 149, 209, 252, 162, 130, 254, 154, 20, 59, 241, 62, 200, 155, 135, 230,
            143, 77, 249, 132, 163, 72,
        ];

        let bpe = Bpe::new(&[pattern1, pattern2]);
        let pattern1 = bpe.encode(pattern1);
        let pattern2 = bpe.encode(pattern2);

        let c1 = f(pattern1.clone(), pattern2.clone(), &bpe);
        let c2 = f(pattern2, pattern1, &bpe);

        println!("{}", serde_json::to_string(&c1).unwrap());
        let expected =
            serde_json::from_str::<Vec<TokenId>>("[269,117,122,38,174,226,121,100,248,262]")
                .unwrap();
        assert_eq!(c1, expected);

        println!("{}", serde_json::to_string(&c2).unwrap());
        let expected =
            serde_json::from_str::<Vec<TokenId>>("[269,254,154,20,59,241,62,200,155,262]").unwrap();
        assert_eq!(c2, expected);
    }
}
