//! Techniques for comparing patterns

#[cfg(test)]
mod tests {

    use crate::bpe::Bpe;
    use crate::matcher;
    use crate::recode::{condense, expand, range};
    use crate::test_utils::{self, print_tokens};
    use crate::token::{Token, TokenId};

    #[test]
    fn technique01() {
        // Technique 1: to show pattern1 in terms of pattern2 diff
        // 1. train a BPE model on both patterns
        // 2. encode both patterns
        // 3. get the range of all ids in and "reachable from" pattern2
        // 4. expand pattern1 to its representation with smallest tokens (just bytes)
        // 5. condense the result by merging tokens that are purely in or out of the pattern2 range
        // 6. print the result with highlighting based on whether the token is in or out of the pattern2 range

        let file1 = "Hello 12345 World";
        let file2 = "Hello World";

        let bpe = Bpe::new(&[file1.as_bytes(), file2.as_bytes()]);

        let pattern1 = bpe.encode(file1.as_bytes());
        let pattern2 = bpe.encode(file2.as_bytes());

        let in_pattern2 = range(pattern2.clone(), bpe.ids_to_tokens(), bpe.tokens_to_ids());

        let merge_if = |id0: TokenId, id1: TokenId| -> Option<TokenId> {
            bpe.tokens_to_ids()
                .get(&Token::Merge(id0, id1))
                .filter(|_| in_pattern2.contains(&id0) == in_pattern2.contains(&id1))
                .copied()
        };

        let e = expand(pattern1.clone(), bpe.ids_to_tokens());
        let c = condense(e.clone(), merge_if);

        print_tokens(c, &bpe, |id| !in_pattern2.contains(id));
    }

    #[test]
    fn technique02() {
        // Technique 2:
        // 1. train a BPE model on both patterns
        // 2. encode both patterns
        // 3. use greedy00 matcher to get matched token lists
        // 4. print the result in columns with aligned matched token blocks

        let file1 = "aJAOA1pjSAwCr9CkW3FE7166ch/309iOkW3FRa+1ch/30WIYjbT";
        let file2 = "aJAOA1pjSAwCr9CkW3kkZMFE7166ch/309iORa+1ch/30WkkZMIYjbT";

        let bpe = Bpe::new(&[file1.as_bytes(), file2.as_bytes()]);

        let pattern1 = bpe.encode(file1.as_bytes());
        let pattern2 = bpe.encode(file2.as_bytes());

        let matches = matcher::greedy00(&pattern1, &pattern2);

        test_utils::print_ui_01(&matches, |x| bpe.decode(x.clone()), true);

        //redundant output
        println!("print_ui_02 test");
        let (cells0, cells1) = test_utils::matches_to_cells(&matches, |x| bpe.decode(x.clone()));
        test_utils::print_ui_02(&cells0, &cells1);
    }
}
