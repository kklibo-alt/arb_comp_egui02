use crate::matcher::Matched;
use crate::test_utils::{hex_cells, HexCell};
use crate::token::TokenId;
use rangemap::RangeMap;

pub struct AlignedCells {
    matches: Vec<Matched>,
    cells0_index_to_matches_index: RangeMap<usize, usize>,
    cells1_index_to_matches_index: RangeMap<usize, usize>,
    matches_index_to_cells0_alignment_offset: Vec<usize>,
    matches_index_to_cells1_alignment_offset: Vec<usize>,
}

impl AlignedCells {
    pub fn new(
        matches: Vec<Matched>,
        decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
    ) -> (Vec<HexCell>, Vec<HexCell>, Self) {
        let mut cells0 = vec![];
        let mut cells1 = vec![];
        let mut cells0_index_to_matches_index = RangeMap::new();
        let mut cells1_index_to_matches_index = RangeMap::new();
        let mut matches_index_to_cells0_alignment_offset = vec![];
        let mut matches_index_to_cells1_alignment_offset = vec![];

        let mut index0 = 0;
        let mut index1 = 0;
        let mut offset0 = 0;
        let mut offset1 = 0;

        matches
            .iter()
            .enumerate()
            .for_each(|(match_index, matched)| match matched {
                Matched::Same(ids) => {
                    let mut block_len = 0;
                    for &id in ids {
                        let mut new_cells = hex_cells(false, id, &decode);
                        block_len += new_cells.len();
                        cells0.append(&mut new_cells.clone());
                        cells1.append(&mut new_cells);
                    }

                    cells0_index_to_matches_index.insert(index0..index0 + block_len, match_index);
                    cells1_index_to_matches_index.insert(index1..index1 + block_len, match_index);

                    matches_index_to_cells0_alignment_offset.push(offset0);
                    matches_index_to_cells1_alignment_offset.push(offset1);

                    index0 += block_len;
                    index1 += block_len;
                }
                Matched::Diff(ids0, ids1) => {
                    let mut block_cells0 = vec![];
                    let mut block_cells1 = vec![];

                    for &id in ids0 {
                        block_cells0.append(&mut hex_cells(true, id, &decode));
                    }
                    for &id in ids1 {
                        block_cells1.append(&mut hex_cells(true, id, &decode));
                    }

                    let block_len = std::cmp::max(block_cells0.len(), block_cells1.len());
                    let padding_len0 = block_len - block_cells0.len();
                    let padding_len1 = block_len - block_cells1.len();

                    for _ in 0..padding_len0 {
                        block_cells0.push(HexCell::Blank);
                    }

                    for _ in 0..padding_len1 {
                        block_cells1.push(HexCell::Blank);
                    }

                    cells0.append(&mut block_cells0);
                    cells1.append(&mut block_cells1);

                    matches_index_to_cells0_alignment_offset.push(offset0);
                    matches_index_to_cells1_alignment_offset.push(offset1);

                    index0 += block_len;
                    index1 += block_len;

                    offset0 += padding_len0;
                    offset1 += padding_len1;
                }
            });
        (
            cells0,
            cells1,
            Self {
                matches,

                cells0_index_to_matches_index,
                cells1_index_to_matches_index,
                matches_index_to_cells0_alignment_offset,
                matches_index_to_cells1_alignment_offset,
            },
        )
    }
}
