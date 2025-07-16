use crate::matcher::Matched;
use crate::test_utils::{hex_cells, HexCell};
use crate::token::TokenId;
use rangemap::RangeMap;

struct AlignedCells {
    matches: Vec<Matched>,
    pub cells0: Vec<HexCell>,
    pub cells1: Vec<HexCell>,
    cells0_index_to_matches_index: RangeMap<usize, usize>,
    cells1_index_to_matches_index: RangeMap<usize, usize>,
    matches_index_to_cells0_alignment_offset: Vec<usize>,
    matches_index_to_cells1_alignment_offset: Vec<usize>,
}

impl AlignedCells {
    pub fn new(matches: Vec<Matched>, decode: impl Fn(&Vec<TokenId>) -> Vec<u8>) -> Self {
        let mut cells0 = vec![];
        let mut cells1 = vec![];

        matches.iter().for_each(|matched| match matched {
            Matched::Same(ids) => {
                for &id in ids {
                    cells0.append(&mut hex_cells(false, id, &decode));
                    cells1.append(&mut hex_cells(false, id, &decode));
                }
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

                while block_cells0.len() < block_cells1.len() {
                    block_cells0.push(HexCell::Blank);
                }

                while block_cells1.len() < block_cells0.len() {
                    block_cells1.push(HexCell::Blank);
                }

                cells0.append(&mut block_cells0);
                cells1.append(&mut block_cells1);
            }
        });

        Self {
            matches,
            cells0,
            cells1,
            cells0_index_to_matches_index: RangeMap::default(),
            cells1_index_to_matches_index: RangeMap::default(),
            matches_index_to_cells0_alignment_offset: vec![],
            matches_index_to_cells1_alignment_offset: vec![],
        }
    }
}
