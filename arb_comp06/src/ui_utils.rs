use crate::matcher::Matched;
use crate::test_utils::{hex_cells, HexCell};
use crate::token::TokenId;

pub fn matches_to_cells(
    matches: &[Matched],
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
) -> (Vec<HexCell>, Vec<HexCell>) {
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

    (cells0, cells1)
}
