use crate::matcher::Matched;
use crate::test_utils::{hex_cells, HexCell};
use crate::token::TokenId;

#[derive(Debug, Default)]
pub struct CellAlignment {
    cells0_addresses: Vec<usize>,
    cells1_addresses: Vec<usize>,
    aligned_addresses: Vec<usize>,
}

impl CellAlignment {
    fn add(&mut self, cells0_address: usize, cells1_address: usize) {
        let prev_cells0_address = *self.cells0_addresses.last().unwrap_or(&0);
        let prev_cells1_address = *self.cells1_addresses.last().unwrap_or(&0);
        let prev_aligned_address = *self.aligned_addresses.last().unwrap_or(&0);

        let cells0_delta = cells0_address.checked_sub(prev_cells0_address).unwrap();
        let cells1_delta = cells1_address.checked_sub(prev_cells1_address).unwrap();
        assert!(cells0_delta > 0);
        assert!(cells1_delta > 0);

        let new_aligned_address = prev_aligned_address + std::cmp::max(cells0_delta, cells1_delta);

        self.cells0_addresses.push(cells0_address);
        self.cells1_addresses.push(cells1_address);
        self.aligned_addresses.push(new_aligned_address);
    }
}

pub fn matches_to_cells(
    matches: &[Matched],
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
) -> (Vec<HexCell>, Vec<HexCell>) {
    let mut cells0 = vec![];
    let mut cells1 = vec![];
    let mut alignment = CellAlignment::default();

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
