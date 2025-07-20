use crate::matcher::Matched;
use crate::test_utils::{hex_cells, HexCell};
use crate::token::TokenId;

#[derive(Debug, Clone, Copy, Default)]
struct AlignedAddress {
    in_cells0: usize,
    in_cells1: usize,
    in_alignment: usize,
}

impl AlignedAddress {
    // replace with real trait impl?
    fn greater_than(&self, address: Address) -> bool {
        match address {
            Address::Cells0(x) => x < self.in_cells0,
            Address::Cells1(x) => x < self.in_cells1,
            Address::Alignment(x) => x < self.in_alignment,
        }
    }

    fn offset_to(&self, address: Address) -> Offset {
        match address {
            Address::Cells0(x) => Offset::new(self.in_cells0, x),
            Address::Cells1(x) => Offset::new(self.in_cells1, x),
            Address::Alignment(x) => Offset::new(self.in_alignment, x),
        }
    }

    fn apply(&self, offset: Offset) -> Option<Self> {
        match offset {
            Offset::Increase(x) => Some(Self {
                in_cells0: self.in_cells0.checked_add(x)?,
                in_cells1: self.in_cells1.checked_add(x)?,
                in_alignment: self.in_alignment.checked_add(x)?,
            }),
            Offset::Decrease(x) => Some(Self {
                in_cells0: self.in_cells0.checked_sub(x)?,
                in_cells1: self.in_cells1.checked_sub(x)?,
                in_alignment: self.in_alignment.checked_sub(x)?,
            }),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Address {
    Cells0(usize),
    Cells1(usize),
    Alignment(usize),
}

#[derive(Debug, Clone, Copy)]
enum Offset {
    Increase(usize),
    Decrease(usize),
}

impl Offset {
    fn new(from: usize, to: usize) -> Self {
        if from < to {
            Self::Increase(to - from)
        } else {
            Self::Decrease(from - to)
        }
    }
}

#[derive(Debug, Default)]
pub struct CellAlignment {
    // Within an address space,
    // a block is specified by an address that is its exclusive upper bound.
    // The previous block's address (or 0 for the first block) is its start.
    aligned_blocks: Vec<AlignedAddress>,
}

impl CellAlignment {
    fn add(&mut self, cells0_block_len: usize, cells1_block_len: usize) {
        dbg!(cells0_block_len);
        dbg!(cells1_block_len);

        let new_aligned_block_len = std::cmp::max(cells0_block_len, cells1_block_len);

        let prev_block = self.aligned_blocks.last().copied().unwrap_or_default();

        let new_block = AlignedAddress {
            in_cells0: prev_block.in_cells0 + cells0_block_len,
            in_cells1: prev_block.in_cells1 + cells1_block_len,
            in_alignment: prev_block.in_alignment + new_aligned_block_len,
        };

        self.aligned_blocks.push(new_block);
    }

    fn get_block_index(&self, address: usize, blocks: &Vec<usize>) -> usize {
        blocks.partition_point(|&block_start| address < block_start)
    }

    fn aligned_address(&self, address: Address) -> Option<AlignedAddress> {
        let index = self
            .aligned_blocks
            .partition_point(|block| block.greater_than(address));
        self.aligned_blocks
            .get(index)
            .and_then(|x| x.apply(x.offset_to(address)))
    }
}

pub fn matches_to_cells(
    matches: &[Matched],
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
) -> (Vec<HexCell>, Vec<HexCell>, CellAlignment) {
    let mut cells0 = vec![];
    let mut cells1 = vec![];
    let mut alignment = CellAlignment::default();

    let token_ids_to_hex_cells = |ids: &[TokenId], diff: bool| -> Vec<HexCell> {
        ids.iter()
            .flat_map(|&id| hex_cells(diff, id, &decode))
            .collect()
    };

    dbg!(matches);

    matches.iter().for_each(|matched| match matched {
        Matched::Same(ids) => {
            let mut new_cells = token_ids_to_hex_cells(ids, false);

            alignment.add(new_cells.len(), new_cells.len());

            cells0.append(&mut new_cells.clone());
            cells1.append(&mut new_cells);
        }
        Matched::Diff(ids0, ids1) => {
            let mut block_cells0 = token_ids_to_hex_cells(ids0, true);
            let mut block_cells1 = token_ids_to_hex_cells(ids1, true);

            alignment.add(block_cells0.len(), block_cells1.len());

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

    dbg!(&alignment);

    (cells0, cells1, alignment)
}
