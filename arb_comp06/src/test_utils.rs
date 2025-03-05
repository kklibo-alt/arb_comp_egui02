use crate::bpe::Bpe;
use crate::matcher::Matched;
use crate::token::TokenId;
use colored::*;

fn color(c: usize, s: &str) -> ColoredString {
    match c % 6 {
        0 => s.red(),
        1 => s.yellow(),
        2 => s.green(),
        3 => s.cyan(),
        4 => s.blue(),
        5 => s.magenta(),
        _ => unreachable!(),
    }
}

fn color_highlight(c: usize, s: &str) -> ColoredString {
    match c % 6 {
        0 => s.on_red(),
        1 => s.on_yellow(),
        2 => s.on_green(),
        3 => s.on_cyan(),
        4 => s.on_blue(),
        5 => s.on_magenta(),
        _ => unreachable!(),
    }
}

pub fn print_tokens(
    ids: impl IntoIterator<Item = TokenId>,
    bpe: &Bpe,
    highlight: impl Fn(&TokenId) -> bool,
) {
    for id in ids {
        let s = format!("{:?}", bpe.decode(vec![id]));

        let s = if highlight(&id) {
            color_highlight(id.0, &s)
        } else {
            color(id.0, &s)
        };
        print!("{s}");
    }
    println!();
}

fn print_columns(
    left: impl IntoIterator<Item = ColoredString>,
    right: impl IntoIterator<Item = ColoredString>,
) {
    let width = 8;
    let mut left = left.into_iter().peekable();
    let mut right = right.into_iter().peekable();

    fn row_section(width: usize, i: &mut impl Iterator<Item = ColoredString>) {
        for c in 0..width {
            if let Some(s) = i.next() {
                print!("{s}");
            } else {
                print!("  ");
            }
            if c != width - 1 {
                print!(" ");
            }
        }
    }

    while left.peek().is_some() || right.peek().is_some() {
        row_section(width, &mut left);
        print!(" - ");
        row_section(width, &mut right);
        println!();
    }
}

fn colored_hex(
    highlight: bool,
    id: TokenId,
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
) -> Vec<ColoredString> {
    decode(&vec![id])
        .iter()
        .map(|b| format!("{b:02x}"))
        .map(|s| {
            assert_eq!(2, s.chars().count());
            if highlight {
                color_highlight(id.0, &s)
            } else {
                color(id.0, &s)
            }
        })
        .collect()
}

fn colored_ascii(
    highlight: bool,
    id: TokenId,
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
) -> Vec<ColoredString> {
    decode(&vec![id])
        .iter()
        .map(|b| {
            format!(
                "{:2}",
                char::from_u32(*b as u32)
                    .filter(|c| c.is_ascii() && !c.is_control())
                    .unwrap_or('.')
            )
        })
        .map(|s| {
            assert_eq!(2, s.chars().count());
            if highlight {
                color_highlight(id.0, &s)
            } else {
                color(id.0, &s)
            }
        })
        .collect()
}

fn print_colored_id_lists(highlight: bool, left: &[TokenId], right: &[TokenId]) {
    let hex_width = 8;
    let middle_width = 3;
    let width = hex_width * 3 - 1 + middle_width;

    let print_id = |id: TokenId| {
        let s = format!("{}", id.0);

        let s = if highlight {
            color_highlight(id.0, &s)
        } else {
            color(id.0, &s)
        };
        print!("[{s}] ");
    };

    for &id in left.iter() {
        print_id(id);
    }
    println!();

    if !right.is_empty() {
        print!("{:width$}", "");
        for &id in right.iter() {
            print_id(id);
        }
        println!();
    }
}

pub fn print_ui_01(
    matches: &[Matched],
    decode: impl Fn(&Vec<TokenId>) -> Vec<u8>,
    print_token_ids: bool,
) {
    matches.iter().for_each(|matched| match matched {
        Matched::Same(ids) => {
            if print_token_ids {
                print_colored_id_lists(false, ids, &[]);
            }
            print_columns(
                ids.iter().flat_map(|&id| colored_hex(false, id, &decode)),
                ids.iter().flat_map(|&id| colored_hex(false, id, &decode)),
            );
            print_columns(
                ids.iter().flat_map(|&id| colored_ascii(false, id, &decode)),
                ids.iter().flat_map(|&id| colored_ascii(false, id, &decode)),
            );
        }
        Matched::Diff(ids0, ids1) => {
            if print_token_ids {
                print_colored_id_lists(true, ids0, ids1);
            }
            print_columns(
                ids0.iter().flat_map(|&id| colored_hex(true, id, &decode)),
                ids1.iter().flat_map(|&id| colored_hex(true, id, &decode)),
            );
            print_columns(
                ids0.iter().flat_map(|&id| colored_ascii(true, id, &decode)),
                ids1.iter().flat_map(|&id| colored_ascii(true, id, &decode)),
            );
        }
    })
}

//todo: dedup w/ colored_hex?
fn hex_cells(diff: bool, id: TokenId, decode: impl Fn(&Vec<TokenId>) -> Vec<u8>) -> Vec<HexCell> {
    decode(&vec![id])
        .iter()
        .map(|&b| {
            if diff {
                HexCell::Diff {
                    value: b,
                    source_id: id.0,
                }
            } else {
                HexCell::Same {
                    value: b,
                    source_id: id.0,
                }
            }
        })
        .collect()
}

//todo: dedup w/ print_ui_01?
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

// test interface for very lightweight frontend
#[derive(Debug, Clone, Copy)]
pub enum HexCell {
    Same { value: u8, source_id: usize },
    Diff { value: u8, source_id: usize },
    Blank,
}

pub fn print_ui_02(cells0: &[HexCell], cells1: &[HexCell]) {
    fn f(cell: &HexCell) -> ColoredString {
        match cell {
            HexCell::Same { value, source_id } => {
                let s = format!("{value:02x}");
                assert_eq!(2, s.chars().count());
                color(*source_id, &s)
            }
            HexCell::Diff { value, source_id } => {
                let s = format!("{value:02x}");
                assert_eq!(2, s.chars().count());
                color_highlight(*source_id, &s)
            }
            HexCell::Blank => "__".white(),
        }
    }

    print_columns(cells0.iter().map(f), cells1.iter().map(f));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_print_ui_01() {
        let pattern0 = "test abc".as_bytes();
        let pattern1 = "test 123 abc".as_bytes();

        let bpe = Bpe::new(&[pattern0, pattern1]);
        let token_ids0 = bpe.encode(pattern0);
        let token_ids1 = bpe.encode(pattern1);

        let matches = vec![
            Matched::Same(token_ids1.clone()),
            Matched::Diff(token_ids0, token_ids1),
        ];
        let decode = |x: &Vec<TokenId>| bpe.decode(x.clone());

        print_ui_01(&matches, decode, true);

        for a in colored_hex(true, TokenId(256), decode) {
            print!("{a} ");
        }
        println!();
    }

    #[test]
    fn test_print_ui_02() {
        let pattern0 = "test abc".as_bytes();
        let pattern1 = "test 123 abc".as_bytes();

        let bpe = Bpe::new(&[pattern0, pattern1]);
        let token_ids0 = bpe.encode(pattern0);
        let token_ids1 = bpe.encode(pattern1);

        let matches = vec![
            Matched::Same(token_ids1.clone()),
            Matched::Diff(token_ids0, token_ids1),
        ];
        let decode = |x: &Vec<TokenId>| bpe.decode(x.clone());

        let (cells0, cells1) = matches_to_cells(&matches, decode);

        print_ui_02(&cells0, &cells1);
    }
}
