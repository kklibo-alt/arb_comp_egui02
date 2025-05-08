pub mod bpe;
pub mod matcher;
mod pairs;
pub mod re_pair;
pub mod recode;
mod techniques;
pub mod test_utils;
mod token;
mod utils;

use bpe::Bpe;
use matcher::greedy00;
use test_utils::print_ui_01;

fn main() {
    println!("diff test");

    //read files from first 2 arguments
    let args: Vec<String> = std::env::args().collect();
    let files = args
        .iter()
        .skip(1)
        .take(2)
        .map(|s| s.to_string())
        .collect::<Vec<_>>();
    let file1 = std::fs::read(&files[0]).expect("Could not read file");
    let file2 = std::fs::read(&files[1]).expect("Could not read file");

    let bpe = Bpe::new(&[&file1, &file2]);

    let ids0 = bpe.encode(&file1);
    let ids1 = bpe.encode(&file2);

    let matches = greedy00(&ids0, &ids1);

    print_ui_01(&matches, |x| bpe.decode(x.clone()), false);
}
