use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;

pub fn trivial() -> (Vec<u8>, Vec<u8>) {
    let pattern0 = "aJAOA1pjSAwCr9CkW3FE7166ch/309iOkW3FRa+1ch/30WIYjbT";
    let pattern1 = "aJAOA1pjSAwCr9CkW3kkZMFE7166ch/309iORa+1ch/30WkkZMIYjbT";
    (pattern0.as_bytes().to_vec(), pattern1.as_bytes().to_vec())
}

fn random_block(count: usize) -> Vec<u8> {
    let mut rng = ChaCha20Rng::seed_from_u64(12345);

    let mut pattern = vec![0u8; count];
    rng.fill_bytes(&mut pattern);

    pattern
}

/// Uses ChaCha20Rng for portable and reproducible random data generation
fn random(count: usize) -> (Vec<u8>, Vec<u8>) {
    // Use ChaCha20Rng for portable and reproducible results across platforms
    let mut rng = ChaCha20Rng::seed_from_u64(12345);

    let mut pattern0 = vec![0u8; count];
    rng.fill_bytes(&mut pattern0);

    let mut pattern1 = vec![0u8; count];
    rng.fill_bytes(&mut pattern1);

    (pattern0, pattern1)
}

pub fn random_1k() -> (Vec<u8>, Vec<u8>) {
    random(1024)
}

pub fn random_256() -> (Vec<u8>, Vec<u8>) {
    random(256)
}

pub fn random_minus_block() -> (Vec<u8>, Vec<u8>) {
    let pattern0 = random_block(256);
    let mut pattern1 = pattern0.clone();

    pattern1.drain(32..64);
    (pattern0, pattern1)
}
