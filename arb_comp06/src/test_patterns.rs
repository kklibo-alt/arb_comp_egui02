use rand::{RngCore, SeedableRng};
use rand_chacha::ChaCha20Rng;

pub fn trivial() -> (Vec<u8>, Vec<u8>) {
    let pattern0 = "aJAOA1pjSAwCr9CkW3FE7166ch/309iOkW3FRa+1ch/30WIYjbT";
    let pattern1 = "aJAOA1pjSAwCr9CkW3kkZMFE7166ch/309iORa+1ch/30WkkZMIYjbT";
    (pattern0.as_bytes().to_vec(), pattern1.as_bytes().to_vec())
}

/// (implemented by Claude 4 Sonnet +thinking)
/// Uses ChaCha20Rng for portable and reproducible random data generation
pub fn random_1k() -> (Vec<u8>, Vec<u8>) {
    // Use ChaCha20Rng for portable and reproducible results across platforms
    let mut rng = ChaCha20Rng::seed_from_u64(12345);

    // Generate first 1000-byte pattern
    let mut pattern0 = vec![0u8; 1000];
    rng.fill_bytes(&mut pattern0);

    // Generate second 1000-byte pattern
    let mut pattern1 = vec![0u8; 1000];
    rng.fill_bytes(&mut pattern1);

    (pattern0, pattern1)
}
