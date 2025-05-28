#![warn(clippy::all, rust_2018_idioms)]

mod hex_app;
pub use arb_comp06::diff;
pub use hex_app::HexApp;

#[cfg(target_arch = "wasm32")]
pub use wasm_bindgen_rayon::init_thread_pool;
