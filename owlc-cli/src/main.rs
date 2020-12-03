//! Driver code. Entrypoint into the CLI application is in this crate.

#![forbid(unsafe_code)]

fn main() {
    owlc_driver::driver();
}
