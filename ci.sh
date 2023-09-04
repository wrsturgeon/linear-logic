#!/usr/bin/env sh

set -eux

rustup toolchain install nightly
rustup component add miri --toolchain nightly

cargo fmt --check
cargo clippy --all-targets --no-default-features
cargo clippy --all-targets --all-features
RUST_BACKTRACE=1 cargo test -r --all-features
MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri test --target=x86_64-apple-darwin
MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri test --target=x86_64-unknown-linux-gnu
MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri test --target=aarch64-apple-darwin
MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri test --target=aarch64-unknown-linux-gnu
MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri test --target=arm-unknown-linux-gnueabi