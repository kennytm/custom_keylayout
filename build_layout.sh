#!/bin/sh

# FIXME: Remove the workaround `| tail -n+2` after rust-lang/cargo#653 is fixed.
cargo run --release --bin custom_keylayout -- -i -30000 -n "U.S. (kennytm)" < kennytm.txt | tail -n+2 > kennytm.keylayout

