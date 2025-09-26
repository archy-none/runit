cargo run -q > ./example.rs 2> example.tr.meta
rustc --verbose ./example.rs
./example
rm ./example
