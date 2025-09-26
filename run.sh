cargo run -q > ./example.rs 2> example.meta
rustc --verbose ./example.rs
./example
rm ./example
