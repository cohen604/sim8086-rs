# 8086 Processor Simulator in Rust

The goal of this project is to follow up with computer enhance course by building an 8086 processor decoder.

## Listing 37
Here We want to create a decoder that takes the assembled data from `listing_0037_single_register_mov` and decode it back
to `listing_0037_single_register_move.asm` (assembly code)

## Listing 38
Has just a long list of the same kind of `mov` operations like in `Listing 37`

## Listing 39 & 40
Those were hard!!! I had some difficulty on deciding which way to go to be able to fetch different length of bytes because each
operation requires different length according to the first or second byte of data, and not all of them need the second byte to understand this.
I though tgoing the State Machine route but decided to just pass a `&mut` reference to the iterator and the parsers were responsible to fetch as many bytes as they needed before returning the decoded operation.