//! This example demonstrates how to compile a shulkerscript file into a datapack using the shulkerscript compiler.
//! Most basic version of a shulkerscript compiler, which takes a single input file and places the resulting datapack in the specified output directory.
//!
//! For a ready-to-use compiler, see the `shulkerscript-cli` crate.

use shulkerscript::{
    base::{FsProvider, PrintHandler},
    compile,
};

#[cfg(not(feature = "shulkerbox"))]
compile_error!("Need feature 'shulkerbox' to compile this example");

fn main() {
    let mut args = std::env::args();
    let _ = args.next().unwrap();
    let input = args.next().expect("Expect path to shulkerscript file");

    let output = args.next().expect("Expect path to output directory");

    let code = compile(
        &PrintHandler::new(),
        &FsProvider::default(),
        shulkerbox::datapack::Datapack::LATEST_FORMAT,
        &[("main".to_string(), &input)],
    )
    .expect("failed to compile");

    code.place(output).expect("failed to place datapack");
}
