use std::path::Path;

use shulkerbox::virtual_fs::{VFile, VFolder};
use shulkerscript::{
    base::{source_file::SourceElement, PrintHandler},
    syntax::syntax_tree::{declaration::Declaration, statement::Statement},
};

#[test]
fn parsing_test1() {
    let source = include_str!("./test1.shu");
    let mut dir = VFolder::new();
    dir.add_file("test1.shu", VFile::Text(source.to_string()));

    let parsed = shulkerscript::parse(&PrintHandler::default(), &dir, Path::new("test1.shu"))
        .expect("Failed to parse");

    assert_eq!(
        parsed.namespace().namespace_name().str_content(),
        "parsing-test"
    );

    let declarations = parsed.declarations();
    assert_eq!(declarations.len(), 1);

    let main_fn = declarations.first().unwrap();
    if let Declaration::Function(func) = main_fn {
        assert!(!func.is_public());
        assert_eq!(func.identifier().span().str(), "main");
        assert!(func.parameters().is_none());
        let annotations = func.annotations();
        assert!(annotations.is_empty());
        let statements = func.block().statements();
        assert_eq!(statements.len(), 1);
        let hello_cmd = statements.first().unwrap();
        if let Statement::LiteralCommand(hello_cmd) = hello_cmd {
            assert_eq!(hello_cmd.span().str(), "/say Hello, World!");
        } else {
            panic!("Expected hello command");
        }
    } else {
        panic!("Expected main function declaration");
    }
}

#[test]
fn parsing_invalid() {
    let source = include_str!("./invalid.shu");
    let mut dir = VFolder::new();
    dir.add_file("invalid.shu", VFile::Text(source.to_string()));

    shulkerscript::parse(&PrintHandler::default(), &dir, Path::new("invalid.shu"))
        .expect_err("Expecting parsing failure");
}
