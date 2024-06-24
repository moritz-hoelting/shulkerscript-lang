use shulkerbox::{
    datapack::{Command, Condition, Datapack, Execute},
    virtual_fs::{VFile, VFolder},
};

#[test]
fn transpile_test1() {
    let source = include_str!("./test1.shu");
    let mut dir = VFolder::new();
    dir.add_file("test1.shu", VFile::Text(source.to_string()));

    let transpiled = shulkerscript::transpile(&dir, &[("test1".to_string(), "./test1.shu")])
        .expect("Failed to transpile");

    let expected = {
        let mut dp = Datapack::new(48);

        let namespace = dp.namespace_mut("transpiling-test");

        let main_fn = namespace.function_mut("main");

        main_fn.add_command(Command::Raw("say Hello, World!".to_string()));

        let exec_cmd = Command::Execute(Execute::As(
            "@a".to_string(),
            Box::new(Execute::If(
                Condition::Atom("entity @p[distance=..5]".to_string()),
                Box::new(Execute::Run(Box::new(Command::Raw(
                    "say You are close to me!".to_string(),
                )))),
                Some(Box::new(Execute::Run(Box::new(Command::Raw(
                    "say You are alone!".to_string(),
                ))))),
            )),
        ));

        main_fn.add_command(exec_cmd);

        dp
    };

    assert_eq!(transpiled, expected);
}
