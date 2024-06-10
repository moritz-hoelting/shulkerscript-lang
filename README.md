# ShulkerScript Language

ShulkerScript is a simple, easy-to-use scripting language for Minecraft datapacks. It is designed to be easy to learn and use, while still being powerful enough to create complex scripts, while not being hindered by Minecraft command limitations.

## Usage

Add the following to your dependencies in `Cargo.toml`:
```toml
[dependencies]
shulkerscript-lang = { git = "https://github.com/moritz-hoelting/shulkerscript-lang" }
```

## Features

### Functions
```shu
#[load]
fn load() {
    /say Loaded!
    hello();
}

fn hello() {
    /say Hello, world!
}
```

### Execute blocks
```shu
as("@a") { // execute as all players
    /say Hello, 
    /say world!
}
```
Chained execute blocks are also supported:
```shu
as("@a"), at("@s") { // execute as all players at the executing entity
    /setblock ~ ~ ~ minecraft:diamond_block
}
```

### Conditionals
```shu
if("@s[tag=foo]" && "block ~ ~-1 ~ minecraft:stone") { // if the executing entity has the tag "foo" and is standing on stone
    /tag @s remove foo
    /say Hello, foo!
} else {
    /say Hello, world!
}
```
Unlike in `mcfunction` files, the actions in the conditional blocks are atomic, meaning that once the condition is fullfilled the actions are executed even after the condition is no longer fullfilled (in this case the "foo" tag has been removed).

### Lua scripting
```shu
run lua() {
    -- Lua code goes here
    return "Hello, Lua!";
};
```