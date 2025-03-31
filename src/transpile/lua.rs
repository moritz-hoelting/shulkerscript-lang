//! Executes the Lua code and returns the resulting command.

#[cfg(all(feature = "lua", feature = "shulkerbox"))]
mod enabled {
    use std::sync::Arc;

    use mlua::{Lua, Table, Value};

    use crate::{
        base::{self, source_file::SourceElement, Handler},
        lexical::token::Identifier,
        syntax::syntax_tree::expression::LuaCode,
        transpile::{
            error::{
                InvalidArgument, LuaRuntimeError, MismatchedTypes, TranspileError, TranspileResult,
                UnknownIdentifier,
            },
            expression::{ComptimeValue, ExpectedType},
            Scope, VariableData,
        },
    };

    impl LuaCode {
        /// Evaluated the Lua code and returns the resulting value.
        ///
        /// # Errors
        /// - If evaluation fails
        #[tracing::instrument(level = "debug", name = "eval_lua", skip_all, ret)]
        pub fn eval(
            &self,
            scope: &Arc<Scope>,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<(mlua::Value, mlua::Lua)> {
            tracing::debug!("Evaluating Lua code");

            let lua = Lua::new();

            let name = {
                let span = self.span();
                let file = span.source_file();
                let path = file.path_relative().unwrap_or_else(|| file.path().clone());

                let start = span.start_location();
                let end = span.end_location().unwrap_or_else(|| {
                    let content_size = file.content().len();
                    file.get_location(content_size - 1)
                        .expect("Failed to get location")
                });

                format!(
                    "{}:{}:{}-{}:{}",
                    path.display(),
                    start.line,
                    start.column,
                    end.line,
                    end.column
                )
            };

            self.add_globals(&lua, scope)
                .inspect_err(|err| handler.receive(err.clone()))?;

            let res = lua
                .load(self.code())
                .set_name(name)
                .eval::<Value>()
                .map_err(|err| {
                    let err =
                        TranspileError::from(LuaRuntimeError::from_lua_err(&err, self.span()));
                    handler.receive(crate::Error::from(err.clone()));
                    err
                });

            res.map(|v| {
                tracing::debug!("Lua code evaluated successfully");
                (v, lua)
            })
        }

        /// Evaluates the Lua code and returns the resulting [`ComptimeValue`].
        ///
        /// # Errors
        /// - If Lua code evaluation is disabled.
        #[tracing::instrument(level = "debug", name = "eval_lua", skip_all, ret)]
        pub fn eval_comptime(
            &self,
            scope: &Arc<Scope>,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<Option<ComptimeValue>> {
            // required to keep the lua instance alive
            let (lua_result, _lua) = self.eval(scope, handler)?;

            self.handle_lua_result(lua_result, handler)
        }

        fn add_globals(&self, lua: &Lua, scope: &Arc<Scope>) -> TranspileResult<()> {
            let globals = lua.globals();

            let shulkerscript_globals = self
                .get_std_library(lua)
                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;

            if let Some(inputs) = self.inputs() {
                for identifier in inputs.elements() {
                    let (name, value) = self.add_input_to_globals(identifier, lua, scope)?;
                    globals
                        .set(name, value)
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                }
            }

            globals
                .set("shulkerscript", shulkerscript_globals)
                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;

            Ok(())
        }

        fn get_std_library(&self, lua: &Lua) -> mlua::Result<Table> {
            let table = lua.create_table()?;

            let (location_path, location_start, location_end) = {
                let span = self.span();
                let file = span.source_file();
                let path = file.path().to_owned();
                let start_location = span.start_location();
                let end_location = span.end_location().unwrap_or_else(|| {
                    let line_amount = file.line_amount();
                    let column = file.get_line(line_amount).expect("line amount used").len();

                    crate::base::source_file::Location {
                        line: line_amount,
                        column,
                    }
                });

                (path, start_location, end_location)
            };

            table.set("file_path", location_path.to_string_lossy())?;
            table.set("start_line", location_start.line)?;
            table.set("start_column", location_start.column)?;
            table.set("end_line", location_end.line)?;
            table.set("end_column", location_end.column)?;

            table.set("version", crate::VERSION)?;

            Ok(table)
        }

        #[expect(clippy::too_many_lines)]
        fn add_input_to_globals<'a>(
            &self,
            identifier: &'a Identifier,
            lua: &Lua,
            scope: &Arc<Scope>,
        ) -> TranspileResult<(&'a str, Value)> {
            let name = identifier.span.str();
            let value = match scope.get_variable(name).as_deref() {
                Some(VariableData::MacroParameter { macro_name, .. }) => Value::String(
                    lua.create_string(format!("$({macro_name})"))
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?,
                ),
                Some(VariableData::Scoreboard { objective }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("objective", objective.as_str())
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::ScoreboardValue { objective, target }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set(
                            "objective",
                            lua.create_string(objective)
                                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?,
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set(
                            "target",
                            lua.create_string(target)
                                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?,
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::ScoreboardArray { objective, targets }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("objective", objective.as_str())
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    let values = lua
                        .create_table_from(
                            targets
                                .iter()
                                .enumerate()
                                .map(|(i, target)| (i + 1, target.as_str())),
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("targets", values)
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::BooleanStorage { storage_name, path }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set(
                            "storage",
                            lua.create_string(storage_name)
                                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?,
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set(
                            "path",
                            lua.create_string(path)
                                .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?,
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::BooleanStorageArray {
                    storage_name,
                    paths,
                }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("storage", storage_name.as_str())
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    let values = lua
                        .create_table_from(
                            paths
                                .iter()
                                .enumerate()
                                .map(|(i, path)| (i + 1, path.as_str())),
                        )
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("paths", values)
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::Tag { tag_name }) => {
                    let table = lua
                        .create_table()
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    table
                        .set("name", tag_name.as_str())
                        .map_err(|err| LuaRuntimeError::from_lua_err(&err, self.span()))?;
                    Value::Table(table)
                }
                Some(VariableData::Function { .. } | VariableData::InternalFunction { .. }) => {
                    // TODO: add support for functions
                    return Err(TranspileError::InvalidArgument(InvalidArgument {
                        reason: "functions cannot be passed to Lua".to_string(),
                        span: identifier.span(),
                    }));
                }
                None => {
                    return Err(TranspileError::UnknownIdentifier(UnknownIdentifier {
                        identifier: identifier.span(),
                    }));
                }
            };

            Ok((name, value))
        }

        fn handle_lua_result(
            &self,
            value: Value,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<Option<ComptimeValue>> {
            match value {
                Value::Nil => Ok(None),
                Value::String(s) => Ok(Some(ComptimeValue::String(s.to_string_lossy()))),
                Value::Integer(i) => Ok(Some(ComptimeValue::Integer(i))),
                // TODO: change when floating point comptime numbers are supported
                Value::Number(n) => Ok(Some(ComptimeValue::String(n.to_string()))),
                Value::Function(f) => self.handle_lua_result(
                    f.call(()).map_err(|err| {
                        TranspileError::LuaRuntimeError(LuaRuntimeError::from_lua_err(
                            &err,
                            self.span(),
                        ))
                    })?,
                    handler,
                ),
                Value::Boolean(boolean) => Ok(Some(ComptimeValue::Boolean(boolean))),
                Value::Table(table) => match table.get::<Value>("value") {
                    Ok(Value::Nil) => {
                        let err = TranspileError::LuaRuntimeError(LuaRuntimeError {
                            code_block: self.span(),
                            error_message: "return table must contain non-nil 'value'".to_string(),
                        });
                        handler.receive(err.clone());
                        Err(err)
                    }
                    Ok(value) => {
                        let value = match self.handle_lua_result(value, handler)? {
                            Some(ComptimeValue::String(s)) => {
                                let contains_macro = match table.get::<Value>("contains_macro") {
                                    Ok(Value::Boolean(boolean)) => Ok(boolean),
                                    Ok(value) => {
                                        if let Some(ComptimeValue::Boolean(boolean)) =
                                            self.handle_lua_result(value, handler)?
                                        {
                                            Ok(boolean)
                                        } else {
                                            let err =
                                                TranspileError::MismatchedTypes(MismatchedTypes {
                                                    expression: self.span(),
                                                    expected_type: ExpectedType::Boolean,
                                                });
                                            handler.receive(err.clone());
                                            Err(err)
                                        }
                                    }
                                    _ => {
                                        let err =
                                            TranspileError::MismatchedTypes(MismatchedTypes {
                                                expression: self.span(),
                                                expected_type: ExpectedType::Boolean,
                                            });
                                        handler.receive(err.clone());
                                        Err(err)
                                    }
                                }?;

                                if contains_macro {
                                    Some(ComptimeValue::MacroString(
                                        s.parse().expect("parsing cannot fail"),
                                    ))
                                } else {
                                    Some(ComptimeValue::String(s))
                                }
                            }
                            value => value,
                        };
                        Ok(value)
                    }
                    Err(err) => {
                        let err = TranspileError::LuaRuntimeError(LuaRuntimeError::from_lua_err(
                            &err,
                            self.span(),
                        ));
                        handler.receive(err.clone());
                        Err(err)
                    }
                },
                Value::Error(_)
                | Value::Thread(_)
                | Value::UserData(_)
                | Value::LightUserData(_)
                | Value::Other(..) => {
                    let err = TranspileError::LuaRuntimeError(LuaRuntimeError {
                        code_block: self.span(),
                        error_message: format!("invalid return type {}", value.type_name()),
                    });
                    handler.receive(err.clone());
                    Err(err)
                }
            }
        }
    }
}

#[cfg(all(not(feature = "lua"), feature = "shulkerbox"))]
mod disabled {
    use std::sync::Arc;

    use crate::{
        base::{self, Handler},
        syntax::syntax_tree::expression::LuaCode,
        transpile::error::{TranspileError, TranspileResult},
    };

    use crate::transpile::{expression::ComptimeValue, Scope};

    impl LuaCode {
        /// Will always return an error because Lua code evaluation is disabled.
        /// Enable the feature `lua` to enable Lua code evaluation.
        ///
        /// # Errors
        /// - Always, as the lua feature is disabled
        #[tracing::instrument(level = "debug", name = "eval_lua", skip_all, ret)]
        pub fn eval(
            &self,
            scope: &Arc<Scope>,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<((), ())> {
            let _ = scope;
            handler.receive(TranspileError::LuaDisabled);
            tracing::error!("Lua code evaluation is disabled");
            Err(TranspileError::LuaDisabled)
        }

        /// Will always return an error because Lua code evaluation is disabled.
        /// Enable the feature `lua` to enable Lua code evaluation.
        ///
        /// # Errors
        /// - If Lua code evaluation is disabled.
        pub fn eval_comptime(
            &self,
            scope: &Arc<Scope>,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<Option<ComptimeValue>> {
            let _ = scope;
            handler.receive(TranspileError::LuaDisabled);
            tracing::error!("Lua code evaluation is disabled");
            Err(TranspileError::LuaDisabled)
        }
    }
}
