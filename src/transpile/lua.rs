//! Executes the Lua code and returns the resulting command.

#[cfg(feature = "lua")]
mod enabled {
    use std::sync::Arc;

    use mlua::{Lua, Value};

    use crate::{
        base::{self, source_file::SourceElement, Handler},
        syntax::syntax_tree::expression::LuaCode,
        transpile::{
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

            if let Err(err) = self.add_globals(&lua, scope) {
                let err = TranspileError::LuaRuntimeError(LuaRuntimeError::from_lua_err(
                    &err,
                    self.span(),
                ));
                handler.receive(crate::Error::from(err.clone()));
                return Err(err);
            }

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

        fn add_globals(&self, lua: &Lua, scope: &Arc<Scope>) -> mlua::Result<()> {
            let globals = lua.globals();

            let location = {
                let span = self.span();
                let file = span.source_file();
                file.path_relative().unwrap_or_else(|| file.path().clone())
            };
            globals.set("shu_location", location.to_string_lossy())?;

            if let Some(inputs) = self.inputs() {
                for x in inputs.elements() {
                    let name = x.span.str();
                    let value = match scope.get_variable(name).as_deref() {
                        Some(VariableData::MacroParameter { macro_name, .. }) => {
                            Value::String(lua.create_string(format!("$({macro_name})"))?)
                        }
                        Some(VariableData::ScoreboardValue { objective, target }) => {
                            let table = lua.create_table()?;
                            table.set("objective", lua.create_string(objective)?)?;
                            table.set("target", lua.create_string(target)?)?;
                            Value::Table(table)
                        }
                        Some(VariableData::BooleanStorage { storage_name, path }) => {
                            let table = lua.create_table()?;
                            table.set("storage", lua.create_string(storage_name)?)?;
                            table.set("path", lua.create_string(path)?)?;
                            Value::Table(table)
                        }
                        Some(_) => todo!("allow other types"),
                        None => todo!("throw correct error"),
                    };
                    globals.set(name, value)?;
                }
            }

            Ok(())
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
                Value::Error(_)
                | Value::Table(_)
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

#[cfg(not(feature = "lua"))]
mod disabled {
    use crate::{
        base::{self, Handler},
        syntax::syntax_tree::expression::LuaCode,
        transpile::error::{TranspileError, TranspileResult},
    };

    use crate::transpile::expression::ComptimeValue;

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
        ) -> TranspileResult<()> {
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
            handler.receive(TranspileError::LuaDisabled);
            tracing::error!("Lua code evaluation is disabled");
            Err(TranspileError::LuaDisabled)
        }
    }
}
