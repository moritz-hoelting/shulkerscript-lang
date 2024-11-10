//! Executes the Lua code and returns the resulting command.

#[cfg(feature = "lua")]
mod enabled {
    use mlua::{Lua, Value};

    use crate::{
        base::{self, source_file::SourceElement, Handler},
        syntax::syntax_tree::expression::LuaCode,
        transpile::error::{LuaRuntimeError, TranspileError, TranspileResult},
    };

    impl LuaCode {
        /// Evaluates the Lua code and returns the resulting command.
        ///
        /// # Errors
        /// - If Lua code evaluation is disabled.
        #[tracing::instrument(level = "debug", name = "eval_lua", skip_all, ret)]
        pub fn eval_string(
            &self,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<Option<String>> {
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

            self.add_globals(&lua).unwrap();

            let lua_result = lua
                .load(self.code())
                .set_name(name)
                .eval::<Value>()
                .map_err(|err| {
                    let err =
                        TranspileError::from(LuaRuntimeError::from_lua_err(&err, self.span()));
                    handler.receive(crate::Error::from(err.clone()));
                    err
                })?;

            self.handle_lua_result(lua_result).inspect_err(|err| {
                handler.receive(err.clone());
            })
        }

        fn add_globals(&self, lua: &Lua) -> mlua::Result<()> {
            let globals = lua.globals();

            let location = {
                let span = self.span();
                let file = span.source_file();
                file.path_relative().unwrap_or_else(|| file.path().clone())
            };
            globals.set("shu_location", location.to_string_lossy())?;

            Ok(())
        }

        fn handle_lua_result(&self, value: Value) -> TranspileResult<Option<String>> {
            match value {
                Value::Nil => Ok(None),
                Value::String(s) => Ok(Some(s.to_string_lossy())),
                Value::Integer(i) => Ok(Some(i.to_string())),
                Value::Number(n) => Ok(Some(n.to_string())),
                Value::Function(f) => self.handle_lua_result(f.call(()).map_err(|err| {
                    TranspileError::LuaRuntimeError(LuaRuntimeError::from_lua_err(
                        &err,
                        self.span(),
                    ))
                })?),
                Value::Boolean(_)
                | Value::Error(_)
                | Value::Table(_)
                | Value::Thread(_)
                | Value::UserData(_)
                | Value::LightUserData(_)
                | Value::Other(..) => Err(TranspileError::LuaRuntimeError(LuaRuntimeError {
                    code_block: self.span(),
                    error_message: format!("invalid return type {}", value.type_name()),
                })),
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

    impl LuaCode {
        /// Will always return an error because Lua code evaluation is disabled.
        /// Enable the feature `lua` to enable Lua code evaluation.
        ///
        /// # Errors
        /// - If Lua code evaluation is disabled.
        pub fn eval_string(
            &self,
            handler: &impl Handler<base::Error>,
        ) -> TranspileResult<Option<String>> {
            handler.receive(TranspileError::LuaDisabled);
            tracing::error!("Lua code evaluation is disabled");
            Err(TranspileError::LuaDisabled)
        }
    }
}
