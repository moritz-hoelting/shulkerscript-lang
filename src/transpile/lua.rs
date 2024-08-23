//! Executes the Lua code and returns the resulting command.

#[cfg(feature = "lua")]
mod enabled {
    use mlua::Lua;

    use crate::{
        base::{self, source_file::SourceElement, Handler},
        syntax::syntax_tree::expression::LuaCode,
        transpile::error::{TranspileError, TranspileResult},
    };

    impl LuaCode {
        /// Evaluates the Lua code and returns the resulting command.
        ///
        /// # Errors
        /// - If Lua code evaluation is disabled.
        #[tracing::instrument(level = "debug", name = "eval_lua", skip_all, ret)]
        pub fn eval_string(&self, handler: &impl Handler<base::Error>) -> TranspileResult<String> {
            tracing::debug!("Evaluating Lua code");

            let lua = Lua::new();

            let name = {
                let span = self.span();
                let file = span.source_file();
                let path = file.path();

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

            let lua_result = lua
                .load(self.code())
                .set_name(name)
                .eval::<String>()
                .map_err(|err| {
                    let err = TranspileError::from(err);
                    handler.receive(err.clone());
                    err
                })?;

            Ok(lua_result)
        }
    }

    impl From<mlua::Error> for TranspileError {
        fn from(value: mlua::Error) -> Self {
            let string = value.to_string();
            Self::LuaRuntimeError(
                string
                    .strip_prefix("runtime error: ")
                    .unwrap_or(&string)
                    .to_string(),
            )
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
        pub fn eval_string(&self, handler: &impl Handler<base::Error>) -> TranspileResult<String> {
            handler.receive(TranspileError::LuaDisabled);
            tracing::error!("Lua code evaluation is disabled");
            Err(TranspileError::LuaDisabled)
        }
    }
}
