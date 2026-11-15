# Documentation

1. **Comments may show syntax that the language does not have**: the prelude described lists as `[a, b; xs]`, sets as `{a, b}`, and arrays as `|a, b|` long after the forms replaced them, which an agent reads as ulang (the design review of 5cd2fe1). Solution: a check of `checks/docs.u` that the comments of the sources contain no syntax that the lexer rejects, such as `[a, b]` outside type arguments.
