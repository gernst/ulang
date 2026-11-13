# Unclear

Trip-ups and unclear points met while working on ulang code, each with what was wrong and what is so, until the
documentation is fixed from them (see the working rules of `CLAUDE.md`), when they move into the items of `ideas/`.

- A field named `test` does not parse, `keyword test is not a name`: `test` is a keyword, so a record of a generated test names it by its role, `cmd`. `CLAUDE.md` lists the keywords nowhere in one place.
- Moving a module out of the library cannot remove its file at once: the seed `bootstrap/ulang.bin` has the old `library_modules` compiled in and reads every module of it from `ULANG_LIB` when `make` compiles the new sources, so the file stays until `make selfcompile` has replaced the seed.
- A package loaded before the main file, as the enumeration of the tests of lemmas, would load the library first without its tests, so a library module as the main file lost them; the enumeration is loaded after the main file and placed after the library (`load_after_library` of `ulang/parse/parse.u`).
