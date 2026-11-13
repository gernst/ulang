# Ideas for examples

Programs that show ulang on tasks of real software, beside `examples/words.u`, the most frequent words of files, `examples/csv.u`, the relational algebra on CSV files over `library/csv`, and `examples/grep.u`, over the regular expressions of `library/regex`.
What `os` offers bounds them: files, arguments, printing, and directories, and bytes, but no processes, clock, or network, so most are tools from text or bytes to text.

## Within the language as it is

1. **`json.u`**, a parser and pretty-printer of JSON with a small query, `.a.b[0]`, like `jq`: a datatype `Json` with a `show` that prints JSON, a parser of procedures that consume characters, `raise` with positions, `Map` for objects, and a test of the round trip, parse and print. About 150 lines; the most of a real tool, checked against real files and timed interpreted, compiled, and against `jq` on a large file. As a package `json/` with an `export` section, it also shows modules.
2. **`diff.u`**, a diff of the lines of two files by Myers' algorithm, printed as `diff -u` does: `Array` for random access, a search by `while` over a `ref` state, and output that `diff` checks. About 80 lines. The `format error` of `check`, which reports the first line that differs, could use it.
3. **`calc.u`**, a calculator of its argument, `run calc.u -- "2 ^ 100 / 3 + x where x = 7"`: notations `infix` of the language itself, pattern aliases over the tree of expressions, unbounded integers, and `DivisionByZero` caught. About 70 lines; features that mainstream languages do not have in this form.
4. **`tree.u`**, like `tree` or `du`, the files of a directory with their counts of files and lines: `os_walk`, `os_list`, `os_is_dir`, recursion over the file system by procedures, and `Buffer` for the output. About 50 lines; run on `ulang/`, it shows the size of the compiler.
5. **`topo.u`**, a topological sort of a file of dependencies, `a: b c`, reporting cycles: `Set` and `Map` as a graph, an exception `Cycle(List[Str])` that carries the cycle, and `try` restoring the state. About 60 lines; the core of a build system, without the processes that ulang cannot run.
6. **`markdown.u`**, Markdown to HTML, rendering `README.md`: text processing with a real use, but mostly strings, so it shows less of the types than the others.
7. **`datalog.u`**, a Datalog engine by semi-naive evaluation: datatypes, sets, and fixpoints.

### More ambitious

8. **`scheme.u`**, an interpreter of Scheme with tail calls and a REPL over a file.
9. **`ray.u`**, a ray tracer writing PPM `P3`, which is text, so no bytes are needed.
10. **`site.u`**, a static site generator that renders `README.md` and the docs to HTML, with their references as links; it builds on `markdown.u`.
11. **`tex.u`**, an expander of TeX macros, the core of TeX: catcodes, `\def` with patterns of parameters, `#1` and delimited arguments, `\let`, `\expandafter`, `\if…\else\fi`, counters, and groups `{…}` whose local definitions are undone at `}`, with the expanded text as output. Tokens and macros as datatypes and `Map`s, and expansion, which need not terminate, by procedures; the undoing at `}` is the idea of `try` restoring the variables, so one can be implemented by the other. About 400 to 600 lines. Full LaTeX is out of reach: `latex.ltx` needs the complete engine, and fonts, DVI, and PDF are bytes.
12. **`latex.u`**, LaTeX to HTML, a small pandoc: `\section`, `itemize` and `enumerate`, `\emph`, `\textbf`, `\href`, `\label` and `\ref` with numbering, and `\newcommand` by the expander of `tex.u` or a simpler one; it renders a real document and shares the parser with `tex.u`.
13. **`typeset.u`**, paragraphs broken by the algorithm of Knuth and Plass and justified as text of fixed width: dynamic programming over `Array`s, with output judged by eye. About 150 lines; a backend for `tex.u` or `latex.u`.

## An HTTP server

The language has no sockets and no reading from stdin, and strings are code points, not bytes. In the order of what it takes:

1. **Behind inetd**: `socat TCP-LISTEN:8080,fork EXEC:'out/ulang.sh run examples/http.u'` gives each connection a process whose stdin and stdout are the socket, and the program parses the request, routes it, and serves files or a generated page: a parser of requests, a `Map` of headers, routing by `match` on `(method, path)`, `OsError` as 404 or 500, and listings by `os_list`. About 150 lines. It reads the request by `io_read_line` of `io_stdin`, and its body by `io_read_bytes`. Limits: a process per request, and the start of the interpreter, seconds per request since it loads the library, milliseconds compiled.
2. **A server of its own**: builtins `io_listen(in port, out server)` and `io_accept(in server, out conn)` of `io.u`, whose connections are streams of `IO`, read and written as files are, with an abstract type `Server` in its `builtin` section, and a loop that accepts, serves, and closes, one connection at a time. Each builtin is added in all the places of Code 2 of `ideas/code.md`, `builtin_names` of `ulang/run/run.u`, `builtin_functions` and `native_procs` of `ulang/compile/lower.u`, their uses in `ulang/compile/x86.u`, the runtime, and `ulang.h`, and needs `make testc`. Networking is a new kind of effect that signatures do not show (Later 1). Best after Code 2 has reduced those places.
3. **A web app on top of 2**: notes or todos with HTML forms and a JSON API, kept in a file, which the semantics of `try` keep consistent on errors; it shares `json/` with `json.u` as a package that both import.
