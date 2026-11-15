# Ideas for examples

Programs that show ulang on tasks of real software, beside `examples/words.u`, the most frequent words of files, `examples/csv.u`, the relational algebra on CSV files over `library/csv`, `examples/grep.u`, over the regular expressions of `library/regex`, `examples/jq.u`, queries on JSON over `library/json`, and `examples/md2html.u`, Markdown to HTML over `library/md`.
What `os` and `io` offer bounds them: files, streams, arguments, printing, directories, and bytes, but no processes, clock, or network, so most are tools from text or bytes to text.

## Within the language as it is

1. **`diff.u`**, a diff of the lines of two files by Myers' algorithm, printed as `diff -u` does: `Array` for random access, a search by `while` over a `ref` state, and output that `diff` checks. About 80 lines. The `format error` of `check`, which reports the first line that differs, could use it.
2. **`calc.u`**, a calculator of its argument, `run calc.u -- "2 ^ 100 / 3 + x where x = 7"`: notations `infix` of the language itself, pattern aliases over the tree of expressions, unbounded integers, and `DivisionByZero` caught. About 70 lines; features that mainstream languages do not have in this form.
3. **`tree.u`**, like `tree` or `du`, the files of a directory with their counts of files and lines: `os_walk`, `os_list`, `os_is_dir`, recursion over the file system by procedures, and `Buffer` for the output. About 50 lines; run on `ulang/`, it shows the size of the compiler.
4. **`topo.u`**, a topological sort of a file of dependencies, `a: b c`, reporting cycles: `Set` and `Map` as a graph, an exception `Cycle(List[Str])` that carries the cycle, and `try` restoring the state. About 60 lines; the core of a build system, without the processes that ulang cannot run.
5. **`datalog.u`**, a Datalog engine by semi-naive evaluation: datatypes, sets, and fixpoints.

### More ambitious

6. **`scheme.u`**, an interpreter of Scheme with tail calls and a REPL over a file.
7. **`ray.u`**, a ray tracer writing PPM `P3`, which is text, so no bytes are needed.
8. **`site.u`**, a static site generator that renders `README.md` and the docs to HTML, with their references as links; it builds on `library/md`.
9. **`tex.u`**, an expander of TeX macros, the core of TeX: catcodes, `\def` with patterns of parameters, `#1` and delimited arguments, `\let`, `\expandafter`, `\if…\else\fi`, counters, and groups `{…}` whose local definitions are undone at `}`, with the expanded text as output. Tokens and macros as datatypes and `Map`s, and expansion, which need not terminate, by procedures; the undoing at `}` is the idea of `try` restoring the variables, so one can be implemented by the other. About 400 to 600 lines. Full LaTeX is out of reach: `latex.ltx` needs the complete engine, and fonts, DVI, and PDF are bytes.
10. **`latex.u`**, LaTeX to HTML, a small pandoc: `\section`, `itemize` and `enumerate`, `\emph`, `\textbf`, `\href`, `\label` and `\ref` with numbering, and `\newcommand` by the expander of `tex.u` or a simpler one; it renders a real document and shares the parser with `tex.u`.
11. **`typeset.u`**, paragraphs broken by the algorithm of Knuth and Plass and justified as text of fixed width: dynamic programming over `Array`s, with output judged by eye. About 150 lines; a backend for `tex.u` or `latex.u`.

## An HTTP server

The language has no sockets, while `io` reads and writes the streams of the system as bytes. In the order of what it takes:

1. **Behind inetd**: `socat TCP-LISTEN:8080,fork EXEC:'out/ulang.sh run examples/http.u'` gives each connection a process whose stdin and stdout are the socket, and the program parses the request, routes it, and serves files or a generated page: a parser of requests, a `Map` of headers, routing by `match` on `(method, path)`, `OsError` as 404 or 500, and listings by `os_list`. About 150 lines. It reads the request by `io_read_line` of `io_stdin`, and its body by `io_read_bytes`. Limits: a process per request, and the start of the interpreter, seconds per request since it loads the library, milliseconds compiled.
2. **A server of its own**: `io_listen` and `io_accept` of Library 1, whose connections are streams of `IO`, read and written as files are, and a loop that accepts, serves, and closes, one connection at a time.
3. **A web app on top of 2**: notes or todos with HTML forms and a JSON API, kept in a file, which the semantics of `try` keep consistent on errors; it shares `library/json` with `examples/jq.u`.
