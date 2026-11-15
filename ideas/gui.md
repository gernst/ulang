# GUI

A plan for graphical interfaces, not started: programs of ulang have no windows, so the examples of `ideas/examples.md` stop at the terminal. The ulang side is functional, in the architecture of Elm: a program is a model, a pure `update` of the model by an event, and a pure `view` of the model as a tree of widgets, which a loop in ulang compares with the tree before and turns into patches of the widgets on screen. The toolkit is GTK 4 (4.24 here), reached through a few builtin procedures of the runtime that never call back into ulang.

## Why GTK 4

- Retained widgets with their own layout, text entry, scrolling, focus, accessibility, and themes, which a diff of trees drives directly, where SDL or raylib would draw and lay out everything themselves, without input methods or text editing.
- A C API, which the runtime calls as it calls GMP, while Qt is C++ and Tk brings an interpreter of Tcl; cairo for a canvas; Wayland and X11 alike, also macOS.
- The patches of the protocol (item 2) do not depend on GTK, so a second backend can serve the same ulang code later, e.g. a browser over `io_listen` of `ulang/prelude/io.u` that renders the patches as DOM, without new builtins on the side of ulang.

## Design

- **No callbacks into ulang.** GTK signals only queue raw events, numbered by the widget, in the runtime, and a builtin procedure that waits for the next event iterates the main context of GLib until the queue has one. So no `longjmp` of an exception of ulang crosses a frame of GTK, the interpreter needs no reentry, and the loop of the program, its state, and its errors stay in ulang, as the loops of a server over `io_accept` do.
- **Functional surface.** A program is `App(title, init, update, view)` with `update: (E, M) -> M` and `view: M -> Widget[E]`, where `E` is the type of the events of the program, the messages of Elm. Effects are data that `update` returns, item 4, so update and view stay functions, tested and proven as functions are.
- **Widgets are built as in Elm.** `Widget[E]` and `Attr[E]` are abstract types, built only by functions, each element of a list of attributes and, where it has them, a list of children, as `button [ onClick Inc ] [ text "+" ]` of Elm's `Html`; events are attributes, as values, `on_click(Inc)`, or as functions for payloads, `on_input(lambda s => Typed(s))`, and a component of its own events is embedded by `map_events(f, w)`, as `Html.map` of Elm, which maps the handlers of the tree. A counter:

  ```
  data Msg :=
    case Inc
    case Dec

  function update(msg: Msg, n: Int): Int :=
    match msg
    case Inc =>
      n + 1
    case Dec =>
      n - 1
    end

  function view(n: Int): Widget[Msg] :=
    column(
      List(spacing(8)),
      List(
        button(List(on_click(Dec)), List(text("-"))),
        text(show(n)),
        button(List(on_click(Inc)), List(text("+")))
      )
    )

  procedure main() raises GuiError :=
    gui_run(in simple("Counter", 0, update, view))
  ```
- **The diff is pure and in ulang.** `view` gives a `Widget[E]`; its skeleton, the tree without the handlers, a `Node` of kinds and properties, is what the diff compares, so that the diff, its patches, and a model of the toolkit are values of enumerable types, which lemmas and their tests reach (item 3), while the handlers are looked up in the current tree by the id of the widget that the raw event names.
- **The garbage collector sees everything.** GTK allocates by `malloc`, which the Boehm collector does not scan, so the runtime keeps no value of ulang in a widget: each widget has an `Int` id, its properties are set as C strings and numbers, and what a canvas draws, its list of shapes, is held by the object of the window, allocated by the collector, keyed by the id, and read by the draw function of the drawing area.
- **GTK is loaded on demand.** `ulang/runtime/gui.c` opens `libgtk-4.so.1` by `dlopen` on the first `gui_open`, with the 40 or so functions it calls declared in the runtime, as Build 3 proposes for GMP and the collector, so that neither `out/ulang.bin` nor a compiled program needs GTK to build or to run unless it opens a window, and a machine without it raises `GuiError` at `gui_open`. The interpreter calls the procedures of its host, as it calls `io_open` of `ulang/prelude/io.u` (`builtin_names` of `ulang/run/run.u`), so `ulang.sh run` shows windows too.

## Items

1. **The library has no package that a program imports explicitly**: every member of `ulang/prelude/` is imported by every module (`implicit_imports` of `ulang/syntax/syntax.u`), so functions such as `button`, `text`, and `column` and the builtins of GTK would be seen everywhere, and `import "prelude"` is rejected. Solution: subdirectories of the library as packages of their own, imported by `import "prelude/gui"`, loaded from the directory that `os_library_dir` of `ulang/prelude/os.u` gives, never implicitly, with the parser accepting that prefix only for directories below the library (`import_package` of `ulang/parse/parse.u`), as the tester already loads `ulang/test/enumerate/` next to the library (`load_after_library` of `ulang/parse/parse.u`); its modules are `prelude/gui/gui` and so on, shown so in messages and kept by their file names in C. The tests of the package live in `ulang/prelude/gui/test/`.
2. **The builtins and the protocol**, in the builtin section of `ulang/prelude/gui/gui.u`, each in both backends, where the runtime reads a datatype by its tags and fields as it reads `IoMode`:
   - `type Window`, ordered by opening and shown as `<window title>`, and `data GuiError := GuiError(message: Str)`.
   - `gui_open(in title: Str, width: Int, height: Int, out window: Window) raises GuiError`, `gui_close(in window: Window)`.
   - `gui_apply(in window: Window, patches: List[Patch]) raises GuiError`, where a `Patch` is `PCreate(id, parent, index, kind)`, `PSet(id, prop)`, `PMove(id, index)`, and `PRemove(id)`, a `Kind` is one of the widgets below, and a `Prop` one of `PText`, `PChecked`, `PSensitive`, `PSpacing`, `PSize`, and `PShapes(shapes)` of a canvas; an unknown id or a property that its kind lacks is `GuiError`, a defect of the diff that the lemmas of item 3 rule out.
   - `gui_next(in window: Window, out event: RawEvent) raises GuiError`, which waits, with `RClicked(id)`, `RChanged(id, text)`, `RToggled(id, on)`, `RPointer(id, x, y, button)`, `RKey(id, name)`, `RTick(id)`, and `RClosed`; consecutive `RChanged` of one entry and `RPointer` motions are merged in the queue, so that a slow `view` does not fall behind.
   - Widgets of the first version, functions in `ulang/prelude/gui/widget.u`: `text(s)`; `button(attrs, children)` (`GtkButton` with its children as its child); `entry(attrs)`; `check(attrs, children)`; `column(attrs, children)` and `row(attrs, children)` (`GtkBox`); `scroll(attrs, children)`; and `canvas(attrs, shapes)` (`GtkDrawingArea` drawn by cairo from shapes, `rect`, `circle`, `line`, and `label`, with colors as `Rgb(r, g, b)`). Attributes: `on_click(e)`, `on_input(f)`, `on_toggle(f)`, `on_pointer(f)`, `on_key(f)`, `value(s)`, `checked(b)`, `disabled(b)`, `spacing(n)`, `size(width, height)`, `expand(b)`, and `key(k)` of children (item 3). Inside, `data Widget[E] := Widget(kind: Kind, attrs: List[Attr[E]], children: List[Widget[E]])` and `Attr[E]` a datatype of the properties and the handlers, both private to the package, so that a new kind or attribute changes no program. Grids, lists of many rows (`GtkListView`), menus, and dialogs later, each a function and a kind of the protocol.
   - An attribute that its widget lacks, `spacing` of a `button`, which Elm ignores, raises `GuiError` in `gui_run` at the first view that has it, naming the widget and the attribute, by the pure `misplaced(w): List[Str]`, which tests call too, since a silently ignored attribute is a trip-up that nothing reports.
3. **The diff and its proofs**, functions of `ulang/prelude/gui/diff.u`: `skeleton(w: Widget[E]): Node`; `placed(next: Int, node: Node)`, which numbers the nodes; `diff(old: Placed, new: Node, next: Int): (List[Patch], Placed)`, which keeps the id of a node of the same kind at the same place among its siblings, sets the properties that changed, and creates and removes the rest, children by position, with keys of children (the attribute `key(k)`, as `Html.Keyed` of Elm) as a second step for lists that insert in the middle; and `realize(patches, screen)`, a pure model of what `gui_apply` does to the tree of widgets, which is what the lemmas speak of:
   - `lemma diff_realizes(old: Placed, new: Node) := ensures realize(first(diff(old, new, fresh(old))), screen_of(old)) == screen_of(second(diff(…)))`, i.e. the screen after the patches is the new tree, and no patch names an id that the screen lacks.
   - `diff(p, skeleton_of(p))` is empty, so a `view` that gives the same tree touches no widget.
   - Their tests `l.lemma` enumerate small trees, which a `Node` of kinds and texts allows, and `check --prove` may prove them by induction on the tree; a gap is no reason to stop, the tests and examples carry the work meanwhile.
4. **The loop and effects**, procedures of `ulang/prelude/gui/gui.u`: `gui_run[M, E](in app: App[M, E]) raises GuiError` opens the window, applies the patches of the first view, and then repeats `gui_next`, the lookup of the handler of the raw event in the current tree, `update`, `view`, `diff`, and `gui_apply` until `RClosed` or a `Quit`; an event that no handler takes, of a widget that the last patches removed, is dropped. Effects as data, Elm's commands: `update` of an `App` with effects returns `(M, List[Effect[E]])`, with `Quit`, `Every(milliseconds, E)` (a timer of GLib, raising `RTick`), `ReadFile(path, Result -> E)`, and `WriteFile(path, text, E)`, which the loop performs between events, its `OsError` turned into an event, so a program reads and writes files without a procedure of its own; an `App` without effects is the case of an empty list, built by a function `simple(title, init, update, view)`.
5. **Testing without a screen**, which agents need, since they see no window: a pure `simulate(app, events: List[E]): (M, Widget[E])` and the finders `click(caption, w): Option[E]`, the event of the button whose text is the caption, and `input(caption, s, w): Option[E]`, that of the entry after it, in `ulang/prelude/gui/test.u`, so that a test clicks by the text of a button, `assert second(simulate(counter(), List(Inc, Inc))) …`, without GTK; and for the runtime, `ULANG_GUI_SHOT=<dir>` of the environment, which makes `gui_apply` write the window as PNG after each apply (`gtk_widget_snapshot` into a texture, `gdk_texture_save_to_png`), and `ULANG_GUI_EVENTS=<file>`, which feeds `gui_next` from a file of raw events instead of the display, so that a run is scripted end to end under `GDK_BACKEND=broadway` or a headless compositor and its shots read back as images. A target `make testgui` runs the programs of `ulang/prelude/gui/test/` compiled so, only after asking, as the other compiled tests are.
6. **Examples**: a counter, the smallest program; a list of tasks with an entry, checks, and a filter, which shows keyed children and `ReadFile` and `WriteFile`; the game of life on a canvas with `Every`, which shows shapes and pointer events; and later a viewer of the outline of a ulang file by `refactor @outline`, a first tool of the language with a window. Each joins `examples/` with its tests by `simulate`, see `ideas/examples.md`.

## Try-out

The ulang side runs in the top-level directory `gui/`, without any change to `ulang/`: GTK is in a host process instead of the runtime, `gui/host/host.py` of PyGObject, which listens on a port, starts the program with the port as its argument, and speaks the protocol of item 2 as lines of fields separated by tabs over TCP, which the program reaches by `io_connect` of `ulang/prelude/io.u`; so no builtin is new, and interpreted and compiled programs alike open windows.

- `gui/protocol.u`: kinds, properties, shapes, the skeletons `Node` and `Placed`, the patches `Op`, the events, `create` and `diff` by position, and the lines of both directions; `gui/widget.u`: the abstract `Widget[E]` and `Attr[E]`, their builders as in Elm, `map_events`, `skeleton`, `misplaced`, `handle`, and the finders `click` and `input` for tests; `gui/app.u`: `App`, `simulate`, and `gui_run`, which raises `GuiError` for a misplaced attribute.
- `gui/examples/`: `counter.u`, `todo.u` (an entry, checks, a filter, with checks reused by the diff when the filter hides a row), and `life.u` (a canvas with pointer events), each tested by `simulate`, `click`, and `input`.
- `python3 gui/host/host.py gui/examples/todo.u` runs one on the screen; `--script FILE` drives it by commands after each commit and writes shots as PNG, `xvfb-run -a` around it without a screen, which is how the try-out was checked, also of a binary compiled by `compile`; about 5 seconds a script, mostly the start of Xvfb and the pause of 150 ms after each commit.
- Learned: the diff by position is enough for these examples, since the handlers are looked up in the current tree, so a reused widget gets the event of its new place; `expand` must grow along the axis of the box around it, else an entry in a row grows tall; and the host only queues events, so a slow `update` loses nothing.
- Open in the try-out: keys of children, effects and timers (item 4), the lemma of `realize` (item 3), and the runtime instead of the host, which items 1 and 2 still plan; or the host kept, as the backend of a browser would be another host of the same lines.

## Steps

1. Item 1, the package `prelude/gui` with an empty builtin section and a test, which `make test` runs.
2. Item 3 alone, the widgets, nodes, patches, `diff`, and `realize`, all pure, with examples, tests, and lemmas, before any C.
3. `ulang/runtime/gui.c` with `dlopen`, `gui_open`, `gui_apply`, `gui_next`, and `gui_close` for text, buttons, boxes, and entries, the cases of `ulang/run/run.u`, the names of `ulang/compile/lower.u` and of `ulang/runtime/ulang.h`, and the layouts in `ulang/compile/x86.u` for the assembly backend, then `make testc` and `make testelf` after asking; then the counter runs, interpreted and compiled.
4. `gui_run` without effects and `simulate`; then the canvas and pointer events; then effects and timers; then shots and scripted events (item 5); then the examples.
5. `CLAUDE.md`, Library, the package and its rules, and `LANGUAGE.md` a section of programs with windows, when the counter runs; this file deleted when the examples run.

## Open

- Whether `update` gets the effects in every `App`, Elm's choice, or only in a second constructor, as item 4 assumes, so that the first programs are shorter.
- Whether the queue of raw events should also be reachable without a window, so that `gui_next` serves timers and sockets together, which Library 1, `io_poll`, would join.
- The names of the builders follow Elm, `text` among them, which a local `text` hides, a common name of locals, so that `text(s)` then calls the local; `HiddenLocal` does not catch it, since locals may hide declarations, so either the error of the call names the hidden function, or the builder is `label`.
- Capabilities (Later 1 and 4): opening a window is an effect that the signature of `gui_run` shows only by `GuiError`.
