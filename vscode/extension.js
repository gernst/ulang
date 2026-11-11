// runs ulang files from the editor, shows their errors, and integrates their tests with the Testing view

const vscode = require("vscode");
const cp = require("child_process");
const fs = require("fs");
const path = require("path");

// e.g. "/a/b.u: type error: cannot unify Int and Bool", optionally followed by " in m" for an imported module m and
// " at 3.11", the position in the file of that module, with the kinds of error_kinds of ulang/syntax/error.u
const errorLine = /^(.*?): ((?:parse|type|format|runtime|internal) error: .*?)(?: in ([\w/-]+))?(?: at (\d+)\.(\d+))?$/;
const testLine = /^test (\S+) (passed|failed: (.*))$/;
// e.g. "lemma append_nil proved" or "lemma append_nil open: forall xs => ...", of ulang prove and ulang test
const lemmaLine = /^lemma (\S+) (?:proved|open: (.*))$/;

function binary() {
  const configured = vscode.workspace.getConfiguration("ulang").get("binary");
  if (configured) return configured;
  for (const folder of vscode.workspace.workspaceFolders || []) {
    const candidate = path.join(folder.uri.fsPath, "out", "ulang.bin");
    if (fs.existsSync(candidate)) return candidate;
  }
  return undefined;
}

// runs ulang.bin with args in the directory of file, resolving to its exit code, output, and errors,
// with a stack as large as the system allows, since the interpreter recurses as deeply as the programs it runs
function ulang(args, file) {
  return new Promise((resolve) => {
    const bin = binary();
    if (!bin) {
      vscode.window.showErrorMessage("ulang: no out/ulang.bin found, set ulang.binary in the settings");
      resolve(undefined);
      return;
    }
    const script = 'ulimit -s unlimited 2>/dev/null; exec "$0" "$@"';
    cp.execFile("sh", ["-c", script, bin, ...args], { cwd: path.dirname(file) }, (error, stdout, stderr) => {
      resolve({ code: error ? error.code || 1 : 0, stdout, stderr });
    });
  });
}

// the procedures without parameters, the tests, and the lemmas of a document with their names, which ulang run, ulang
// test, and ulang prove select
function blocks(document) {
  const result = [];
  for (let line = 0; line < document.lineCount; line++) {
    const text = document.lineAt(line).text;
    const test = /^test\s+(\w+)/.exec(text);
    const proc = /^procedure\s+(\w+)\(\s*\)\s*:=/.exec(text);
    if (test) result.push({ kind: "test", name: test[1], line });
    const lemma = /^lemma\s+(\w+)/.exec(text);
    if (proc) result.push({ kind: "run", name: proc[1], line });
    if (lemma) result.push({ kind: "prove", name: lemma[1], line });
  }
  return result;
}

// import, export, builtin, and implementation stand out in bold in the color of modules, whatever the theme, and the
// sections have a rule above them, since they divide a module into its interface, its builtins, and its implementation
function decorateSections(context) {
  const keyword = vscode.window.createTextEditorDecorationType({
    fontWeight: "bold",
    color: new vscode.ThemeColor("symbolIcon.moduleForeground"),
  });
  const section = vscode.window.createTextEditorDecorationType({
    isWholeLine: true,
    borderWidth: "1px 0 0 0",
    borderStyle: "solid",
    borderColor: new vscode.ThemeColor("editorRuler.foreground"),
  });

  // clears the decorations of editors that do not show ulang, e.g. after a change of the language mode
  function decorate(editor) {
    if (!editor) return;
    const keywords = [];
    const sections = [];
    if (editor.document.languageId === "ulang") {
      for (let line = 0; line < editor.document.lineCount; line++) {
        const match = /^(import|export|builtin|implementation)\b/.exec(editor.document.lineAt(line).text);
        if (!match) continue;
        keywords.push(new vscode.Range(line, 0, line, match[1].length));
        if (match[1] !== "import") sections.push(new vscode.Range(line, 0, line, 0));
      }
    }
    editor.setDecorations(keyword, keywords);
    editor.setDecorations(section, sections);
  }

  function decorateDocument(document) {
    for (const editor of vscode.window.visibleTextEditors) if (editor.document.uri.toString() === document.uri.toString()) decorate(editor);
  }

  // edits decorate their document once typing pauses, rather than at every keystroke
  const pending = new Map();
  function decorateLater(document) {
    clearTimeout(pending.get(document));
    pending.set(document, setTimeout(() => {
      pending.delete(document);
      decorateDocument(document);
    }, 100));
  }

  context.subscriptions.push(
    keyword,
    section,
    vscode.window.onDidChangeVisibleTextEditors((editors) => editors.forEach(decorate)),
    // a change of the language mode closes the document and opens it again with the new language
    vscode.workspace.onDidOpenTextDocument(decorateDocument),
    vscode.workspace.onDidChangeTextDocument((event) => decorateLater(event.document)),
    { dispose: () => pending.forEach(clearTimeout) }
  );
  vscode.window.visibleTextEditors.forEach(decorate);
}

// the root of the project of file, as the parser finds it: the nearest directory above it with project.u, else its own
// directory, where the modules of a directory with prelude.u, the library, are known as prelude/name, as from its parent
function projectRoot(file) {
  const home = path.dirname(path.resolve(file));
  if (fs.existsSync(path.join(home, "prelude.u"))) return path.dirname(home);
  for (let dir = home; ; dir = path.dirname(dir)) {
    if (fs.existsSync(path.join(dir, "project.u"))) return dir;
    if (dir === path.dirname(dir)) return home;
  }
}

function activate(context) {
  decorateSections(context);
  const output = vscode.window.createOutputChannel("ulang");
  const diagnostics = vscode.languages.createDiagnosticCollection("ulang");
  context.subscriptions.push(output, diagnostics);

  // errors of file in the error output of ulang, without a position at the start of the file, and those of another
  // module, known by its path from the root, at the import of its package, the directory of the module, or else at the
  // start, as for a sibling, pointing to the module; the lemmas that stdout states open are warnings at their lemma
  function diagnose(file, stderr, stdout) {
    const found = [];
    const lines = fs.existsSync(file) ? fs.readFileSync(file, "utf8").split("\n") : [];
    for (const line of stderr.split("\n")) {
      const match = errorLine.exec(line);
      if (!match) continue;
      const [, , message, module, row, column] = match;
      const at = row ? new vscode.Position(Number(row) - 1, Number(column) - 1) : new vscode.Position(0, 0);
      if (!module) {
        found.push(new vscode.Diagnostic(new vscode.Range(at, at.translate(0, 1)), message, vscode.DiagnosticSeverity.Error));
        continue;
      }
      const root = projectRoot(file);
      const imported = lines.findIndex((l) => {
        const named = /^import "(.*)"$/.exec(l.trim());
        return named && named[1] === path.posix.dirname(module);
      });
      const position = new vscode.Position(Math.max(imported, 0), 0);
      const text = message + " in " + module + (row ? " at " + row + "." + column : "");
      const diagnostic = new vscode.Diagnostic(new vscode.Range(position, position.translate(0, 1)), text, vscode.DiagnosticSeverity.Error);
      const source = [path.join(root, module + ".u")].find((f) => fs.existsSync(f)) || "";
      if (row && source) {
        diagnostic.relatedInformation = [new vscode.DiagnosticRelatedInformation(new vscode.Location(vscode.Uri.file(source), at), message)];
      }
      found.push(diagnostic);
    }
    for (const line of (stdout || "").split("\n")) {
      const match = lemmaLine.exec(line);
      if (!match || match[2] === undefined) continue;
      const row = lines.findIndex((l) => new RegExp("^lemma\\s+" + match[1] + "\\b").test(l));
      const at = new vscode.Position(Math.max(row, 0), 0);
      const range = new vscode.Range(at, at.translate(0, "lemma".length));
      found.push(new vscode.Diagnostic(range, "lemma " + match[1] + " open: " + match[2], vscode.DiagnosticSeverity.Warning));
    }
    diagnostics.set(vscode.Uri.file(file), found);
  }

  // ulang command file names..., e.g. ulang run file name, ulang test file name, ulang prove file name, or ulang check file
  async function execute(document, command, names, show) {
    if (document.isDirty) await document.save();
    const file = document.uri.fsPath;
    const result = await ulang([command, file, ...names], file);
    if (!result) return;
    diagnose(file, result.stderr, result.stdout);
    if (show) {
      output.clear();
      output.append(result.stdout);
      output.append(result.stderr);
      output.show(true);
    }
    await record(document);
  }

  // the results of all tests of document, as a test run, so that they show up in the editor and the Testing view
  async function record(document) {
    discover(document);
    const file = controller.items.get(document.uri.toString());
    if (!file || file.children.size === 0) return;
    const run = controller.createTestRun(new vscode.TestRunRequest([file]), undefined, false);
    const tests = [...file.children].map(([, child]) => child);
    await test(file, tests, run);
    run.end();
  }

  // runs the tests of file with ulang test, reporting the results of tests to run
  async function test(file, tests, run) {
    tests.forEach((t) => run.started(t));
    const result = await ulang(["test", file.uri.fsPath], file.uri.fsPath);
    if (!result) return;
    diagnose(file.uri.fsPath, result.stderr, result.stdout);
    const outcomes = new Map();
    for (const line of result.stdout.split("\n")) {
      const match = testLine.exec(line);
      if (match) outcomes.set(match[1], match[3]);
    }
    for (const t of tests) {
      const name = names.get(t.id);
      if (!outcomes.has(name)) {
        run.errored(t, new vscode.TestMessage(result.stderr.trim() || "the test was not run"));
      } else if (outcomes.get(name) === undefined) {
        run.passed(t);
      } else {
        const message = new vscode.TestMessage(outcomes.get(name));
        message.location = new vscode.Location(t.uri, t.range);
        run.failed(t, message);
      }
    }
  }

  function document(uri) {
    if (uri) return vscode.workspace.openTextDocument(uri);
    return vscode.window.activeTextEditor && vscode.window.activeTextEditor.document;
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("ulang.runFile", async (uri) => {
      const doc = await document(uri);
      if (doc) await execute(doc, "run", [], true);
    }),
    vscode.commands.registerCommand("ulang.runBlock", async (uri, kind, name) => {
      const doc = await document(uri);
      if (doc) await execute(doc, kind, [name], true);
    }),
    vscode.workspace.onDidSaveTextDocument((doc) => {
      if (doc.languageId === "ulang" && vscode.workspace.getConfiguration("ulang").get("checkOnSave")) execute(doc, "check", [], false);
    }),
    vscode.languages.registerCodeLensProvider(
      { language: "ulang" },
      {
        provideCodeLenses(doc) {
          return blocks(doc).map((block) => {
            const range = new vscode.Range(block.line, 0, block.line, 0);
            const title = { test: "▶ Run test", run: "▶ Run", prove: "▶ Prove" }[block.kind];
            return new vscode.CodeLens(range, { title, command: "ulang.runBlock", arguments: [doc.uri, block.kind, block.name] });
          });
        },
      }
    )
  );

  // the Testing view, with a test item for each file and each of its tests
  const controller = vscode.tests.createTestController("ulang", "ulang");
  context.subscriptions.push(controller);
  const names = new Map();

  function discover(doc) {
    if (doc.languageId !== "ulang" || doc.uri.scheme !== "file") return;
    const id = doc.uri.toString();
    const file = controller.items.get(id) || controller.createTestItem(id, vscode.workspace.asRelativePath(doc.uri), doc.uri);
    const tests = blocks(doc)
      .filter((block) => block.kind === "test")
      .map((block) => {
        const item = controller.createTestItem(`${id}#${block.name}`, block.name, doc.uri);
        item.range = new vscode.Range(block.line, 0, block.line, 4);
        names.set(item.id, block.name);
        return item;
      });
    file.children.replace(tests);
    if (tests.length > 0) controller.items.add(file);
    else controller.items.delete(id);
  }

  async function discoverAll() {
    for (const uri of await vscode.workspace.findFiles("**/*.u", "**/out/**")) {
      discover(await vscode.workspace.openTextDocument(uri));
    }
  }

  controller.resolveHandler = async (item) => {
    if (!item) await discoverAll();
  };
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument(discover),
    vscode.workspace.onDidChangeTextDocument((event) => discover(event.document))
  );
  for (const doc of vscode.workspace.textDocuments) discover(doc);

  controller.createRunProfile(
    "Run",
    vscode.TestRunProfileKind.Run,
    async (request, token) => {
      const run = controller.createTestRun(request);
      // the requested tests, grouped by their file
      const files = new Map();
      const requested = request.include || [...controller.items].map(([, item]) => item);
      for (const item of requested) {
        const file = item.parent || item;
        const tests = item.parent ? [item] : [...item.children].map(([, child]) => child);
        files.set(file, [...(files.get(file) || []), ...tests]);
      }
      for (const [file, tests] of files) {
        if (token.isCancellationRequested) break;
        const doc = await vscode.workspace.openTextDocument(file.uri);
        if (doc.isDirty) await doc.save();
        await test(file, tests, run);
      }
      run.end();
    },
    true
  );
}

function deactivate() {}

module.exports = { activate, deactivate };
