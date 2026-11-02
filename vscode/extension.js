// runs ulang files from the editor, shows their errors, and integrates their tests with the Testing view

const vscode = require("vscode");
const cp = require("child_process");
const fs = require("fs");
const path = require("path");

// e.g. "/a/b.u: type error: cannot unify Int and Bool", optionally followed by "at 3.11" and "in module"
const errorLine = /^(.*?): ((?:parse|type|runtime) error: .*?)(?: at (\d+)\.(\d+))?( in \w+)?$/;
const testLine = /^test (\S+) (passed|failed: (.*))$/;

function jar() {
  const configured = vscode.workspace.getConfiguration("ulang").get("jar");
  if (configured) return configured;
  for (const folder of vscode.workspace.workspaceFolders || []) {
    const candidate = path.join(folder.uri.fsPath, "ulang.jar");
    if (fs.existsSync(candidate)) return candidate;
  }
  return undefined;
}

// runs ulang.jar with args in the directory of file, resolving to its exit code, output, and errors
function ulang(args, file) {
  return new Promise((resolve) => {
    const path_ = jar();
    if (!path_) {
      vscode.window.showErrorMessage("ulang: no ulang.jar found, set ulang.jar in the settings");
      resolve(undefined);
      return;
    }
    const java = vscode.workspace.getConfiguration("ulang").get("java") || "java";
    // the library next to ulang.jar, as ulang.sh reads it, or else the one in the jar
    const lib = path.join(path.dirname(path_), "ulang", "resources");
    const props = fs.existsSync(lib) ? ["-Dulang.lib=" + lib] : [];
    cp.execFile(java, [...props, "-jar", path_, ...args], { cwd: path.dirname(file) }, (error, stdout, stderr) => {
      resolve({ code: error ? error.code || 1 : 0, stdout, stderr });
    });
  });
}

// the runs and tests of a document with their names, which ulang run and ulang test select
function blocks(document) {
  const result = [];
  for (let line = 0; line < document.lineCount; line++) {
    const match = /^(test|run)\s+(\w+)/.exec(document.lineAt(line).text);
    if (match) result.push({ kind: match[1], name: match[2], line });
  }
  return result;
}

function activate(context) {
  const output = vscode.window.createOutputChannel("ulang");
  const diagnostics = vscode.languages.createDiagnosticCollection("ulang");
  context.subscriptions.push(output, diagnostics);

  // errors of file in the error output of ulang, without a position at the start of the file
  function diagnose(file, stderr) {
    const found = [];
    for (const line of stderr.split("\n")) {
      const match = errorLine.exec(line);
      if (!match) continue;
      const position = match[3] ? new vscode.Position(Number(match[3]) - 1, Number(match[4]) - 1) : new vscode.Position(0, 0);
      const message = match[2] + (match[5] || "");
      found.push(new vscode.Diagnostic(new vscode.Range(position, position.translate(0, 1)), message, vscode.DiagnosticSeverity.Error));
    }
    diagnostics.set(vscode.Uri.file(file), found);
  }

  // ulang command file names..., e.g. ulang run file name, ulang test file name, or ulang check file
  async function execute(document, command, names, show) {
    if (document.isDirty) await document.save();
    const file = document.uri.fsPath;
    const result = await ulang([command, file, ...names], file);
    if (!result) return;
    diagnose(file, result.stderr);
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
    diagnose(file.uri.fsPath, result.stderr);
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
            const title = block.kind === "test" ? "▶ Run test" : "▶ Run";
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
