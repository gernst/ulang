// runs ulang files from the editor, shows their errors, and integrates their tests with the Testing view

const vscode = require("vscode");
const cp = require("child_process");
const fs = require("fs");
const path = require("path");

// e.g. "/a/b.u: type error: cannot unify Int and Bool", optionally followed by "at 3.11" and "in module"
const errorLine = /^(.*?): ((?:parse|type|runtime) error: .*?)(?: at (\d+)\.(\d+))?( in \w+)?$/;
const testLine = /^test (\d+) (passed|failed: (.*))$/;

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
    cp.execFile(java, ["-jar", path_, ...args], { cwd: path.dirname(file) }, (error, stdout, stderr) => {
      resolve({ code: error ? error.code || 1 : 0, stdout, stderr });
    });
  });
}

// the runs and tests of a document, numbered from 1 as by ulang --only and --tests
function blocks(document) {
  const result = [];
  for (let line = 0; line < document.lineCount; line++) {
    const match = /^(test|run)\b/.exec(document.lineAt(line).text);
    if (match) result.push({ kind: match[1], line, index: result.length + 1 });
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

  async function execute(document, args, show) {
    if (document.isDirty) await document.save();
    const file = document.uri.fsPath;
    const result = await ulang([...args, file], file);
    if (!result) return;
    diagnose(file, result.stderr);
    if (show) {
      output.clear();
      output.append(result.stdout);
      output.append(result.stderr);
      output.show(true);
    }
  }

  function document(uri) {
    if (uri) return vscode.workspace.openTextDocument(uri);
    return vscode.window.activeTextEditor && vscode.window.activeTextEditor.document;
  }

  context.subscriptions.push(
    vscode.commands.registerCommand("ulang.runFile", async (uri) => {
      const doc = await document(uri);
      if (doc) await execute(doc, [], true);
    }),
    vscode.commands.registerCommand("ulang.runBlock", async (uri, index) => {
      const doc = await document(uri);
      if (doc) await execute(doc, ["--only", String(index)], true);
    }),
    vscode.workspace.onDidSaveTextDocument((doc) => {
      if (doc.languageId === "ulang" && vscode.workspace.getConfiguration("ulang").get("checkOnSave")) execute(doc, [], false);
    }),
    vscode.languages.registerCodeLensProvider(
      { language: "ulang" },
      {
        provideCodeLenses(doc) {
          return blocks(doc).map((block) => {
            const range = new vscode.Range(block.line, 0, block.line, 0);
            const title = block.kind === "test" ? "▶ Run test" : "▶ Run";
            return new vscode.CodeLens(range, { title, command: "ulang.runBlock", arguments: [doc.uri, block.index] });
          });
        },
      }
    )
  );

  // the Testing view, with a test item for each file and each of its tests
  const controller = vscode.tests.createTestController("ulang", "ulang");
  context.subscriptions.push(controller);
  const indices = new Map();

  function discover(doc) {
    if (doc.languageId !== "ulang" || doc.uri.scheme !== "file") return;
    const id = doc.uri.toString();
    const file = controller.items.get(id) || controller.createTestItem(id, vscode.workspace.asRelativePath(doc.uri), doc.uri);
    const tests = blocks(doc)
      .filter((block) => block.kind === "test")
      .map((block) => {
        const item = controller.createTestItem(`${id}#${block.index}`, `test ${block.index}`, doc.uri);
        item.range = new vscode.Range(block.line, 0, block.line, 4);
        indices.set(item.id, block.index);
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
        tests.forEach((test) => run.started(test));
        const doc = await vscode.workspace.openTextDocument(file.uri);
        if (doc.isDirty) await doc.save();
        const result = await ulang(["--tests", file.uri.fsPath], file.uri.fsPath);
        if (!result) break;
        diagnose(file.uri.fsPath, result.stderr);
        const outcomes = new Map();
        for (const line of result.stdout.split("\n")) {
          const match = testLine.exec(line);
          if (match) outcomes.set(Number(match[1]), match[3]);
        }
        for (const test of tests) {
          const index = indices.get(test.id);
          if (!outcomes.has(index)) {
            run.errored(test, new vscode.TestMessage(result.stderr.trim() || "the test was not run"));
          } else if (outcomes.get(index) === undefined) {
            run.passed(test);
          } else {
            const message = new vscode.TestMessage(outcomes.get(index));
            message.location = new vscode.Location(test.uri, test.range);
            run.failed(test, message);
          }
        }
      }
      run.end();
    },
    true
  );
}

function deactivate() {}

module.exports = { activate, deactivate };
