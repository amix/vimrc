'use strict'
Object.defineProperty(exports, "__esModule", {value: true})
const tslib_1 = require("tslib")
const assert = tslib_1.__importStar(require("assert"))
const vscode_languageserver_1 = require("vscode-languageserver")
let connection = vscode_languageserver_1.createConnection()

let documents = new vscode_languageserver_1.TextDocuments()
documents.listen(connection)
connection.onInitialize((params) => {
  assert.equal(params.capabilities.workspace.applyEdit, true)
  assert.equal(params.capabilities.workspace.workspaceEdit.documentChanges, true)
  assert.deepEqual(params.capabilities.workspace.workspaceEdit.resourceOperations, [vscode_languageserver_1.ResourceOperationKind.Create, vscode_languageserver_1.ResourceOperationKind.Rename, vscode_languageserver_1.ResourceOperationKind.Delete])
  assert.equal(params.capabilities.workspace.workspaceEdit.failureHandling, vscode_languageserver_1.FailureHandlingKind.Undo)
  assert.equal(params.capabilities.textDocument.completion.completionItem.deprecatedSupport, true)
  assert.equal(params.capabilities.textDocument.completion.completionItem.preselectSupport, true)
  assert.equal(params.capabilities.textDocument.signatureHelp.signatureInformation.parameterInformation.labelOffsetSupport, true)
  assert.equal(params.capabilities.textDocument.rename.prepareSupport, true)
  let valueSet = params.capabilities.textDocument.completion.completionItemKind.valueSet
  assert.equal(valueSet[0], 1)
  assert.equal(valueSet[valueSet.length - 1], vscode_languageserver_1.CompletionItemKind.TypeParameter)
  let capabilities = {
    textDocumentSync: documents.syncKind,
    completionProvider: {resolveProvider: true, triggerCharacters: ['"', ':']},
    hoverProvider: true,
    renameProvider: {
      prepareProvider: true
    }
  }
  return {capabilities, customResults: {"hello": "world"}}
})
connection.onInitialized(() => {
  connection.sendDiagnostics({uri: "uri:/test.ts", diagnostics: []})
})
// Listen on the connection
connection.listen()
