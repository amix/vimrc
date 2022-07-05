const languageserver = require('vscode-languageserver')
let connection = languageserver.createConnection()
let documents = new languageserver.TextDocuments()
documents.listen(connection)

connection.onInitialize(() => {
  let capabilities = {
    textDocumentSync: documents.syncKind
  }
  return { capabilities }
})

connection.onInitialized(() => {
  connection.sendRequest('client/registerCapability', {
    registrations: [{
      id: 'didChangeWatchedFiles',
      method: 'workspace/didChangeWatchedFiles',
      registerOptions: {
        watchers: [{ globPattern: "**" }]
      }
    }]
  })
})

let received

connection.onNotification('workspace/didChangeWatchedFiles', params => {
  received = params
})

connection.onRequest('custom/received', async () => {
  return received
})

connection.listen()
