import { Duplex } from 'stream'
import { createProtocolConnection, ProgressType, DocumentSymbolParams, DocumentSymbolRequest, InitializeParams, InitializeRequest, InitializeResult, ProtocolConnection, StreamMessageReader, StreamMessageWriter } from 'vscode-languageserver-protocol/node'
import { SymbolInformation, SymbolKind } from 'vscode-languageserver-types'
import { NullLogger } from '../../language-client/client'

class TestStream extends Duplex {
  public _write(chunk: string, _encoding: string, done: () => void): void {
    this.emit('data', chunk)
    done()
  }

  public _read(_size: number): void {
  }
}

let serverConnection: ProtocolConnection
let clientConnection: ProtocolConnection
let progressType: ProgressType<any> = new ProgressType()

beforeEach(() => {
  const up = new TestStream()
  const down = new TestStream()
  const logger = new NullLogger()
  serverConnection = createProtocolConnection(new StreamMessageReader(up), new StreamMessageWriter(down), logger)
  clientConnection = createProtocolConnection(new StreamMessageReader(down), new StreamMessageWriter(up), logger)
  serverConnection.listen()
  clientConnection.listen()
})

afterEach(() => {
  serverConnection.dispose()
  clientConnection.dispose()
})

describe('Connection Tests', () => {
  it('should ensure proper param passing', async () => {
    let paramsCorrect = false
    serverConnection.onRequest(InitializeRequest.type, params => {
      paramsCorrect = !Array.isArray(params)
      let result: InitializeResult = {
        capabilities: {
        }
      }
      return result
    })

    const init: InitializeParams = {
      rootUri: 'file:///home/dirkb',
      processId: 1,
      capabilities: {},
      workspaceFolders: null,
    }
    await clientConnection.sendRequest(InitializeRequest.type, init)
    expect(paramsCorrect).toBe(true)
  })

  it('should provide token', async () => {
    serverConnection.onRequest(DocumentSymbolRequest.type, params => {
      expect(params.partialResultToken).toBe('3b1db4c9-e011-489e-a9d1-0653e64707c2')
      return []
    })

    const params: DocumentSymbolParams = {
      textDocument: { uri: 'file:///abc.txt' },
      partialResultToken: '3b1db4c9-e011-489e-a9d1-0653e64707c2'
    }
    await clientConnection.sendRequest(DocumentSymbolRequest.type, params)
  })

  it('should report result', async () => {
    let result: SymbolInformation = {
      name: 'abc',
      kind: SymbolKind.Class,
      location: {
        uri: 'file:///abc.txt',
        range: { start: { line: 0, character: 1 }, end: { line: 2, character: 3 } }
      }
    }
    serverConnection.onRequest(DocumentSymbolRequest.type, params => {
      expect(params.partialResultToken).toBe('3b1db4c9-e011-489e-a9d1-0653e64707c2')
      serverConnection.sendProgress(progressType, params.partialResultToken, [result])
      return []
    })

    const params: DocumentSymbolParams = {
      textDocument: { uri: 'file:///abc.txt' },
      partialResultToken: '3b1db4c9-e011-489e-a9d1-0653e64707c2'
    }
    let progressOK = false
    clientConnection.onProgress(progressType, '3b1db4c9-e011-489e-a9d1-0653e64707c2', values => {
      progressOK = (values !== undefined && values.length === 1)
    })
    await clientConnection.sendRequest(DocumentSymbolRequest.type, params)
    expect(progressOK).toBeTruthy()
  })

  it('should provide workDoneToken', async () => {
    serverConnection.onRequest(DocumentSymbolRequest.type, params => {
      expect(params.workDoneToken).toBe('3b1db4c9-e011-489e-a9d1-0653e64707c2')
      return []
    })

    const params: DocumentSymbolParams = {
      textDocument: { uri: 'file:///abc.txt' },
      workDoneToken: '3b1db4c9-e011-489e-a9d1-0653e64707c2'
    }
    await clientConnection.sendRequest(DocumentSymbolRequest.type, params)
  })

  it('should report work done progress', async () => {
    serverConnection.onRequest(DocumentSymbolRequest.type, params => {
      expect(params.workDoneToken).toBe('3b1db4c9-e011-489e-a9d1-0653e64707c2')
      serverConnection.sendProgress(progressType, params.workDoneToken, {
        kind: 'begin',
        title: 'progress'
      })
      serverConnection.sendProgress(progressType, params.workDoneToken, {
        kind: 'report',
        message: 'message'
      })
      serverConnection.sendProgress(progressType, params.workDoneToken, {
        kind: 'end',
        message: 'message'
      })
      return []
    })

    const params: DocumentSymbolParams = {
      textDocument: { uri: 'file:///abc.txt' },
      workDoneToken: '3b1db4c9-e011-489e-a9d1-0653e64707c2'
    }
    let result = ''
    clientConnection.onProgress(progressType, '3b1db4c9-e011-489e-a9d1-0653e64707c2', value => {
      result += value.kind
    })
    await clientConnection.sendRequest(DocumentSymbolRequest.type, params)
    expect(result).toBe('beginreportend')
  })
})
