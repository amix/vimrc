import { CompletionTriggerKind, Position, TextDocumentItem, TextDocumentSaveReason } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import * as cv from '../../language-client/utils/converter'

describe('converter', () => {

  function createDocument(): TextDocument {
    return TextDocument.create('file:///1', 'css', 1, '')
  }

  it('should convertToTextDocumentItem', () => {
    let doc = createDocument()
    expect(cv.convertToTextDocumentItem(doc).uri).toBe(doc.uri)
    expect(TextDocumentItem.is(cv.convertToTextDocumentItem(doc))).toBe(true)
  })

  it('should asCloseTextDocumentParams', () => {
    let doc = createDocument()
    expect(cv.asCloseTextDocumentParams(doc).textDocument.uri).toBe(doc.uri)
  })

  it('should asChangeTextDocumentParams', () => {
    let doc = createDocument()
    expect(cv.asChangeTextDocumentParams(doc).textDocument.uri).toBe(doc.uri)
  })

  it('should asWillSaveTextDocumentParams', () => {
    let res = cv.asWillSaveTextDocumentParams({ document: createDocument(), reason: TextDocumentSaveReason.Manual, waitUntil: () => {} })
    expect(res.textDocument).toBeDefined()
    expect(res.reason).toBeDefined()
  })

  it('should asVersionedTextDocumentIdentifier', () => {
    let res = cv.asVersionedTextDocumentIdentifier(createDocument())
    expect(res.uri).toBeDefined()
    expect(res.version).toBeDefined()
  })

  it('should asSaveTextDocumentParams', () => {
    let res = cv.asSaveTextDocumentParams(createDocument(), true)
    expect(res.textDocument.uri).toBeDefined()
    expect(res.text).toBeDefined()
    res = cv.asSaveTextDocumentParams(createDocument(), false)
    expect(res.text).toBeUndefined()
  })

  it('should asUri', () => {
    let uri = URI.file('/tmp/a')
    expect(cv.asUri(uri)).toBe(uri.toString())
  })

  it('should asCompletionParams', () => {
    let params = cv.asCompletionParams(createDocument(), Position.create(0, 0), { triggerKind: CompletionTriggerKind.Invoked })
    expect(params.textDocument).toBeDefined()
    expect(params.position).toBeDefined()
    expect(params.context).toBeDefined()
  })

  it('should asTextDocumentPositionParams', () => {
    let params = cv.asTextDocumentPositionParams(createDocument(), Position.create(0, 0))
    expect(params.textDocument).toBeDefined()
    expect(params.position).toBeDefined()
  })

  it('should asTextDocumentIdentifier', () => {
    let doc = cv.asTextDocumentIdentifier(createDocument())
    expect(doc.uri).toBeDefined()
  })

  it('should asReferenceParams', () => {
    let params = cv.asReferenceParams(createDocument(), Position.create(0, 0), { includeDeclaration: false })
    expect(params.textDocument.uri).toBeDefined()
    expect(params.position).toBeDefined()
  })

  it('should asDocumentSymbolParams', () => {
    let doc = cv.asDocumentSymbolParams(createDocument())
    expect(doc.textDocument.uri).toBeDefined()
  })

  it('should asCodeLensParams', () => {
    let doc = cv.asCodeLensParams(createDocument())
    expect(doc.textDocument.uri).toBeDefined()
  })
})
