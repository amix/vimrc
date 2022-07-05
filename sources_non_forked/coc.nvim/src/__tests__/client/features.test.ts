import * as assert from 'assert'
import path from 'path'
import { URI } from 'vscode-uri'
import { LanguageClient, ServerOptions, TransportKind, Middleware, LanguageClientOptions } from '../../language-client/index'
import { CancellationTokenSource, Color, DocumentSelector, Position, Range, DefinitionRequest, Location, HoverRequest, Hover, CompletionRequest, CompletionTriggerKind, CompletionItem, SignatureHelpRequest, SignatureHelpTriggerKind, SignatureInformation, ParameterInformation, ReferencesRequest, DocumentHighlightRequest, DocumentHighlight, DocumentHighlightKind, CodeActionRequest, CodeAction, WorkDoneProgressBegin, WorkDoneProgressReport, WorkDoneProgressEnd, ProgressToken, DocumentFormattingRequest, TextEdit, DocumentRangeFormattingRequest, DocumentOnTypeFormattingRequest, RenameRequest, WorkspaceEdit, DocumentLinkRequest, DocumentLink, DocumentColorRequest, ColorInformation, ColorPresentation, DeclarationRequest, FoldingRangeRequest, FoldingRange, ImplementationRequest, SelectionRangeRequest, SelectionRange, TypeDefinitionRequest, ProtocolRequestType, CallHierarchyPrepareRequest, CallHierarchyItem, CallHierarchyIncomingCall, CallHierarchyOutgoingCall, SemanticTokensRegistrationType, LinkedEditingRangeRequest, WillCreateFilesRequest, DidCreateFilesNotification, WillRenameFilesRequest, DidRenameFilesNotification, WillDeleteFilesRequest, DidDeleteFilesNotification, TextDocumentEdit, CancellationToken } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import helper from '../helper'
import workspace from '../../workspace'

describe('Client integration', () => {
  let client!: LanguageClient
  let middleware: Middleware
  let uri!: string
  let document!: TextDocument
  let tokenSource!: CancellationTokenSource
  const position: Position = Position.create(1, 1)
  const range: Range = Range.create(1, 1, 1, 2)

  function rangeEqual(range: Range, sl: number, sc: number, el: number, ec: number): void {
    assert.strictEqual(range.start.line, sl)
    assert.strictEqual(range.start.character, sc)
    assert.strictEqual(range.end.line, el)
    assert.strictEqual(range.end.character, ec)
  }

  function colorEqual(color: Color, red: number, green: number, blue: number, alpha: number): void {
    assert.strictEqual(color.red, red)
    assert.strictEqual(color.green, green)
    assert.strictEqual(color.blue, blue)
    assert.strictEqual(color.alpha, alpha)
  }

  function uriEqual(actual: string, expected: string): void {
    assert.strictEqual(actual, expected)
  }

  function isArray<T>(value: Array<T> | undefined | null, clazz: any, length = 1): asserts value is Array<T> {
    assert.ok(Array.isArray(value), `value is array`)
    assert.strictEqual(value!.length, length, 'value has given length')
    if (clazz && typeof clazz.is === 'function') {
      for (let item of value) {
        assert.ok(clazz.is(item))
      }
    }
  }

  function isDefined<T>(value: T | undefined | null): asserts value is Exclude<T, undefined | null> {
    if (value === undefined || value === null) {
      throw new Error(`Value is null or undefined`)
    }
  }

  beforeAll(async () => {
    await helper.setup()
    workspace.registerTextDocumentContentProvider('lsptests', {
      provideTextDocumentContent: (_uri: URI) => {
        return [
          'REM @ECHO OFF',
          'cd c:\\source',
          'REM This is the location of the files that you want to sort',
          'FOR %%f IN (*.doc *.txt) DO XCOPY c:\\source\\"%%f" c:\\text /m /y',
          'REM This moves any files with a .doc or',
          'REM .txt extension from c:\\source to c:\\text',
          'REM %%f is a variable',
          'FOR %%f IN (*.jpg *.png *.bmp) DO XCOPY C:\\source\\"%%f" c:\\images /m /y',
          'REM This moves any files with a .jpg, .png,',
          'REM or .bmp extension from c:\\source to c:\\images;;',
        ].join('\n')
      }
    })

    uri = URI.parse('lsptests://localhost/test.bat').toString()
    let doc = await workspace.loadFile(uri.toString())
    document = doc.textDocument
    tokenSource = new CancellationTokenSource()
    const serverModule = path.join(__dirname, './server/testServer.js')
    const serverOptions: ServerOptions = {
      run: { module: serverModule, transport: TransportKind.ipc },
      debug: { module: serverModule, transport: TransportKind.ipc, options: { execArgv: ['--nolazy', '--inspect=6014'] } }
    }
    const documentSelector: DocumentSelector = [{ scheme: 'lsptests' }]

    middleware = {}
    const clientOptions: LanguageClientOptions = {
      documentSelector, synchronize: {}, initializationOptions: {}, middleware
    }

    client = new LanguageClient('test svr', 'Test Language Server', serverOptions, clientOptions)
    client.start()
    await client.onReady()
  })

  afterAll(async () => {
    await client.stop()
    await helper.shutdown()
  })

  test('InitializeResult', () => {
    let expected = {
      capabilities: {
        textDocumentSync: 1,
        definitionProvider: true,
        hoverProvider: true,
        completionProvider: { resolveProvider: true, triggerCharacters: ['"', ':'] },
        signatureHelpProvider: {
          triggerCharacters: [':'],
          retriggerCharacters: [':']
        },
        referencesProvider: true,
        documentHighlightProvider: true,
        codeActionProvider: {
          resolveProvider: true
        },
        documentFormattingProvider: true,
        documentRangeFormattingProvider: true,
        documentOnTypeFormattingProvider: {
          firstTriggerCharacter: ':'
        },
        renameProvider: {
          prepareProvider: true
        },
        documentLinkProvider: {
          resolveProvider: true
        },
        colorProvider: true,
        declarationProvider: true,
        foldingRangeProvider: true,
        implementationProvider: true,
        selectionRangeProvider: true,
        typeDefinitionProvider: true,
        callHierarchyProvider: true,
        semanticTokensProvider: {
          legend: {
            tokenTypes: [],
            tokenModifiers: []
          },
          range: true,
          full: {
            delta: true
          }
        },
        workspace: {
          fileOperations: {
            didCreate: { filters: [{ scheme: 'file', pattern: { glob: '**/created-static/**{/,/*.txt}' } }] },
            didRename: {
              filters: [
                { scheme: 'file', pattern: { glob: '**/renamed-static/**/', matches: 'folder' } },
                { scheme: 'file', pattern: { glob: '**/renamed-static/**/*.txt', matches: 'file' } }
              ]
            },
            didDelete: { filters: [{ scheme: 'file', pattern: { glob: '**/deleted-static/**{/,/*.txt}' } }] },
            willCreate: { filters: [{ scheme: 'file', pattern: { glob: '**/created-static/**{/,/*.txt}' } }] },
            willRename: {
              filters: [
                { scheme: 'file', pattern: { glob: '**/renamed-static/**/', matches: 'folder' } },
                { scheme: 'file', pattern: { glob: '**/renamed-static/**/*.txt', matches: 'file' } }
              ]
            },
            willDelete: { filters: [{ scheme: 'file', pattern: { glob: '**/deleted-static/**{/,/*.txt}' } }] },
          },
        },
        linkedEditingRangeProvider: true
      },
      customResults: {
        hello: 'world'
      }
    }
    assert.deepEqual(client.initializeResult, expected)
  })

  test('Goto Definition', async () => {
    const provider = client.getFeature(DefinitionRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideDefinition(document, position, tokenSource.token)) as Location
    assert.strictEqual(Location.is(result), true)
    uriEqual(result.uri, uri)
    rangeEqual(result.range, 0, 0, 0, 1)
    let middlewareCalled = false
    middleware.provideDefinition = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideDefinition(document, position, tokenSource.token)
    middleware.provideDefinition = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Hover', async () => {
    const provider = client.getFeature(HoverRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideHover(document, position, tokenSource.token)
    assert.ok(Hover.is(result))
    assert.strictEqual((result.contents as any).kind, 'plaintext')
    assert.strictEqual((result.contents as any).value, 'foo')
    let middlewareCalled = false
    middleware.provideHover = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideHover(document, position, tokenSource.token)
    middleware.provideHover = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Completion', async () => {
    const provider = client.getFeature(CompletionRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideCompletionItems(document, position, tokenSource.token, { triggerKind: CompletionTriggerKind.Invoked, triggerCharacter: ':' })) as CompletionItem[]

    isArray(result, CompletionItem)
    const item = result[0]
    assert.strictEqual(item.label, 'item')
    assert.strictEqual(item.insertText, 'text')
    assert.strictEqual(item.detail, undefined)
    isDefined(provider.resolveCompletionItem)

    const resolved = await provider.resolveCompletionItem(item, tokenSource.token)
    isDefined(resolved)
    assert.strictEqual(resolved.detail, 'detail')

    let middlewareCalled = 0
    middleware.provideCompletionItem = (document, position, context, token, next) => {
      middlewareCalled++
      return next(document, position, context, token)
    }
    middleware.resolveCompletionItem = (item, token, next) => {
      middlewareCalled++
      return next(item, token)
    }
    await provider.provideCompletionItems(document, position, tokenSource.token, { triggerKind: CompletionTriggerKind.Invoked, triggerCharacter: ':' })
    await provider.resolveCompletionItem(item, tokenSource.token)
    middleware.provideCompletionItem = undefined
    middleware.resolveCompletionItem = undefined
    assert.strictEqual(middlewareCalled, 2)
  })

  test('SignatureHelpRequest', async () => {
    const provider = client.getFeature(SignatureHelpRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideSignatureHelp(document, position, tokenSource.token,
      {
        isRetrigger: false,
        triggerKind: SignatureHelpTriggerKind.Invoked,
        triggerCharacter: ':'
      }
    )

    assert.strictEqual(result.activeSignature, 1)
    assert.strictEqual(result.activeParameter, 1)
    isArray(result.signatures, SignatureInformation)

    const signature = result.signatures[0]
    assert.strictEqual(signature.label, 'label')
    assert.strictEqual(signature.documentation, 'doc')
    isArray(signature.parameters, ParameterInformation)

    const parameter = signature.parameters[0]
    assert.strictEqual(parameter.label, 'label')
    assert.strictEqual(parameter.documentation, 'doc')

    let middlewareCalled = false
    middleware.provideSignatureHelp = (d, p, c, t, n) => {
      middlewareCalled = true
      return n(d, p, c, t)
    }
    await provider.provideSignatureHelp(document, position, tokenSource.token,
      {
        isRetrigger: false,
        triggerKind: SignatureHelpTriggerKind.Invoked,
        triggerCharacter: ':'
      }
    )
    middleware.provideSignatureHelp = undefined
    assert.ok(middlewareCalled)
  })

  test('References', async () => {
    const provider = client.getFeature(ReferencesRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideReferences(document, position, {
      includeDeclaration: true
    }, tokenSource.token)

    isArray(result, Location, 2)
    for (let i = 0; i < result.length; i++) {
      const location = result[i]
      rangeEqual(location.range, i, i, i, i)
      assert.strictEqual(location.uri.toString(), document.uri.toString())
    }

    let middlewareCalled = false
    middleware.provideReferences = (d, p, c, t, n) => {
      middlewareCalled = true
      return n(d, p, c, t)
    }
    await provider.provideReferences(document, position, {
      includeDeclaration: true
    }, tokenSource.token)
    middleware.provideReferences = undefined
    assert.ok(middlewareCalled)
  })

  test('Document Highlight', async () => {
    const provider = client.getFeature(DocumentHighlightRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideDocumentHighlights(document, position, tokenSource.token)

    isArray(result, DocumentHighlight, 1)

    const highlight = result[0]
    assert.strictEqual(highlight.kind, DocumentHighlightKind.Read)
    rangeEqual(highlight.range, 2, 2, 2, 2)

    let middlewareCalled = false
    middleware.provideDocumentHighlights = (d, p, t, n) => {
      middlewareCalled = true
      return n(d, p, t)
    }
    await provider.provideDocumentHighlights(document, position, tokenSource.token)
    middleware.provideDocumentHighlights = undefined
    assert.ok(middlewareCalled)
  })

  test('Code Actions', async () => {
    const provider = client.getFeature(CodeActionRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideCodeActions(document, range, {
      diagnostics: []
    }, tokenSource.token)) as CodeAction[]

    isArray(result, CodeAction)
    const action = result[0]
    assert.strictEqual(action.title, 'title')
    assert.strictEqual(action.command?.title, 'title')
    assert.strictEqual(action.command?.command, 'id')

    const resolved = (await provider.resolveCodeAction(result[0], tokenSource.token))
    assert.strictEqual(resolved?.title, 'resolved')

    let middlewareCalled = false
    middleware.provideCodeActions = (d, r, c, t, n) => {
      middlewareCalled = true
      return n(d, r, c, t)
    }

    await provider.provideCodeActions(document, range, { diagnostics: [] }, tokenSource.token)
    middleware.provideCodeActions = undefined
    assert.ok(middlewareCalled)

    middlewareCalled = false
    middleware.resolveCodeAction = (c, t, n) => {
      middlewareCalled = true
      return n(c, t)
    }

    await provider.resolveCodeAction!(result[0], tokenSource.token)
    middleware.resolveCodeAction = undefined
    assert.ok(middlewareCalled)
  })

  test('Progress', async () => {
    const progressToken = 'TEST-PROGRESS-TOKEN'
    const middlewareEvents: Array<WorkDoneProgressBegin | WorkDoneProgressReport | WorkDoneProgressEnd> = []
    let currentProgressResolver: (value: unknown) => void | undefined

    // Set up middleware that calls the current resolve function when it gets its 'end' progress event.
    middleware.handleWorkDoneProgress = (token: ProgressToken, params, next) => {
      if (token === progressToken) {
        middlewareEvents.push(params)
        if (params.kind === 'end') {
          setImmediate(currentProgressResolver)
        }
      }
      return next(token, params)
    }

    // Trigger multiple sample progress events.
    for (let i = 0; i < 2; i++) {
      await new Promise<unknown>((resolve, reject) => {
        currentProgressResolver = resolve
        client.sendRequest(
          new ProtocolRequestType<any, null, never, any, any>('testing/sendSampleProgress'),
          {},
          tokenSource.token,
        ).catch(reject)
      })
    }

    middleware.handleWorkDoneProgress = undefined

    // Ensure all events were handled.
    assert.deepStrictEqual(
      middlewareEvents.map(p => p.kind),
      ['begin', 'report', 'end', 'begin', 'report', 'end'],
    )
  })

  test('Document Formatting', async () => {
    const provider = client.getFeature(DocumentFormattingRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideDocumentFormattingEdits(document, { tabSize: 4, insertSpaces: false }, tokenSource.token)

    isArray(result, TextEdit)
    const edit = result[0]
    assert.strictEqual(edit.newText, 'insert')
    rangeEqual(edit.range, 0, 0, 0, 0)

    let middlewareCalled = true
    middleware.provideDocumentFormattingEdits = (d, c, t, n) => {
      middlewareCalled = true
      return n(d, c, t)
    }
    await provider.provideDocumentFormattingEdits(document, { tabSize: 4, insertSpaces: false }, tokenSource.token)
    middleware.provideDocumentFormattingEdits = undefined
    assert.ok(middlewareCalled)
  })

  test('Document Range Formatting', async () => {
    const provider = client.getFeature(DocumentRangeFormattingRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideDocumentRangeFormattingEdits(document, range, { tabSize: 4, insertSpaces: false }, tokenSource.token)

    isArray(result, TextEdit)
    const edit = result[0]
    assert.strictEqual(edit.newText, '')
    rangeEqual(edit.range, 1, 1, 1, 2)

    let middlewareCalled = true
    middleware.provideDocumentRangeFormattingEdits = (d, r, c, t, n) => {
      middlewareCalled = true
      return n(d, r, c, t)
    }
    await provider.provideDocumentRangeFormattingEdits(document, range, { tabSize: 4, insertSpaces: false }, tokenSource.token)
    middleware.provideDocumentFormattingEdits = undefined
    assert.ok(middlewareCalled)
  })

  test('Document on Type Formatting', async () => {
    const provider = client.getFeature(DocumentOnTypeFormattingRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideOnTypeFormattingEdits(document, position, 'a', { tabSize: 4, insertSpaces: false }, tokenSource.token)

    isArray(result, TextEdit)
    const edit = result[0]
    assert.strictEqual(edit.newText, 'replace')
    rangeEqual(edit.range, 2, 2, 2, 3)

    let middlewareCalled = true
    middleware.provideOnTypeFormattingEdits = (d, p, s, c, t, n) => {
      middlewareCalled = true
      return n(d, p, s, c, t)
    }
    await provider.provideOnTypeFormattingEdits(document, position, 'a', { tabSize: 4, insertSpaces: false }, tokenSource.token)
    middleware.provideDocumentFormattingEdits = undefined
    assert.ok(middlewareCalled)
  })

  test('Rename', async () => {
    const provider = client.getFeature(RenameRequest.method).getProvider(document)
    isDefined(provider)
    isDefined(provider.prepareRename)
    const prepareResult = await provider.prepareRename(document, position, tokenSource.token) as Range

    rangeEqual(prepareResult, 1, 1, 1, 2)
    const renameResult = await provider.provideRenameEdits(document, position, 'newName', tokenSource.token)
    assert.ok(WorkspaceEdit.is(renameResult))
    let middlewareCalled = 0
    middleware.prepareRename = (d, p, t, n) => {
      middlewareCalled++
      return n(d, p, t)
    }
    await provider.prepareRename(document, position, tokenSource.token)
    middleware.prepareRename = undefined
    middleware.provideRenameEdits = (d, p, w, t, n) => {
      middlewareCalled++
      return n(d, p, w, t)
    }
    await provider.provideRenameEdits(document, position, 'newName', tokenSource.token)
    middleware.provideRenameEdits = undefined
    assert.strictEqual(middlewareCalled, 2)
  })

  test('Document Link', async () => {
    const provider = client.getFeature(DocumentLinkRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideDocumentLinks(document, tokenSource.token)

    isArray(result, DocumentLink)
    const documentLink = result[0]
    rangeEqual(documentLink.range, 1, 1, 1, 2)

    let middlewareCalled = 0
    middleware.provideDocumentLinks = (d, t, n) => {
      middlewareCalled++
      return n(d, t)
    }
    await provider.provideDocumentLinks(document, tokenSource.token)
    middleware.provideDocumentLinks = undefined

    isDefined(provider.resolveDocumentLink)
    const resolved = await provider.resolveDocumentLink(documentLink, tokenSource.token)
    isDefined(resolved.target)
    assert.strictEqual(resolved.target.toString(), URI.file('/target.txt').toString())

    middleware.resolveDocumentLink = (i, t, n) => {
      middlewareCalled++
      return n(i, t)
    }
    await provider.resolveDocumentLink(documentLink, tokenSource.token)
    middleware.resolveDocumentLink = undefined
    assert.strictEqual(middlewareCalled, 2)
  })

  test('Document Color', async () => {
    const provider = client.getFeature(DocumentColorRequest.method).getProvider(document)
    isDefined(provider)
    const colors = await provider.provideDocumentColors(document, tokenSource.token)

    isArray(colors, ColorInformation)
    const color = colors[0]

    rangeEqual(color.range, 1, 1, 1, 2)
    colorEqual(color.color, 1, 1, 1, 1)

    let middlewareCalled = 0
    middleware.provideDocumentColors = (d, t, n) => {
      middlewareCalled++
      return n(d, t)
    }
    await provider.provideDocumentColors(document, tokenSource.token)
    middleware.provideDocumentColors = undefined

    const presentations = await provider.provideColorPresentations(color.color, { document, range }, tokenSource.token)

    isArray(presentations, ColorPresentation)
    const presentation = presentations[0]
    assert.strictEqual(presentation.label, 'label')

    middleware.provideColorPresentations = (c, x, t, n) => {
      middlewareCalled++
      return n(c, x, t)
    }
    await provider.provideColorPresentations(color.color, { document, range }, tokenSource.token)
    middleware.provideColorPresentations = undefined
    assert.strictEqual(middlewareCalled, 2)
  })

  test('Goto Declaration', async () => {
    const provider = client.getFeature(DeclarationRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideDeclaration(document, position, tokenSource.token)) as Location

    uriEqual(result.uri, uri)
    rangeEqual(result.range, 1, 1, 1, 2)

    let middlewareCalled = false
    middleware.provideDeclaration = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideDeclaration(document, position, tokenSource.token)
    middleware.provideDeclaration = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Folding Ranges', async () => {
    const provider = client.getFeature(FoldingRangeRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideFoldingRanges(document, {}, tokenSource.token))

    isArray(result, FoldingRange, 1)
    const range = result[0]
    assert.strictEqual(range.startLine, 1)
    assert.strictEqual(range.endLine, 2)
    let middlewareCalled = true
    middleware.provideFoldingRanges = (d, c, t, n) => {
      middlewareCalled = true
      return n(d, c, t)
    }
    await provider.provideFoldingRanges(document, {}, tokenSource.token)
    middleware.provideFoldingRanges = undefined
    assert.ok(middlewareCalled)
  })

  test('Goto Implementation', async () => {
    const provider = client.getFeature(ImplementationRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideImplementation(document, position, tokenSource.token)) as Location

    uriEqual(result.uri, uri)
    rangeEqual(result.range, 2, 2, 3, 3)

    let middlewareCalled = false
    middleware.provideImplementation = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideImplementation(document, position, tokenSource.token)
    middleware.provideImplementation = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Selection Range', async () => {
    const provider = client.getFeature(SelectionRangeRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideSelectionRanges(document, [position], tokenSource.token))

    isArray(result, SelectionRange, 1)
    const range = result[0]
    rangeEqual(range.range, 1, 2, 3, 4)
    let middlewareCalled = false
    middleware.provideSelectionRanges = (d, p, t, n) => {
      middlewareCalled = true
      return n(d, p, t)
    }
    await provider.provideSelectionRanges(document, [position], tokenSource.token)
    middleware.provideSelectionRanges = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Type Definition', async () => {
    const provider = client.getFeature(TypeDefinitionRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.provideTypeDefinition(document, position, tokenSource.token)) as Location

    uriEqual(result.uri, uri)
    rangeEqual(result.range, 2, 2, 3, 3)

    let middlewareCalled = false
    middleware.provideTypeDefinition = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideTypeDefinition(document, position, tokenSource.token)
    middleware.provideTypeDefinition = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Call Hierarchy', async () => {
    const provider = client.getFeature(CallHierarchyPrepareRequest.method).getProvider(document)
    isDefined(provider)
    const result = (await provider.prepareCallHierarchy(document, position, tokenSource.token)) as CallHierarchyItem[]
    expect(result.length).toBe(1)

    let middlewareCalled = false
    middleware.prepareCallHierarchy = (d, p, t, n) => {
      middlewareCalled = true
      return n(d, p, t)
    }
    await provider.prepareCallHierarchy(document, position, tokenSource.token)
    middleware.prepareCallHierarchy = undefined
    assert.strictEqual(middlewareCalled, true)

    const item = result[0]
    const incoming = (await provider.provideCallHierarchyIncomingCalls(item, tokenSource.token)) as CallHierarchyIncomingCall[]
    expect(incoming.length).toBe(1)
    assert.deepEqual(incoming[0].from, item)
    middlewareCalled = false
    middleware.provideCallHierarchyIncomingCalls = (i, t, n) => {
      middlewareCalled = true
      return n(i, t)
    }
    await provider.provideCallHierarchyIncomingCalls(item, tokenSource.token)
    middleware.provideCallHierarchyIncomingCalls = undefined
    assert.strictEqual(middlewareCalled, true)

    const outgoing = (await provider.provideCallHierarchyOutgoingCalls(item, tokenSource.token)) as CallHierarchyOutgoingCall[]
    expect(outgoing.length).toBe(1)
    assert.deepEqual(outgoing[0].to, item)
    middlewareCalled = false
    middleware.provideCallHierarchyOutgoingCalls = (i, t, n) => {
      middlewareCalled = true
      return n(i, t)
    }
    await provider.provideCallHierarchyOutgoingCalls(item, tokenSource.token)
    middleware.provideCallHierarchyOutgoingCalls = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  const referenceFileUri = URI.parse('/dummy-edit')
  function ensureReferenceEdit(edits: WorkspaceEdit, type: string, expectedLines: string[]) {
    // // Ensure the edits are as expected.
    assert.strictEqual(edits.documentChanges?.length, 1)
    const edit = edits.documentChanges[0] as TextDocumentEdit
    assert.strictEqual(edit.edits.length, 1)
    assert.strictEqual(edit.textDocument.uri, referenceFileUri.path)
    assert.strictEqual(edit.edits[0].newText.trim(), `${type}:\n${expectedLines.join('\n')}`.trim())
  }
  async function ensureNotificationReceived(type: string, params: any) {
    const result = await client.sendRequest(
      new ProtocolRequestType<any, any, never, any, any>('testing/lastFileOperationRequest'),
      {},
      tokenSource.token,
    )
    assert.strictEqual(result.type, type)
    assert.deepEqual(result.params, params)
    assert.deepEqual(result, {
      type,
      params
    })
  }

  const createFiles = [
    '/my/file.txt',
    '/my/file.js',
    '/my/folder/',
    // Static registration for tests is [operation]-static and *.txt
    '/my/created-static/file.txt',
    '/my/created-static/file.js',
    '/my/created-static/folder/',
    // Dynamic registration for tests is [operation]-dynamic and *.js
    '/my/created-dynamic/file.txt',
    '/my/created-dynamic/file.js',
    '/my/created-dynamic/folder/',
  ].map(p => URI.file(p))

  const renameFiles = [
    ['/my/file.txt', '/my-new/file.txt'],
    ['/my/file.js', '/my-new/file.js'],
    ['/my/folder/', '/my-new/folder/'],
    // Static registration for tests is [operation]-static and *.txt
    ['/my/renamed-static/file.txt', '/my-new/renamed-static/file.txt'],
    ['/my/renamed-static/file.js', '/my-new/renamed-static/file.js'],
    ['/my/renamed-static/folder/', '/my-new/renamed-static/folder/'],
    // Dynamic registration for tests is [operation]-dynamic and *.js
    ['/my/renamed-dynamic/file.txt', '/my-new/renamed-dynamic/file.txt'],
    ['/my/renamed-dynamic/file.js', '/my-new/renamed-dynamic/file.js'],
    ['/my/renamed-dynamic/folder/', '/my-new/renamed-dynamic/folder/'],
  ].map(([o, n]) => ({ oldUri: URI.file(o), newUri: URI.file(n) }))

  const deleteFiles = [
    '/my/file.txt',
    '/my/file.js',
    '/my/folder/',
    // Static registration for tests is [operation]-static and *.txt
    '/my/deleted-static/file.txt',
    '/my/deleted-static/file.js',
    '/my/deleted-static/folder/',
    // Dynamic registration for tests is [operation]-dynamic and *.js
    '/my/deleted-dynamic/file.txt',
    '/my/deleted-dynamic/file.js',
    '/my/deleted-dynamic/folder/',
  ].map(p => URI.file(p))

  test('File Operations - Will Create Files', async () => {
    const feature = client.getFeature(WillCreateFilesRequest.method)
    isDefined(feature)

    const sendCreateRequest = () => new Promise<WorkspaceEdit>(async (resolve, reject) => {
      void feature.send({ token: CancellationToken.None, files: createFiles, waitUntil: resolve })
      // If feature.send didn't call waitUntil synchronously then something went wrong.
      reject(new Error('Feature unexpectedly did not call waitUntil synchronously'))
    })

    // Send the event and ensure the server responds with an edit referencing the
    // correct files.
    let edits = await sendCreateRequest()
    ensureReferenceEdit(
      edits,
      'WILL CREATE',
      [
        'file:///my/created-static/file.txt',
        'file:///my/created-static/folder/',
        'file:///my/created-dynamic/file.js',
        'file:///my/created-dynamic/folder/',
      ],
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.willCreateFiles = (event, next) => next({
      ...event,
      files: event.files.filter(f => !f.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    edits = await sendCreateRequest()
    ensureReferenceEdit(
      edits,
      'WILL CREATE',
      [
        'file:///my/created-static/file.txt',
        'file:///my/created-dynamic/file.js',
      ],
    )

    middleware.workspace.willCreateFiles = undefined
  })

  test('File Operations - Did Create Files', async () => {
    const feature = client.getFeature(DidCreateFilesNotification.method)
    isDefined(feature)

    // Send the event and ensure the server reports the notification was sent.
    await feature.send({ files: createFiles })
    await ensureNotificationReceived(
      'create',
      {
        files: [
          { uri: 'file:///my/created-static/file.txt' },
          { uri: 'file:///my/created-static/folder/' },
          { uri: 'file:///my/created-dynamic/file.js' },
          { uri: 'file:///my/created-dynamic/folder/' },
        ],
      },
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.didCreateFiles = (event, next) => next({
      files: event.files.filter(f => !f.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    await feature.send({ files: createFiles })
    await ensureNotificationReceived(
      'create',
      {
        files: [
          { uri: 'file:///my/created-static/file.txt' },
          { uri: 'file:///my/created-dynamic/file.js' },
        ],
      },
    )

    middleware.workspace.didCreateFiles = undefined
  })

  test('File Operations - Will Rename Files', async () => {
    const feature = client.getFeature(WillRenameFilesRequest.method)
    isDefined(feature)

    const sendRenameRequest = () => new Promise<WorkspaceEdit>(async (resolve, reject) => {
      void feature.send({ files: renameFiles, waitUntil: resolve })
      // If feature.send didn't call waitUntil synchronously then something went wrong.
      reject(new Error('Feature unexpectedly did not call waitUntil synchronously'))
    })

    // Send the event and ensure the server responds with an edit referencing the
    // correct files.
    let edits = await sendRenameRequest()
    ensureReferenceEdit(
      edits,
      'WILL RENAME',
      [
        'file:///my/renamed-static/file.txt -> file:///my-new/renamed-static/file.txt',
        'file:///my/renamed-static/folder/ -> file:///my-new/renamed-static/folder/',
        'file:///my/renamed-dynamic/file.js -> file:///my-new/renamed-dynamic/file.js',
        'file:///my/renamed-dynamic/folder/ -> file:///my-new/renamed-dynamic/folder/',
      ],
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.willRenameFiles = (event, next) => next({
      ...event,
      files: event.files.filter(f => !f.oldUri.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    edits = await sendRenameRequest()
    ensureReferenceEdit(
      edits,
      'WILL RENAME',
      [
        'file:///my/renamed-static/file.txt -> file:///my-new/renamed-static/file.txt',
        'file:///my/renamed-dynamic/file.js -> file:///my-new/renamed-dynamic/file.js',
      ],
    )

    middleware.workspace.willRenameFiles = undefined
  })

  test('File Operations - Did Rename Files', async () => {
    const feature = client.getFeature(DidRenameFilesNotification.method)
    isDefined(feature)

    // Send the event and ensure the server reports the notification was sent.
    await feature.send({ files: renameFiles })
    await ensureNotificationReceived(
      'rename',
      {
        files: [
          { oldUri: 'file:///my/renamed-static/file.txt', newUri: 'file:///my-new/renamed-static/file.txt' },
          { oldUri: 'file:///my/renamed-static/folder/', newUri: 'file:///my-new/renamed-static/folder/' },
          { oldUri: 'file:///my/renamed-dynamic/file.js', newUri: 'file:///my-new/renamed-dynamic/file.js' },
          { oldUri: 'file:///my/renamed-dynamic/folder/', newUri: 'file:///my-new/renamed-dynamic/folder/' },
        ],
      },
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.didRenameFiles = (event, next) => next({
      files: event.files.filter(f => !f.oldUri.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    await feature.send({ files: renameFiles })
    await ensureNotificationReceived(
      'rename',
      {
        files: [
          { oldUri: 'file:///my/renamed-static/file.txt', newUri: 'file:///my-new/renamed-static/file.txt' },
          { oldUri: 'file:///my/renamed-dynamic/file.js', newUri: 'file:///my-new/renamed-dynamic/file.js' },
        ],
      },
    )

    middleware.workspace.didRenameFiles = undefined
  })

  test('File Operations - Will Delete Files', async () => {
    const feature = client.getFeature(WillDeleteFilesRequest.method)
    isDefined(feature)

    const sendDeleteRequest = () => new Promise<WorkspaceEdit>(async (resolve, reject) => {
      void feature.send({ files: deleteFiles, waitUntil: resolve })
      // If feature.send didn't call waitUntil synchronously then something went wrong.
      reject(new Error('Feature unexpectedly did not call waitUntil synchronously'))
    })

    // Send the event and ensure the server responds with an edit referencing the
    // correct files.
    let edits = await sendDeleteRequest()
    ensureReferenceEdit(
      edits,
      'WILL DELETE',
      [
        'file:///my/deleted-static/file.txt',
        'file:///my/deleted-static/folder/',
        'file:///my/deleted-dynamic/file.js',
        'file:///my/deleted-dynamic/folder/',
      ],
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.willDeleteFiles = (event, next) => next({
      ...event,
      files: event.files.filter(f => !f.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    edits = await sendDeleteRequest()
    ensureReferenceEdit(
      edits,
      'WILL DELETE',
      [
        'file:///my/deleted-static/file.txt',
        'file:///my/deleted-dynamic/file.js',
      ],
    )

    middleware.workspace.willDeleteFiles = undefined
  })

  test('File Operations - Did Delete Files', async () => {
    const feature = client.getFeature(DidDeleteFilesNotification.method)
    isDefined(feature)

    // Send the event and ensure the server reports the notification was sent.
    await feature.send({ files: deleteFiles })
    await ensureNotificationReceived(
      'delete',
      {
        files: [
          { uri: 'file:///my/deleted-static/file.txt' },
          { uri: 'file:///my/deleted-static/folder/' },
          { uri: 'file:///my/deleted-dynamic/file.js' },
          { uri: 'file:///my/deleted-dynamic/folder/' },
        ],
      },
    )

    // Add middleware that strips out any folders.
    middleware.workspace = middleware.workspace || {}
    middleware.workspace.didDeleteFiles = (event, next) => next({
      files: event.files.filter(f => !f.path.endsWith('/')),
    })

    // Ensure we get the same results minus the folders that the middleware removed.
    await feature.send({ files: deleteFiles })
    await ensureNotificationReceived(
      'delete',
      {
        files: [
          { uri: 'file:///my/deleted-static/file.txt' },
          { uri: 'file:///my/deleted-dynamic/file.js' },
        ],
      },
    )

    middleware.workspace.didDeleteFiles = undefined
  })

  test('Semantic Tokens', async () => {
    const provider = client.getFeature(SemanticTokensRegistrationType.method).getProvider(document)
    const rangeProvider = provider?.range
    isDefined(rangeProvider)
    const rangeResult = await rangeProvider.provideDocumentRangeSemanticTokens(document, range, tokenSource.token)
    assert.ok(rangeResult !== undefined)

    let middlewareCalled = false
    middleware.provideDocumentRangeSemanticTokens = (d, r, t, n) => {
      middlewareCalled = true
      return n(d, r, t)
    }
    await rangeProvider.provideDocumentRangeSemanticTokens(document, range, tokenSource.token)
    middleware.provideDocumentRangeSemanticTokens = undefined
    assert.strictEqual(middlewareCalled, true)

    const fullProvider = provider?.full
    isDefined(fullProvider)
    const fullResult = await fullProvider.provideDocumentSemanticTokens(document, tokenSource.token)
    assert.ok(fullResult !== undefined)

    middlewareCalled = false
    middleware.provideDocumentSemanticTokens = (d, t, n) => {
      middlewareCalled = true
      return n(d, t)
    }
    await fullProvider.provideDocumentSemanticTokens(document, tokenSource.token)
    middleware.provideDocumentSemanticTokens = undefined
    assert.strictEqual(middlewareCalled, true)

    middlewareCalled = false
    middleware.provideDocumentSemanticTokensEdits = (d, i, t, n) => {
      middlewareCalled = true
      return n(d, i, t)
    }
    await fullProvider.provideDocumentSemanticTokensEdits!(document, '2', tokenSource.token)
    middleware.provideDocumentSemanticTokensEdits = undefined
    assert.strictEqual(middlewareCalled, true)
  })

  test('Linked Editing Ranges', async () => {
    const provider = client.getFeature(LinkedEditingRangeRequest.method).getProvider(document)
    isDefined(provider)
    const result = await provider.provideLinkedEditingRanges(document, position, tokenSource.token)

    isArray(result.ranges, Range, 1)
    rangeEqual(result.ranges[0], 1, 1, 1, 1)

    let middlewareCalled = false
    middleware.provideLinkedEditingRange = (document, position, token, next) => {
      middlewareCalled = true
      return next(document, position, token)
    }
    await provider.provideLinkedEditingRanges(document, position, tokenSource.token)
    middleware.provideTypeDefinition = undefined
    assert.strictEqual(middlewareCalled, true)
  })
})
