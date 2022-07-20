import { Neovim } from '@chemzqm/neovim'
import { Disposable, CallHierarchyItem, SymbolKind, Range, SymbolTag } from 'vscode-languageserver-protocol'
import CallHierarchyHandler from '../../handler/callHierarchy'
import languages from '../../languages'
import workspace from '../../workspace'
import { disposeAll } from '../../util'
import { URI } from 'vscode-uri'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let callHierarchy: CallHierarchyHandler
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  callHierarchy = helper.plugin.getHandler().callHierarchy
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

function createCallItem(name: string, kind: SymbolKind, uri: string, range: Range): CallHierarchyItem {
  return {
    name,
    kind,
    uri,
    range,
    selectionRange: range
  }
}

describe('CallHierarchy', () => {
  it('should throw for when provider does not exist', async () => {
    let err
    try {
      await callHierarchy.getIncoming()
    } catch (e) {
      err = e
    }
    expect(err).toBeDefined()
  })

  it('should get undefined when prepare failed', async () => {
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return undefined
      },
      provideCallHierarchyIncomingCalls() {
        return []
      },
      provideCallHierarchyOutgoingCalls() {
        return []
      }
    }))
    let res = await callHierarchy.getOutgoing()
    expect(res).toBeUndefined()
  })

  it('should get incoming & outgoing callHierarchy items', async () => {
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, 'test:///foo', Range.create(0, 0, 0, 5))
      },
      provideCallHierarchyIncomingCalls() {
        return [{
          from: createCallItem('bar', SymbolKind.Class, 'test:///bar', Range.create(1, 0, 1, 5)),
          fromRanges: [Range.create(0, 0, 0, 5)]
        }]
      },
      provideCallHierarchyOutgoingCalls() {
        return [{
          to: createCallItem('bar', SymbolKind.Class, 'test:///bar', Range.create(1, 0, 1, 5)),
          fromRanges: [Range.create(1, 0, 1, 5)]
        }]
      }
    }))
    let res = await callHierarchy.getIncoming()
    expect(res.length).toBe(1)
    expect(res[0].from.name).toBe('bar')
    let outgoing = await callHierarchy.getOutgoing()
    expect(outgoing.length).toBe(1)
    res = await callHierarchy.getIncoming(outgoing[0].to)
    expect(res.length).toBe(1)
  })

  it('should show message when provider does not exist', async () => {
    await callHierarchy.showCallHierarchyTree('incoming')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines[0]).toMatch('callHierarchy provider not found')
    await nvim.command('wincmd p')
  })

  it('should no results when no result returned.', async () => {
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return []
      },
      provideCallHierarchyIncomingCalls() {
        return []
      },
      provideCallHierarchyOutgoingCalls() {
        return []
      }
    }))
    await callHierarchy.showCallHierarchyTree('incoming')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines[0]).toBe('No results')
    await nvim.command('wincmd p')
  })

  it('should render description and support default action', async () => {
    let doc = await workspace.document
    let bufnr = doc.bufnr
    await doc.buffer.setLines(['foo'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(1, 0, 1, 3))
        item.detail = 'Detail'
        item.tags = [SymbolTag.Deprecated]
        return [{
          from: item,
          fromRanges: [Range.create(2, 0, 2, 5)]
        }]
      },
      provideCallHierarchyOutgoingCalls() {
        return []
      }
    }))
    await callHierarchy.showCallHierarchyTree('incoming')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'INCOMING CALLS',
      '- c foo',
      '  + c bar Detail'
    ])
    await nvim.command('exe 3')
    await nvim.input('t')
    await helper.waitFor('getline', ['.'], '  - c bar Detail')
    await nvim.input('<cr>')
    await helper.waitFor('expand', ['%:p'], fsPath)
    let res = await nvim.call('coc#cursor#position')
    expect(res).toEqual([1, 0])
    let matches = await nvim.call('getmatches') as any[]
    expect(matches.length).toBe(2)
    await nvim.command(`b ${bufnr}`)
    await helper.wait(50)
    matches = await nvim.call('getmatches')
    expect(matches.length).toBe(0)
    await nvim.command(`wincmd o`)
    await helper.wait(50)
  })

  it('should invoke open in new tab action', async () => {
    let doc = await workspace.document
    await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        return []
      },
      provideCallHierarchyOutgoingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(0, 0, 0, 1))
        item.detail = 'Detail'
        return [{
          to: item,
          fromRanges: [Range.create(1, 0, 1, 3)]
        }]
      }
    }))
    let win = await nvim.window
    let tab = await nvim.call('tabpagenr')
    await callHierarchy.showCallHierarchyTree('outgoing')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo',
      '  + c bar Detail'
    ])
    await nvim.command('exe 3')
    await nvim.input('<tab>')
    await helper.wait(100)
    await nvim.input('<cr>')
    await helper.wait(200)
    let newTab = await nvim.call('tabpagenr')
    expect(newTab != tab).toBe(true)
    doc = await workspace.document
    expect(doc.uri).toBe(uri)
    let res = await nvim.call('getmatches', [win.id])
    expect(res.length).toBe(1)
  })

  it('should invoke show incoming calls action', async () => {
    let doc = await workspace.document
    await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        return [{
          from: createCallItem('test', SymbolKind.Class, 'test:///bar', Range.create(1, 0, 1, 5)),
          fromRanges: [Range.create(0, 0, 0, 5)]
        }]
      },
      provideCallHierarchyOutgoingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(0, 0, 0, 1))
        item.detail = 'Detail'
        return [{
          to: item,
          fromRanges: [Range.create(1, 0, 1, 3)]
        }]
      }
    }))
    await callHierarchy.showCallHierarchyTree('outgoing')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo',
      '  + c bar Detail'
    ])
    await nvim.command('exe 3')
    await nvim.input('<tab>')
    await helper.wait(50)
    await nvim.input('2')
    await helper.wait(200)
    lines = await buf.lines
    expect(lines).toEqual([
      'INCOMING CALLS',
      '- c bar Detail',
      '  + c test'
    ])
    await nvim.command('bd!')
  })

  it('should invoke show outgoing calls action', async () => {
    let doc = await workspace.document
    await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        return [{
          from: createCallItem('test', SymbolKind.Class, 'test:///bar', Range.create(1, 0, 1, 5)),
          fromRanges: [Range.create(0, 0, 0, 5)]
        }]
      },
      provideCallHierarchyOutgoingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(0, 0, 0, 1))
        item.detail = 'Detail'
        return [{
          to: item,
          fromRanges: [Range.create(1, 0, 1, 3)]
        }]
      }
    }))
    await callHierarchy.showCallHierarchyTree('incoming')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'INCOMING CALLS',
      '- c foo',
      '  + c test'
    ])
    await nvim.command('exe 3')
    await nvim.input('<tab>')
    await helper.wait(50)
    await nvim.input('3')
    await helper.wait(200)
    lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c test',
      '  + c bar Detail'
    ])
    await nvim.command('bd!')
  })

  it('should invoke dismiss action #1', async () => {
    let doc = await workspace.document
    await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        return []
      },
      provideCallHierarchyOutgoingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(0, 0, 0, 1))
        item.detail = 'Detail'
        return [{
          to: item,
          fromRanges: [Range.create(1, 0, 1, 3)]
        }]
      }
    }))
    await callHierarchy.showCallHierarchyTree('outgoing')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo',
      '  + c bar Detail'
    ])
    await nvim.command('exe 3')
    await nvim.input('<tab>')
    await helper.wait(50)
    await nvim.input('4')
    await helper.wait(200)
    lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo'
    ])
    await nvim.command('wincmd c')
  })

  it('should invoke dismiss action #2', async () => {
    let doc = await workspace.document
    await doc.buffer.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    let fsPath = await createTmpFile('foo\nbar\ncontent\n')
    let uri = URI.file(fsPath).toString()
    disposables.push(languages.registerCallHierarchyProvider([{ language: '*' }], {
      prepareCallHierarchy() {
        return createCallItem('foo', SymbolKind.Class, doc.uri, Range.create(0, 0, 0, 3))
      },
      provideCallHierarchyIncomingCalls() {
        return []
      },
      provideCallHierarchyOutgoingCalls() {
        let item = createCallItem('bar', SymbolKind.Class, uri, Range.create(0, 0, 0, 1))
        item.detail = 'Detail'
        return [{
          to: item,
          fromRanges: [Range.create(1, 0, 1, 3)]
        }]
      }
    }))
    await callHierarchy.showCallHierarchyTree('outgoing')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo',
      '  + c bar Detail'
    ])
    await nvim.command('exe 3')
    await nvim.input('t')
    await helper.wait(50)
    await nvim.command('exe 4')
    await nvim.input('<tab>')
    await helper.wait(50)
    await nvim.input('4')
    await helper.wait(200)
    lines = await buf.lines
    expect(lines).toEqual([
      'OUTGOING CALLS',
      '- c foo',
      '  - c bar Detail'
    ])
    await nvim.command('wincmd c')
  })
})
