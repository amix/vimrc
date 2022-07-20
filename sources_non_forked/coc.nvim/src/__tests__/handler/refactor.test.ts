import { Neovim } from '@chemzqm/neovim'
import { Disposable } from '@chemzqm/neovim/lib/api/Buffer'
import fs from 'fs'
import { Position, Range, TextDocumentEdit, TextEdit, WorkspaceEdit } from 'vscode-languageserver-types'
import { URI } from 'vscode-uri'
import RefactorBuffer, { FileItemDef, fixChangeParams } from '../../handler/refactor/buffer'
import Changes from '../../handler/refactor/changes'
import Refactor from '../../handler/refactor/index'
import languages from '../../languages'
import { DidChangeTextDocumentParams } from '../../types'
import workspace from '../../workspace'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let refactor: Refactor

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  refactor = helper.plugin.getHandler().refactor
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  refactor.reset()
  await helper.reset()
})

function createEdit(uri: string): WorkspaceEdit {
  let edit = TextEdit.insert(Position.create(0, 0), 'a')
  let doc = { uri, version: null }
  return { documentChanges: [TextDocumentEdit.create(doc, [edit])] }
}

// assert ranges is expected.
async function assertSynchronized(buf: RefactorBuffer) {
  let buffer = nvim.createBuffer(buf.bufnr)
  let lines = await buffer.lines
  let items: { lnum: number, lines: string[] }[] = []
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i]
    if (line.includes('\u3000') && line.length > 1) {
      items.push({ lnum: i + 1, lines: [] })
    }
  }
  let curr: { lnum: number, lines: string[] }[] = []
  buf.fileItems.forEach(item => {
    item.ranges.forEach(r => {
      curr.push({ lnum: r.lnum, lines: [] })
    })
  })
  curr.sort((a, b) => a.lnum - b.lnum)
  expect(items).toEqual(curr)
}

describe('fixChangeParams', () => {
  function createChangeParams(range: Range, text: string, original: string, originalLines: ReadonlyArray<string>): DidChangeTextDocumentParams {
    return {
      textDocument: {
        uri: 'untitled:/1',
        version: 1,
      },
      originalLines,
      original,
      bufnr: 1,
      contentChanges: [{ range, text }]
    }
  }

  it('should fix delete change params', async () => {
    let e = createChangeParams(Range.create(0, 4, 2, 4), '', 'x\nfoo\n\u3000bar', [
      '\u3000barx',
      'foo',
      '\u3000bara'
    ])
    e = fixChangeParams(e)
    expect(e.original).toBe('\u3000barx\nfoo\n')
    expect(e.contentChanges[0].range).toEqual(Range.create(0, 0, 2, 0))
  })

  it('should fix insert change params', async () => {
    let e = createChangeParams(Range.create(0, 4, 0, 4), 'x\nfoo\n\u3000bar', '', [
      '\u3000bara'
    ])
    e = fixChangeParams(e)
    expect(e.original).toBe('')
    let change = e.contentChanges[0]
    expect(change.range).toEqual(Range.create(0, 0, 0, 0))
    expect(change.text).toBe('\u3000barx\nfoo\n')
  })
})

describe('refactor', () => {
  describe('checkInsert()', () => {
    it('should check inserted ranges', async () => {
      let c = new Changes()
      expect(c.checkInsert([1])).toBeUndefined()
      c.add([{ filepath: __filename, start: 1, lnum: 1, lines: [''] }])
      expect(c.checkInsert([2])).toBeUndefined()
    })
  })

  describe('getFileRange()', () => {
    it('should throw when range does not exist', async () => {
      let uri = URI.file(__filename).toString()
      let locations = [{ uri, range: Range.create(0, 0, 0, 6) }]
      let buf = await refactor.fromLocations(locations)
      let fn = () => {
        buf.getFileRange(1)
      }
      expect(fn).toThrow(Error)
    })

    it('should find file range', async () => {
      let uri = URI.file(__filename).toString()
      let locations = [{ uri, range: Range.create(0, 0, 0, 6) }]
      let buf = await refactor.fromLocations(locations)
      let res = buf.getFileRange(4)
      expect(res).toBeDefined()
    })
  })

  describe('getRange()', () => {
    it('should get delete range', async () => {
      let filename = await createTmpFile('foo\n\nbar\n')
      let fileItem: FileItemDef = {
        filepath: filename,
        ranges: [{ start: 0, end: 1 }, { start: 2, end: 3 }]
      }
      let buf = await refactor.createRefactorBuffer()
      await buf.addFileItems([fileItem])
      let res = buf.getFileRange(4)
      let r = buf.getDeleteRange(res)
      expect(r).toEqual(Range.create(3, 0, 6, 0))
      res = buf.getFileRange(7)
      r = buf.getDeleteRange(res)
      expect(r).toEqual(Range.create(6, 0, 8, 0))
    })

    it('should get replace range', async () => {
      let filename = await createTmpFile('foo\n\nbar\n')
      let fileItem: FileItemDef = {
        filepath: filename,
        ranges: [{ start: 0, end: 1 }, { start: 2, end: 3 }]
      }
      let buf = await refactor.createRefactorBuffer()
      await buf.addFileItems([fileItem])
      let res = buf.getFileRange(4)
      let r = buf.getReplaceRange(res)
      expect(r).toEqual(Range.create(4, 0, 4, 3))
      res = buf.getFileRange(7)
      r = buf.getReplaceRange(res)
      expect(r).toEqual(Range.create(7, 0, 7, 3))
    })
  })

  describe('fromWorkspaceEdit()', () => {
    it('should not create from invalid workspaceEdit', async () => {
      let res = await refactor.fromWorkspaceEdit(undefined)
      expect(res).toBeUndefined()
      res = await refactor.fromWorkspaceEdit({ documentChanges: [] })
      expect(res).toBeUndefined()
    })

    it('should create from document changes', async () => {
      let edit = createEdit(URI.file(__filename).toString())
      let buf = await refactor.fromWorkspaceEdit(edit)
      let shown = await buf.valid
      expect(shown).toBe(true)
      let items = buf.fileItems
      expect(items.length).toBe(1)
      await nvim.command(`bd! ${buf.bufnr}`)
      await helper.wait(30)
      let has = refactor.has(buf.bufnr)
      expect(has).toBe(false)
    })

    it('should create from workspaceEdit', async () => {
      let changes = {
        [URI.file(__filename).toString()]: [{
          range: Range.create(0, 0, 0, 6),
          newText: ''
        }, {
          range: Range.create(1, 0, 1, 6),
          newText: ''
        }, {
          range: Range.create(50, 0, 50, 1),
          newText: ' '
        }, {
          range: Range.create(60, 0, 60, 1),
          newText: ' '
        }]
      }
      let edit: WorkspaceEdit = { changes }
      let buf = await refactor.fromWorkspaceEdit(edit)
      let shown = await buf.valid
      expect(shown).toBe(true)
      let items = buf.fileItems
      expect(items.length).toBe(1)
    })
  })

  describe('fromLocations()', () => {
    it('should create from locations', async () => {
      let uri = URI.file(__filename).toString()
      let locations = [{
        uri,
        range: Range.create(0, 0, 0, 6),
      }, {
        uri,
        range: Range.create(1, 0, 1, 6),
      }]
      let buf = await refactor.fromLocations(locations)
      let shown = await buf.valid
      expect(shown).toBe(true)
      let items = buf.fileItems
      expect(items.length).toBe(1)
    })

    it('should not create from empty locations', async () => {
      let buf = await refactor.fromLocations([])
      expect(buf).toBeUndefined()
    })
  })

  describe('onChange()', () => {
    async function setup(): Promise<RefactorBuffer> {
      let uri = URI.file(__filename).toString()
      let locations = [{
        uri,
        range: Range.create(0, 0, 0, 6),
      }, {
        uri,
        range: Range.create(1, 0, 1, 6),
      }, {
        uri,
        range: Range.create(10, 0, 10, 6),
      }]
      return await refactor.fromLocations(locations)
    }

    it('should refresh on empty text change', async () => {
      let buf = await setup()
      let line = await nvim.call('getline', [4])
      let doc = workspace.getDocument(buf.bufnr)
      await nvim.call('setline', [4, line])
      doc._forceSync()
      let srcId = await nvim.createNamespace('coc-refactor')
      let markers = await helper.getMarkers(doc.bufnr, srcId)
      expect(markers.length).toBe(2)
    })

    it('should detect range delete and undo', async () => {
      let buf = await setup()
      let doc = workspace.getDocument(buf.bufnr)
      let r = buf.getFileRange(4)
      let end = r.lnum + r.lines.length
      await nvim.command(`${r.lnum},${end + 1}d`)
      await doc.synchronize()
      await assertSynchronized(buf)
      await nvim.command('undo')
      await doc.synchronize()
      await assertSynchronized(buf)
    })

    it('should detect normal delete', async () => {
      let buf = await setup()
      let doc = workspace.getDocument(buf.bufnr)
      let r = buf.getFileRange(4)
      await nvim.command(`${r.lnum + 1},${r.lnum + 1}d`)
      await doc.synchronize()
      await assertSynchronized(buf)
    })

    it('should detect insert', async () => {
      let buf = await setup()
      let doc = workspace.getDocument(buf.bufnr)
      let buffer = nvim.createBuffer(buf.bufnr)
      await buffer.append(['foo'])
      await doc.synchronize()
      await assertSynchronized(buf)
      await buffer.append(['foo', '\u3000'])
      await doc.synchronize()
      await assertSynchronized(buf)
    })
  })

  describe('onDocumentChange()', () => {
    it('should ignore when change after range', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.append(['foo', 'bar'])
      await doc.synchronize()
      let buf = await refactor.fromLocations([{ uri: doc.uri, range: Range.create(0, 0, 0, 3) }])
      let lines = await nvim.call('getline', [1, '$'])
      await doc.buffer.append(['def'])
      await doc.synchronize()
      let newLines = await nvim.call('getline', [1, '$'])
      expect(lines).toEqual(newLines)
      await assertSynchronized(buf)
    })

    it('should adjust when change before range', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.append(['', '', '', '', 'foo', 'bar'])
      await doc.synchronize()
      let buf = await refactor.fromLocations([{ uri: doc.uri, range: Range.create(4, 0, 4, 3) }])
      await doc.buffer.setLines(['def'], { start: 0, end: 0, strictIndexing: false })
      await doc.synchronize()
      let fileRange = buf.getFileRange(4)
      expect(fileRange.start).toBe(2)
      expect(fileRange.lines.length).toBe(6)
      await assertSynchronized(buf)
    })

    it('should remove ranges when lines empty', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.append(['', '', '', '', 'foo', 'bar'])
      await doc.synchronize()
      let buf = await refactor.fromLocations([{ uri: doc.uri, range: Range.create(4, 0, 4, 3) }])
      await doc.buffer.setLines([], { start: 0, end: -1, strictIndexing: false })
      await doc.synchronize()
      let lines = await nvim.call('getline', [1, '$'])
      expect(lines.length).toBe(3)
      let items = buf.fileItems
      expect(items.length).toBe(0)
      await assertSynchronized(buf)
    })

    it('should change when liens changed', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.append(['', '', '', '', 'foo', 'bar'])
      await doc.synchronize()
      let buf = await refactor.fromLocations([{ uri: doc.uri, range: Range.create(4, 0, 4, 3) }])
      await doc.buffer.setLines(['def', 'def'], { start: 5, end: 6, strictIndexing: false })
      await doc.synchronize()
      let lines = await nvim.call('getline', [1, '$'])
      expect(lines[lines.length - 2]).toBe('def')
      await assertSynchronized(buf)
    })
  })

  describe('getFileChanges()', () => {
    it('should get changes #1', async () => {
      await helper.createDocument()
      let lines = `
Save current buffer to make changes
\u3000
\u3000
\u3000/a.ts
    })
  } `
      let buf = await refactor.fromLines(lines.split('\n'))
      let changes = await buf.getFileChanges()
      expect(changes).toEqual([{ lnum: 5, filepath: '/a.ts', lines: ['    })', '  } '] }])
    })

    it('should get changes #2', async () => {
      let lines = `
\u3000/a.ts
    })
  } `
      let buf = await refactor.fromLines(lines.split('\n'))
      let changes = await buf.getFileChanges()
      expect(changes).toEqual([{ lnum: 2, filepath: '/a.ts', lines: ['    })', '  } '] }])
    })

    it('should get changes #3', async () => {
      let lines = `
\u3000/a.ts
    })
  }
\u3000`
      let buf = await refactor.fromLines(lines.split('\n'))
      let changes = await buf.getFileChanges()
      expect(changes).toEqual([{ lnum: 2, filepath: '/a.ts', lines: ['    })', '  }'] }])
    })

    it('should get changes #4', async () => {
      let lines = `
\u3000/a.ts
foo
\u3000/b.ts
bar
\u3000`
      let buf = await refactor.fromLines(lines.split('\n'))
      let changes = await buf.getFileChanges()
      expect(changes).toEqual([
        { filepath: '/a.ts', lnum: 2, lines: ['foo'] },
        { filepath: '/b.ts', lnum: 4, lines: ['bar'] }
      ])
    })
  })

  describe('createRefactorBuffer()', () => {
    it('should create refactor buffer', async () => {
      let winid = await nvim.call('win_getid')
      let buf = await refactor.createRefactorBuffer()
      let curr = await nvim.call('win_getid')
      expect(curr).toBeGreaterThan(winid)
      let valid = await buf.valid
      expect(valid).toBe(true)
      buf = await refactor.createRefactorBuffer('vim')
      valid = await buf.valid
      expect(valid).toBe(true)
    })

    it('should use conceal for line numbers', async () => {
      let buf = await refactor.createRefactorBuffer(undefined, true)
      let fileItem: FileItemDef = {
        filepath: __filename,
        ranges: [{ start: 10, end: 11 }, { start: 15, end: 20 }]
      }
      await buf.addFileItems([fileItem])
      let arr = await nvim.call('getmatches') as any[]
      arr = arr.filter(o => o.group == 'Conceal')
      expect(arr.length).toBeGreaterThan(0)
      await buf.addFileItems([{
        filepath: __filename,
        ranges: [{ start: 1, end: 3 }]
      }])
      await nvim.command('normal! ggdG')
      let doc = workspace.getDocument(buf.bufnr)
      await doc.synchronize()
      let b = nvim.createBuffer(buf.bufnr)
      let res = await b.getVar('line_infos')
      expect(res).toEqual({})
    })
  })

  describe('splitOpen()', () => {
    async function setup(): Promise<RefactorBuffer> {
      let buf = await refactor.createRefactorBuffer()
      let fileItem: FileItemDef = {
        filepath: __filename,
        ranges: [{ start: 10, end: 11 }, { start: 15, end: 20 }]
      }
      await buf.addFileItems([fileItem])
      await nvim.call('cursor', [5, 1])
      return buf
    }

    it('should jump to position by <CR>', async () => {
      let buf = await setup()
      await buf.splitOpen()
      let line = await nvim.eval('line(".")')
      let bufname = await nvim.eval('bufname("%")')
      expect(bufname).toMatch('refactor.test.ts')
      expect(line).toBe(11)
    })

    it('should jump split window when original window not valid', async () => {
      let win = await nvim.window
      let buf = await setup()
      await nvim.call('nvim_win_close', [win.id, true])
      await buf.splitOpen()
      let line = await nvim.eval('line(".")')
      let bufname = await nvim.eval('bufname("%")')
      expect(bufname).toMatch('refactor.test.ts')
      expect(line).toBe(11)
    })
  })

  describe('showMenu()', () => {
    async function setup(): Promise<RefactorBuffer> {
      let buf = await refactor.createRefactorBuffer()
      let fileItem: FileItemDef = {
        filepath: __filename,
        ranges: [{ start: 10, end: 11 }, { start: 15, end: 20 }]
      }
      await buf.addFileItems([fileItem])
      await nvim.call('cursor', [5, 1])
      return buf
    }

    it('should do nothing when cancelled or range not found', async () => {
      let buf = await setup()
      let p = buf.showMenu()
      await helper.wait(50)
      await nvim.input('<esc>')
      await p
      let bufnr = await nvim.call('bufnr', ['%'])
      expect(bufnr).toBe(buf.bufnr)
      await nvim.call('cursor', [1, 1])
      p = buf.showMenu()
      await helper.wait(50)
      await nvim.input('1')
      await p
      bufnr = await nvim.call('bufnr', ['%'])
      expect(bufnr).toBe(buf.bufnr)
    })

    it('should open file in new tab', async () => {
      let buf = await setup()
      await nvim.call('cursor', [4, 1])
      let p = buf.showMenu()
      await helper.wait(30)
      await nvim.input('1')
      await p
      let nr = await nvim.call('tabpagenr')
      expect(nr).toBe(2)
      let lnum = await nvim.call('line', ['.'])
      expect(lnum).toBe(11)
    })

    it('should remove current block', async () => {
      let buf = await setup()
      await nvim.call('cursor', [4, 1])
      let p = buf.showMenu()
      await helper.wait(30)
      await nvim.input('2')
      await p
      let items = buf.fileItems
      expect(items[0].ranges.length).toBe(1)
      await assertSynchronized(buf)
    })
  })

  describe('saveRefactor()', () => {
    it('should adjust line ranges after change', async () => {
      let filename = await createTmpFile('foo\n\nbar\n')
      let fileItem: FileItemDef = {
        filepath: filename,
        ranges: [{ start: 0, end: 1 }, { start: 2, end: 3 }]
      }
      let buf = await refactor.createRefactorBuffer()
      const getRanges = () => {
        let items = buf.fileItems
        let item = items.find(o => o.filepath == filename)
        return item.ranges.map(o => {
          return [o.start, o.start + o.lines.length]
        })
      }
      await buf.addFileItems([fileItem, {
        filepath: __filename,
        ranges: [{ start: 1, end: 5 }]
      }])
      expect(getRanges()).toEqual([[0, 1], [2, 3]])
      nvim.pauseNotification()
      nvim.call('setline', [5, ['xyoo']], true)
      nvim.command('undojoin', true)
      nvim.call('append', [5, ['de']], true)
      nvim.command('undojoin', true)
      nvim.call('setline', [9, ['b']], true)
      await nvim.resumeNotification()
      let doc = workspace.getDocument(buf.bufnr)
      await doc.synchronize()
      let res = await refactor.save(buf.buffer.id)
      expect(res).toBe(true)
      expect(getRanges()).toEqual([[0, 2], [3, 4]])
      let content = fs.readFileSync(filename, 'utf8')
      expect(content).toBe('xyoo\nde\n\nb\n')
    })

    it('should not save when no change made', async () => {
      let buf = await refactor.createRefactorBuffer()
      let fileItem: FileItemDef = {
        filepath: __filename,
        ranges: [{ start: 10, end: 11 }, { start: 15, end: 20 }]
      }
      await buf.addFileItems([fileItem])
      let res = await buf.save()
      expect(res).toBe(false)
    })

    it('should sync buffer change to file', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.replace(['foo', 'bar', 'line'], 0)
      await helper.wait(30)
      let filename = URI.parse(doc.uri).fsPath
      let fileItem: FileItemDef = {
        filepath: filename,
        ranges: [{ start: 0, end: 2 }]
      }
      let buf = await refactor.createRefactorBuffer()
      await buf.addFileItems([fileItem])
      await nvim.call('setline', [5, 'changed'])
      let res = await buf.save()
      expect(res).toBe(true)
      expect(fs.existsSync(filename)).toBe(true)
      let content = fs.readFileSync(filename, 'utf8')
      let lines = content.split('\n')
      expect(lines).toEqual(['changed', 'bar', 'line', ''])
      fs.unlinkSync(filename)
    })
  })

  describe('doRefactor', () => {
    let disposable: Disposable

    afterEach(() => {
      if (disposable) disposable.dispose()
      disposable = null
    })

    it('should throw when rename provider not found', async () => {
      await helper.createDocument()
      let err
      try {
        await refactor.doRefactor()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should show message when prepare failed', async () => {
      await helper.createDocument()
      disposable = languages.registerRenameProvider(['*'], {
        prepareRename: () => {
          return undefined
        },
        provideRenameEdits: () => {
          return null
        }
      })
      await refactor.doRefactor()
      let res = await helper.getCmdline()
      expect(res).toMatch(/unable to rename/)
    })

    it('should show message when returned edits is null', async () => {
      await helper.createDocument()
      disposable = languages.registerRenameProvider(['*'], {
        provideRenameEdits: () => {
          return null
        }
      })
      await refactor.doRefactor()
      let res = await helper.getCmdline()
      expect(res).toMatch(/returns null/)
    })

    it('should open refactor window when edits is valid', async () => {
      let filepath = __filename
      disposable = languages.registerRenameProvider(['*'], {
        provideRenameEdits: () => {
          let changes = {
            [URI.file(filepath).toString()]: [{
              range: Range.create(0, 0, 0, 6),
              newText: ''
            }, {
              range: Range.create(1, 0, 1, 6),
              newText: ''
            }]
          }
          let edit: WorkspaceEdit = { changes }
          return edit
        }
      })
      await helper.createDocument(filepath)
      let winid = await nvim.call('win_getid')
      await refactor.doRefactor()
      let currWin = await nvim.call('win_getid')
      expect(currWin - winid).toBeGreaterThan(0)
      let bufnr = await nvim.call('bufnr', ['%'])
      let b = refactor.getBuffer(bufnr)
      expect(b).toBeDefined()
    })
  })

  describe('search', () => {
    it('should open refactor buffer from search result', async () => {
      let escaped = await nvim.call('fnameescape', [__dirname])
      await nvim.command(`cd ${escaped}`)
      await helper.createDocument()
      await refactor.search(['registerRenameProvider'])
      let buf = await nvim.buffer
      let name = await buf.name
      expect(name).toMatch(/__coc_refactor__/)
      let lines = await buf.lines
      expect(lines[0]).toMatch(/Save current buffer/)
    })
  })
})
