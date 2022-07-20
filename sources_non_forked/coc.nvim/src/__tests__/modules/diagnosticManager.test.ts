import { Neovim } from '@chemzqm/neovim'
import os from 'os'
import path from 'path'
import { Diagnostic, DiagnosticSeverity, DiagnosticTag, Location, Range } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import manager from '../../diagnostic/manager'
import { getNameFromSeverity, severityLevel } from '../../diagnostic/util'
import Document from '../../model/document'
import window from '../../window'
import workspace from '../../workspace'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
function createDiagnostic(msg: string, range?: Range, severity?: DiagnosticSeverity): Diagnostic {
  range = range ? range : Range.create(0, 0, 0, 1)
  return Diagnostic.create(range, msg, severity || DiagnosticSeverity.Error)
}

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  manager.reset()
  await helper.reset()
})

async function createDocument(name?: string): Promise<Document> {
  let doc = await helper.createDocument(name)
  let collection = manager.create('test')
  let diagnostics: Diagnostic[] = []
  await doc.buffer.setLines(['foo bar foo bar', 'foo bar', 'foo', 'bar'], {
    start: 0,
    end: -1,
    strictIndexing: false
  })
  await doc.synchronize()
  diagnostics.push(createDiagnostic('error', Range.create(0, 2, 0, 4), DiagnosticSeverity.Error))
  diagnostics.push(createDiagnostic('warning', Range.create(0, 5, 0, 6), DiagnosticSeverity.Warning))
  diagnostics.push(createDiagnostic('information', Range.create(1, 0, 1, 1), DiagnosticSeverity.Information))
  diagnostics.push(createDiagnostic('hint', Range.create(1, 2, 1, 3), DiagnosticSeverity.Hint))
  diagnostics.push(createDiagnostic('error', Range.create(2, 0, 2, 2), DiagnosticSeverity.Error))
  collection.set(doc.uri, diagnostics)
  return doc
}

describe('diagnostic manager', () => {
  describe('setLocationlist()', () => {
    it('should set location list', async () => {
      let doc = await createDocument()
      await manager.setLocationlist(doc.bufnr)
      let res = await nvim.call('getloclist', [doc.bufnr]) as any[]
      expect(res.length).toBeGreaterThan(2)
      helper.updateConfiguration('diagnostic.locationlistLevel', 'error')
      await manager.setLocationlist(doc.bufnr)
      res = await nvim.call('getloclist', [doc.bufnr]) as any[]
      expect(res.length).toBe(2)
    })

    it('should throw when diagnostic disabled', async () => {
      helper.updateConfiguration('diagnostic.enable', false)
      let fn = async () => {
        let bufnr = await nvim.call('bufnr', ['%'])
        await manager.setLocationlist(bufnr)
      }
      await expect(fn()).rejects.toThrow(/not enabled/)
    })

    it('should throw when buffer not attached', async () => {
      await nvim.command(`vnew +setl\\ buftype=nofile`)
      let doc = await workspace.document
      let fn = async () => {
        await manager.setLocationlist(doc.bufnr)
      }
      await expect(fn()).rejects.toThrow(/not/)
    })
  })

  describe('events', () => {
    it('should delay refresh when buffer visible', async () => {
      let doc = await helper.createDocument()
      await helper.edit()
      let collection = manager.create('foo')
      let diagnostics: Diagnostic[] = []
      await doc.buffer.setLines(['foo bar foo bar', 'foo bar', 'foo', 'bar'], {
        start: 0,
        end: -1,
        strictIndexing: false
      })
      await doc.synchronize()
      diagnostics.push(createDiagnostic('error', Range.create(0, 2, 0, 4), DiagnosticSeverity.Error))
      collection.set(doc.uri, diagnostics)
      await helper.wait(20)
      let buf = doc.buffer
      let val = await buf.getVar('coc_diagnostic_info') as any
      expect(val == null).toBe(true)
      let ns = await nvim.createNamespace('coc-diagnosticfoo')
      let markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBe(0)
      await nvim.command(`b ${buf.id}`)
      await helper.waitFor('eval', ['empty(get(b:,"coc_diagnostic_info",{}))'], 0)
    })

    it('should delay refresh on InsertLeave', async () => {
      let doc = await workspace.document
      await nvim.input('i')
      let collection = manager.create('foo')
      let diagnostics: Diagnostic[] = []
      await doc.buffer.setLines(['foo bar foo bar', 'foo bar', 'foo', 'bar'], {
        start: 0,
        end: -1,
        strictIndexing: false
      })
      await doc.synchronize()
      diagnostics.push(createDiagnostic('error', Range.create(0, 2, 0, 4), DiagnosticSeverity.Error))
      collection.set(doc.uri, diagnostics)
      await helper.wait(30)
      let buf = doc.buffer
      let val = await buf.getVar('coc_diagnostic_info') as any
      expect(val == null).toBe(true)
      let ns = await nvim.createNamespace('coc-diagnosticfoo')
      let markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBe(0)
      await nvim.input('<esc>')
      await helper.wait(30)
      markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBe(1)
    })

    it('should show diagnostic virtual text on CursorMoved', async () => {
      let config = workspace.getConfiguration('diagnostic')
      config.update('virtualText', true)
      config.update('virtualTextCurrentLineOnly', true)
      let doc = await createDocument()
      await helper.wait(30)
      let lnum = await nvim.call('line', ['.'])
      let markers = await doc.buffer.getExtMarks(manager.config.virtualTextSrcId, 0, -1, { details: true })
      expect(markers.length).toBe(2)
      expect(markers[0][1]).toBe(lnum - 1)
      expect(markers[1][1]).toBe(lnum - 1)
      await manager.toggleDiagnosticBuffer(doc.bufnr)
      await nvim.call('cursor', [1, 3])
      await helper.wait(30)
      markers = await doc.buffer.getExtMarks(manager.config.virtualTextSrcId, 0, -1, { details: true })
      expect(markers.length).toBe(0)
    })
  })

  describe('refresh()', () => {
    it('should refresh on buffer create', async () => {
      let uri = URI.file(path.join(path.dirname(__dirname), 'doc')).toString()
      let fn = jest.fn()
      let disposable = manager.onDidRefresh(() => {
        fn()
      })
      let collection = manager.create('tmp')
      let diagnostic = createDiagnostic('My Error')
      collection.set(uri, [diagnostic])
      let doc = await helper.createDocument('doc')
      await helper.wait(30)
      let val = await doc.buffer.getVar('coc_diagnostic_info') as any
      expect(fn).toBeCalled()
      expect(val).toBeDefined()
      expect(val.error).toBe(1)
      collection.dispose()
      disposable.dispose()
    })
  })

  describe('toggleDiagnostic()', () => {
    it('should toggle diagnostics for all buffer', async () => {
      let doc = await createDocument()
      await helper.wait(50)
      manager.toggleDiagnostic()
      await helper.wait(50)
      let val = await doc.buffer.getVar('coc_diagnostic_info') as any
      expect(val).toBe(null)
      manager.toggleDiagnostic()
      await helper.wait(50)
      val = await doc.buffer.getVar('coc_diagnostic_info') as any
      expect(val).toBeDefined()
      expect(val.error).toBe(2)
    })
  })

  describe('getDiagnosticList()', () => {
    it('should get all diagnostics', async () => {
      await createDocument()
      let collection = manager.create('test')
      let fsPath = await createTmpFile('foo')
      let doc = await helper.createDocument(fsPath)
      let diagnostics: Diagnostic[] = []
      diagnostics.push(createDiagnostic('error', Range.create(0, 0, 0, 1), DiagnosticSeverity.Error))
      diagnostics.push(createDiagnostic('error', Range.create(0, 2, 0, 3), DiagnosticSeverity.Warning))
      collection.set(doc.uri, diagnostics)
      let list = await manager.getDiagnosticList()
      expect(list).toBeDefined()
      expect(list.length).toBeGreaterThanOrEqual(5)
      expect(list[0].severity).toBe('Error')
      expect(list[1].severity).toBe('Error')
      expect(list[2].severity).toBe('Error')
    })

    it('should filter diagnostics by configuration', async () => {
      let config = workspace.getConfiguration('diagnostic')
      config.update('level', 'warning')
      config.update('showUnused', false)
      config.update('showDeprecated', false)
      let doc = await createDocument()
      let diagnostics = manager.getDiagnostics(doc.uri)['test']
      diagnostics[0].tags = [DiagnosticTag.Unnecessary]
      diagnostics[2].tags = [DiagnosticTag.Deprecated]
      let list = await manager.getDiagnosticList()
      expect(list.length).toBe(3)
      let res = manager.getDiagnostics(doc.uri)['test']
      expect(res.length).toBe(1)
      let ranges = manager.getSortedRanges(doc.uri)
      expect(ranges.length).toBe(3)
    })
  })

  describe('preview()', () => {
    it('should not throw with empty diagnostics', async () => {
      await manager.preview()
      let tabpage = await nvim.tabpage
      let wins = await tabpage.windows
      expect(wins.length).toBe(1)
    })

    it('should open preview window', async () => {
      await createDocument()
      await nvim.call('cursor', [1, 3])
      await manager.preview()
      let res = await nvim.call('coc#window#find', ['&previewwindow', 1])
      expect(res).toBeDefined()
    })
  })

  describe('setConfigurationErrors()', () => {
    it('should set configuration errors', async () => {
      let doc = await workspace.document
      let errors = [{
        location: Location.create(doc.uri, Range.create(0, 0, 1, 0)),
        message: 'foo',
      }, {
        location: Location.create(doc.uri, Range.create(1, 0, 2, 0)),
        message: 'bar',
      }]
      manager.setConfigurationErrors(errors)
      await helper.wait(50)
      let res = manager.getDiagnostics(doc.uri)
      expect(res.config.length).toBe(2)
      manager.setConfigurationErrors()
      await helper.wait(50)
      res = manager.getDiagnostics(doc.uri)
      expect(res.config).toBeUndefined()
    })
  })

  describe('create()', () => {
    it('should create diagnostic collection', async () => {
      let doc = await workspace.document
      let collection = manager.create('test')
      collection.set(doc.uri, [createDiagnostic('foo')])
      await helper.wait(50)
      let info = await doc.buffer.getVar('coc_diagnostic_info')
      expect(info).toBeDefined()
      await nvim.command('bd!')
      await helper.wait(50)
    })
  })

  describe('getSortedRanges()', () => {
    it('should get sorted ranges of document', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['a', 'b', 'c']])
      let collection = manager.create('test')
      let diagnostics: Diagnostic[] = []
      diagnostics.push(createDiagnostic('x', Range.create(0, 0, 0, 1)))
      diagnostics.push(createDiagnostic('y', Range.create(0, 1, 0, 2)))
      diagnostics.push(createDiagnostic('z', Range.create(1, 0, 1, 2)))
      collection.set(doc.uri, diagnostics)
      let ranges = manager.getSortedRanges(doc.uri)
      expect(ranges[0]).toEqual(Range.create(0, 0, 0, 1))
      expect(ranges[1]).toEqual(Range.create(0, 1, 0, 2))
      expect(ranges[2]).toEqual(Range.create(1, 0, 1, 2))
      ranges = manager.getSortedRanges(doc.uri, 'error')
      expect(ranges.length).toBe(3)
      expect(manager.getSortedRanges(doc.uri, 'warning').length).toBe(0)
    })
  })

  describe('getDiagnosticsInRange', () => {
    it('should get diagnostics in range', async () => {
      let doc = await workspace.document
      let collection = manager.create('test')
      let diagnostics: Diagnostic[] = []
      await doc.buffer.setLines(['foo bar foo bar', 'foo bar'], {
        start: 0,
        end: -1,
        strictIndexing: false
      })
      await doc.synchronize()
      diagnostics.push(createDiagnostic('a', Range.create(0, 0, 0, 1)))
      diagnostics.push(createDiagnostic('b', Range.create(0, 2, 0, 3)))
      diagnostics.push(createDiagnostic('c', Range.create(1, 0, 1, 2)))
      collection.set(doc.uri, diagnostics)
      let res = manager.getDiagnosticsInRange(doc.textDocument, Range.create(0, 0, 0, 3))
      expect(res.length).toBe(2)
    })
  })

  describe('getCurrentDiagnostics', () => {
    it('should get diagnostics under cursor', async () => {
      let config = workspace.getConfiguration('diagnostic')
      await createDocument()
      let diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBe(0)
      await nvim.call('cursor', [1, 4])
      diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBe(1)
      config.update('checkCurrentLine', true)
      await nvim.call('cursor', [1, 2])
      diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBe(2)
    })

    it('should get empty diagnostic at end of line', async () => {
      let doc = await workspace.document
      await nvim.setLine('foo')
      doc.forceSync()
      await nvim.command('normal! $')
      let diagnostic = Diagnostic.create(Range.create(0, 3, 1, 0), 'error', DiagnosticSeverity.Error)
      let collection = manager.create('empty')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.bufnr)
      let diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBeGreaterThanOrEqual(1)
      expect(diagnostics[0].message).toBe('error')
      collection.dispose()
      await manager.refreshBuffer(99)
    })

    it('should get diagnostic next to end of line', async () => {
      let doc = await workspace.document
      await nvim.setLine('foo')
      doc.forceSync()
      await nvim.command('normal! $')
      let diagnostic = Diagnostic.create(Range.create(0, 3, 0, 4), 'error', DiagnosticSeverity.Error)
      let collection = manager.create('empty')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.bufnr)
      let diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBeGreaterThanOrEqual(1)
      expect(diagnostics[0].message).toBe('error')
      collection.dispose()
    })

    it('should get diagnostic with empty range at end of line', async () => {
      let doc = await workspace.document
      await nvim.setLine('foo')
      doc.forceSync()
      await nvim.command('normal! $')
      let diagnostic = Diagnostic.create(Range.create(0, 3, 1, 0), 'error', DiagnosticSeverity.Error)
      let collection = manager.create('empty')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.bufnr)
      let diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBeGreaterThanOrEqual(1)
      expect(diagnostics[0].message).toBe('error')
      collection.dispose()
    })

    it('should get diagnostic pass end of the buffer lines', async () => {
      let doc = await workspace.document
      await nvim.setLine('foo')
      doc.forceSync()
      await nvim.command('normal! ^')
      let diagnostic = Diagnostic.create(Range.create(1, 0, 1, 0), 'error', DiagnosticSeverity.Error)
      let collection = manager.create('empty')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.bufnr)
      let diagnostics = await manager.getCurrentDiagnostics()
      expect(diagnostics.length).toBeGreaterThanOrEqual(1)
      expect(diagnostics[0].message).toBe('error')
      collection.dispose()
    })

  })

  describe('jumpRelated', () => {
    it('should does nothing when no diagnostic exists', async () => {
      let doc = await workspace.document
      await nvim.call('cursor', [1, 1])
      await manager.jumpRelated()
      let bufnr = await nvim.eval('bufnr("%")')
      expect(bufnr).toBe(doc.bufnr)
    })

    it('should does nothing when no related information exists', async () => {
      let doc = await createDocument()
      await nvim.call('cursor', [1, 4])
      await manager.jumpRelated()
      let bufnr = await nvim.eval('bufnr("%")')
      expect(bufnr).toBe(doc.bufnr)
    })

    it('should jump to related position', async () => {
      let doc = await workspace.document
      let range = Range.create(0, 0, 0, 10)
      let location = Location.create(URI.file(__filename).toString(), range)
      let diagnostic = Diagnostic.create(range, 'msg', DiagnosticSeverity.Error, 1000, 'test',
        [{ location, message: 'test' }])
      let collection = manager.create('positions')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.uri)
      await nvim.call('cursor', [1, 1])
      await manager.jumpRelated()
      await helper.wait(100)
      let bufname = await nvim.call('bufname', '%')
      expect(bufname).toMatch('diagnosticManager')
    })

    it('should open location list', async () => {
      let doc = await workspace.document
      let range = Range.create(0, 0, 0, 10)
      let diagnostic = Diagnostic.create(range, 'msg', DiagnosticSeverity.Error, 1000, 'test',
        [{
          location: Location.create(URI.file(__filename).toString(), Range.create(1, 0, 1, 10)),
          message: 'foo'
        }, {
          location: Location.create(URI.file(__filename).toString(), Range.create(2, 0, 2, 10)),
          message: 'bar'
        }])
      let collection = manager.create('positions')
      collection.set(doc.uri, [diagnostic])
      await manager.refreshBuffer(doc.uri)
      await nvim.call('cursor', [1, 1])
      await manager.jumpRelated()
      await helper.waitFor('bufname', ['%'], 'list:///location')
      await nvim.input('<esc>')
    })
  })

  describe('jumpPrevious & jumpNext', () => {
    it('should jump to previous', async () => {
      let doc = await createDocument()
      await nvim.command('normal! G$')
      let ranges = manager.getSortedRanges(doc.uri)
      ranges.reverse()
      for (let i = 0; i < ranges.length; i++) {
        await manager.jumpPrevious()
        let pos = await window.getCursorPosition()
        expect(pos).toEqual(ranges[i].start)
      }
      await manager.jumpPrevious()
    })

    it('should jump to next', async () => {
      let doc = await createDocument()
      await nvim.call('cursor', [0, 0])
      let ranges = manager.getSortedRanges(doc.uri)
      for (let i = 0; i < ranges.length; i++) {
        await manager.jumpNext()
        let pos = await window.getCursorPosition()
        expect(pos).toEqual(ranges[i].start)
      }
      await manager.jumpNext()
    })

    it('should not throw for buffer not attached', async () => {
      await nvim.command('edit foo | setl buftype=nofile')
      let doc = await workspace.document
      expect(doc.attached).toBe(false)
      await manager.jumpNext()
      await manager.jumpPrevious()
    })

    it('should respect wrapscan', async () => {
      await createDocument()
      await nvim.command('setl nowrapscan')
      await nvim.command('normal! G$')
      await manager.jumpNext()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 3, character: 2 })
      await nvim.command('normal! gg0')
      await manager.jumpPrevious()
      pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 0 })
    })
  })

  describe('diagnostic configuration', () => {
    it('should use filetype map from config', async () => {
      let config = workspace.getConfiguration('diagnostic')
      config.update('filetypeMap', { default: 'bufferType' })
      config.update('messageDelay', 10)
      let doc = await createDocument('foo.js')
      await nvim.setLine('foo')
      await doc.synchronize()
      let collection = manager.getCollectionByName('test')
      let diagnostic = createDiagnostic('99', Range.create(0, 0, 0, 3), DiagnosticSeverity.Error)
      diagnostic.codeDescription = {
        href: 'http://www.example.com'
      }
      let diagnostics = [diagnostic]
      collection.set(doc.uri, diagnostics)
      await nvim.call('cursor', [1, 2])
      await manager.echoMessage(false)
      let win = await helper.getFloat()
      let bufnr = await nvim.call('winbufnr', [win.id])
      let buf = nvim.createBuffer(bufnr)
      let lines = await buf.lines
      expect(lines.join('\n')).toMatch('www.example.com')
    })

    it('should show floating window on cursor hold', async () => {
      let config = workspace.getConfiguration('diagnostic')
      config.update('messageTarget', 'float')
      config.update('messageDelay', 10)
      await createDocument()
      await nvim.call('cursor', [1, 3])
      await nvim.command('doautocmd CursorHold')
      let winid = await helper.waitFloat()
      let bufnr = await nvim.call('nvim_win_get_buf', winid) as number
      let buf = nvim.createBuffer(bufnr)
      let lines = await buf.lines
      expect(lines.join('\n')).toMatch('error')
    })

    it('should filter diagnostics by messageLevel', async () => {
      let config = workspace.getConfiguration('diagnostic')
      config.update('messageLevel', 'error')
      config.update('messageTarget', 'echo')
      await createDocument()
      await nvim.call('cursor', [1, 6])
      await manager.echoMessage(false)
      let line = await helper.getCmdline()
      expect(line.indexOf('warning')).toBe(-1)
    })

    it('should echo messages on CursorHold', async () => {
      await createDocument()
      await helper.wait(10)
      let config = workspace.getConfiguration('diagnostic')
      config.update('messageTarget', 'echo')
      config.update('messageDelay', 1)
      await nvim.call('cursor', [1, 3])
      await helper.wait(50)
      let line = await helper.getCmdline()
      expect(line).toMatch('error')
    })

    it('should show diagnostics of current line', async () => {
      helper.updateConfiguration('diagnostic.checkCurrentLine', true)
      helper.updateConfiguration('diagnostic.messageDelay', 1)
      await createDocument()
      await nvim.call('cursor', [1, 3])
      let winid = await helper.waitFloat()
      let win = nvim.createWindow(winid)
      let buf = await win.buffer
      let lines = await buf.lines
      expect(lines.length).toBe(3)
    })

    it('should filter diagnostics by level', async () => {
      helper.updateConfiguration('diagnostic.level', 'warning')
      let doc = await createDocument()
      let diagnosticsMap = manager.getDiagnostics(doc.uri)
      for (let diagnostics of Object.values(diagnosticsMap)) {
        for (let diagnostic of diagnostics) {
          expect(diagnostic.severity != DiagnosticSeverity.Hint).toBe(true)
          expect(diagnostic.severity != DiagnosticSeverity.Information).toBe(true)
        }
      }
    })

    it('should send ale diagnostic items', async () => {
      helper.updateConfiguration('diagnostic.displayByAle', true)
      let content = `
    function! MockAleResults(bufnr, collection, items)
      let g:collection = a:collection
      let g:items = a:items
    endfunction
    `
      let file = await createTmpFile(content)
      await nvim.command(`source ${file}`)
      await createDocument()
      await helper.wait(50)
      let items = await nvim.getVar('items') as any[]
      expect(Array.isArray(items)).toBe(true)
      expect(items.length).toBeGreaterThan(0)
      await nvim.command('bd!')
      await helper.wait(50)
      items = await nvim.getVar('items') as any[]
      expect(items).toEqual([])
    })
  })

  describe('severityLevel & getNameFromSeverity', () => {
    it('should get severity level', () => {
      expect(severityLevel('hint')).toBe(DiagnosticSeverity.Hint)
      expect(severityLevel('error')).toBe(DiagnosticSeverity.Error)
      expect(severityLevel('warning')).toBe(DiagnosticSeverity.Warning)
      expect(severityLevel('information')).toBe(DiagnosticSeverity.Information)
      expect(severityLevel('')).toBe(DiagnosticSeverity.Hint)
    })

    it('should get severity name', () => {
      expect(getNameFromSeverity(null as any)).toBe('CocError')
    })
  })

  describe('toggleDiagnosticBuffer', () => {
    it('should not throw when bufnr is invliad or disabled', async () => {
      let doc = await workspace.document
      await manager.toggleDiagnosticBuffer(99)
      helper.updateConfiguration('diagnostic.enable', false)
      await manager.toggleDiagnosticBuffer(doc.bufnr)
    })

    it('should toggle diagnostics for buffer', async () => {
      let doc = await createDocument()
      // required to wait refresh finish
      await helper.wait(50)
      await manager.toggleDiagnosticBuffer(doc.bufnr)
      let buf = nvim.createBuffer(doc.bufnr)
      let res = await buf.getVar('coc_diagnostic_info') as any
      expect(res == null).toBe(true)
      await manager.toggleDiagnosticBuffer(doc.bufnr)
      await helper.wait(50)
      res = await buf.getVar('coc_diagnostic_info') as any
      expect(res.error).toBe(2)
    })
  })

  describe('refresh', () => {
    let config = workspace.getConfiguration('diagnostic')
    beforeEach(() => {
      config.update('autoRefresh', false)
    })

    it('should refresh by bufnr', async () => {
      let doc = await createDocument()
      let buf = nvim.createBuffer(doc.bufnr)
      let res = await buf.getVar('coc_diagnostic_info') as any
      // should not refresh
      expect(res == null).toBe(true)
      manager.refresh(doc.bufnr)
      await helper.wait(50)
      res = await buf.getVar('coc_diagnostic_info') as any
      expect(res?.error).toBe(2)
      manager.refresh(99)
    })

    it('should refresh all buffers', async () => {
      let uris = ['one', 'two'].map(s => URI.file(path.join(os.tmpdir(), s)).toString())
      await workspace.loadFiles(uris)
      let collection = manager.create('tmp')
      collection.set([[uris[0], [createDiagnostic('Error one')]], [uris[1], [createDiagnostic('Error two')]]])
      manager.refresh()
      await helper.wait(50)
      let bufnrs = [workspace.getDocument(uris[0]).bufnr, workspace.getDocument(uris[1]).bufnr]
      for (let bufnr of bufnrs) {
        let buf = nvim.createBuffer(bufnr)
        let res = await buf.getVar('coc_diagnostic_info') as any
        expect(res?.error).toBe(1)
      }
      collection.dispose()
    })
  })
})
