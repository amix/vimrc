import { Neovim } from '@chemzqm/neovim'
import { CancellationToken, Diagnostic, DiagnosticSeverity, Disposable, Emitter, Location, Range } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import diagnosticManager from '../../diagnostic/manager'
import events from '../../events'
import languages from '../../languages'
import BasicList, { PreviewOptions, toVimFiletype } from '../../list/basic'
import { formatListItems, formatPath, UnformattedListItem } from '../../list/formatting'
import manager from '../../list/manager'
import Document from '../../model/document'
import services, { IServiceProvider } from '../../services'
import { ListArgument, ListContext, ListItem, ServiceStat } from '../../types'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper from '../helper'

let listItems: ListItem[] = []
class OptionList extends BasicList {
  public name = 'option'
  public options: ListArgument[] = [{
    name: '-w, -word',
    description: 'word'
  }, {
    name: '-i, -input INPUT',
    hasValue: true,
    description: 'input'
  }]
  constructor(nvim) {
    super(nvim)
    this.addLocationActions()
  }
  public loadItems(_context: ListContext, _token: CancellationToken): Promise<ListItem[]> {
    return Promise.resolve(listItems)
  }
}

let previewOptions: PreviewOptions
class SimpleList extends BasicList {
  public name = 'simple'
  public defaultAction: 'preview'
  constructor(nvim: Neovim) {
    super(nvim)
    this.addAction('preview', async (_item, context) => {
      await this.preview(previewOptions, context)
    })
  }
  public loadItems(): Promise<ListItem[]> {
    return Promise.resolve(['a', 'b', 'c'].map((s, idx) => {
      return { label: s, location: Location.create('test:///a', Range.create(idx, 0, idx + 1, 0)) } as ListItem
    }))
  }
}

let disposables: Disposable[] = []
let nvim: Neovim
const locations: any[] = [{
  filename: __filename,
  range: Range.create(0, 0, 0, 6),
  text: 'foo'
}, {
  filename: __filename,
  range: Range.create(2, 0, 2, 6),
  text: 'Bar'
}, {
  filename: __filename,
  range: Range.create(3, 0, 4, 6),
  text: 'multiple'
}]

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  manager.dispose()
  await helper.shutdown()
})

afterEach(async () => {
  listItems = []
  disposeAll(disposables)
  manager.reset()
  await helper.reset()
})

describe('formatting', () => {
  describe('formatPath()', () => {
    it('should format path', async () => {
      expect(formatPath('hidden', 'path')).toBe('')
      expect(formatPath('full', __filename)).toMatch('sources.test.ts')
      expect(formatPath('short', __filename)).toMatch('sources.test.ts')
      expect(formatPath('filename', __filename)).toMatch('sources.test.ts')
    })
  })

  describe('formatListItems', () => {
    it('should format list items', async () => {
      expect(formatListItems(false, [])).toEqual([])
      let items: UnformattedListItem[] = [{
        label: ['a', 'b', 'c']
      }]
      expect(formatListItems(false, items)).toEqual([{
        label: 'a\tb\tc'
      }])
      items = [{
        label: ['a', 'b', 'c']
      }, {
        label: ['foo', 'bar', 'go']
      }]
      expect(formatListItems(true, items)).toEqual([{
        label: 'a  \tb  \tc '
      }, {
        label: 'foo\tbar\tgo'
      }])
    })
  })
})

describe('configuration', () => {
  beforeEach(() => {
    let list = new OptionList(nvim)
    manager.registerList(list)
  })

  it('should change default options', async () => {
    helper.updateConfiguration('list.source.option.defaultOptions', ['--normal'])
    await manager.start(['option'])
    await manager.session.ui.ready
    const mode = manager.prompt.mode
    expect(mode).toBe('normal')
  })

  it('should change default action', async () => {
    helper.updateConfiguration('list.source.option.defaultAction', 'split')
    await manager.start(['option'])
    await manager.session.ui.ready
    const action = manager.session.defaultAction
    expect(action.name).toBe('split')
    await manager.session.doAction()
    let tab = await nvim.tabpage
    let wins = await tab.windows
    expect(wins.length).toBeGreaterThan(1)
  })

  it('should change default arguments', async () => {
    helper.updateConfiguration('list.source.option.defaultArgs', ['-word'])
    await manager.start(['option'])
    await manager.session.ui.ready
    const context = manager.session.context
    expect(context.args).toEqual(['-word'])
  })
})

describe('BasicList', () => {
  describe('getFiletype()', () => {
    it('should get filetype', async () => {
      expect(toVimFiletype('latex')).toBe('tex')
      expect(toVimFiletype('foo')).toBe('foo')
    })
  })

  describe('parse arguments', () => {
    it('should parse args #1', () => {
      let list = new OptionList(nvim)
      let res = list.parseArguments(['-w'])
      expect(res).toEqual({ word: true })
    })

    it('should parse args #2', () => {
      let list = new OptionList(nvim)
      let res = list.parseArguments(['-word'])
      expect(res).toEqual({ word: true })
    })

    it('should parse args #3', () => {
      let list = new OptionList(nvim)
      let res = list.parseArguments(['-input', 'foo'])
      expect(res).toEqual({ input: 'foo' })
    })
  })

  describe('jumpTo()', () => {
    let list: OptionList
    beforeAll(() => {
      list = new OptionList(nvim)
    })
    it('should jump to uri', async () => {
      let uri = URI.file(__filename).toString()
      await list.jumpTo(uri, 'edit')
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toMatch('sources.test.ts')
    })

    it('should jump to location', async () => {
      let uri = URI.file(__filename).toString()
      let loc = Location.create(uri, Range.create(0, 0, 1, 0))
      await list.jumpTo(loc, 'edit')
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toMatch('sources.test.ts')
    })

    it('should jump to location with empty range', async () => {
      let uri = URI.file(__filename).toString()
      let loc = Location.create(uri, Range.create(0, 0, 0, 0))
      await list.jumpTo(loc, 'edit')
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toMatch('sources.test.ts')
    })
  })

  describe('convertLocation()', () => {
    let list: OptionList
    beforeAll(() => {
      list = new OptionList(nvim)
    })
    it('should convert uri', async () => {
      let uri = URI.file(__filename).toString()
      let res = await list.convertLocation(uri)
      expect(res.uri).toBe(uri)
    })

    it('should convert location with line', async () => {
      let uri = URI.file(__filename).toString()
      let res = await list.convertLocation({ uri, line: 'convertLocation()', text: 'convertLocation' })
      expect(res.uri).toBe(uri)
      res = await list.convertLocation({ uri, line: 'convertLocation()' })
      expect(res.uri).toBe(uri)
    })

    it('should convert location with custom schema', async () => {
      let uri = 'test:///foo'
      let res = await list.convertLocation({ uri, line: 'convertLocation()' })
      expect(res.uri).toBe(uri)
    })
  })

  describe('createAction()', () => {
    it('should overwrite action', async () => {
      let idx: number
      let list = new OptionList(nvim)
      listItems.push({
        label: 'foo',
        location: Location.create('untitled:///1', Range.create(0, 0, 0, 0))
      })
      list.createAction({
        name: 'foo',
        execute: () => { idx = 0 }
      })
      list.createAction({
        name: 'foo',
        execute: () => { idx = 1 }
      })
      disposables.push(manager.registerList(list))
      await manager.start(['--normal', 'option'])
      await manager.session.ui.ready
      await manager.doAction('foo')
      expect(idx).toBe(1)
    })
  })

  describe('preview()', () => {
    beforeEach(() => {
      let list = new SimpleList(nvim)
      disposables.push(manager.registerList(list))
    })

    async function doPreview(opts: PreviewOptions): Promise<number> {
      previewOptions = opts
      await manager.start(['--normal', 'simple'])
      await manager.session.ui.ready
      await manager.doAction('preview')
      let res = await nvim.call('coc#list#has_preview') as number
      expect(res).toBeGreaterThan(0)
      let winid = await nvim.call('win_getid', [res])
      return winid
    }

    it('should preview lines', async () => {
      await doPreview({ filetype: '', lines: ['foo', 'bar'] })
    })

    it('should preview with bufname', async () => {
      await doPreview({
        bufname: 't.js',
        filetype: 'typescript',
        lines: ['foo', 'bar']
      })
    })

    it('should preview with range highlight', async () => {
      let winid = await doPreview({
        bufname: 't.js',
        filetype: 'typescript',
        lines: ['foo', 'bar'],
        range: Range.create(0, 0, 0, 3)
      })
      let res = await nvim.call('getmatches', [winid])
      expect(res.length).toBeGreaterThan(0)
    })
  })

  describe('previewLocation()', () => {
    it('should preview sketch buffer', async () => {
      await nvim.command('new')
      await nvim.setLine('foo')
      let doc = await workspace.document
      expect(doc.uri).toMatch('untitled')
      let list = new OptionList(nvim)
      listItems.push({
        label: 'foo',
        location: Location.create(doc.uri, Range.create(0, 0, 0, 0))
      })
      disposables.push(manager.registerList(list))
      await manager.start(['option'])
      await manager.session.ui.ready
      await helper.wait(30)
      await manager.doAction('preview')
      await nvim.command('wincmd p')
      let win = await nvim.window
      let isPreview = await win.getVar('previewwindow')
      expect(isPreview).toBe(1)
      let line = await nvim.line
      expect(line).toBe('foo')
    })
  })
})

describe('list sources', () => {
  beforeAll(async () => {
    await nvim.setVar('coc_jump_locations', locations)
  })

  describe('locations', () => {
    it('should highlight ranges', async () => {
      await manager.start(['--normal', '--auto-preview', 'location'])
      await manager.session.ui.ready
      await helper.wait(200)
      manager.prompt.cancel()
      await nvim.command('wincmd k')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('sources.test.ts')
      let res = await nvim.call('getmatches')
      expect(res.length).toBe(1)
    })

    it('should change highlight on cursor move', async () => {
      await manager.start(['--normal', '--auto-preview', 'location'])
      await manager.session.ui.ready
      await nvim.command('exe 2')
      let bufnr = await nvim.eval('bufnr("%")')
      await events.fire('CursorMoved', [bufnr, [2, 1]])
      await helper.waitFor('winnr', ['$'], 3)
      await nvim.command('wincmd k')
      let res = await nvim.call('getmatches')
      expect(res.length).toBe(1)
      expect(res[0]['pos1']).toEqual([3, 1, 6])
    })

    it('should highlight multiple line range', async () => {
      await manager.start(['--normal', '--auto-preview', 'location'])
      await manager.session.ui.ready
      await nvim.command('exe 3')
      let bufnr = await nvim.eval('bufnr("%")')
      await events.fire('CursorMoved', [bufnr, [2, 1]])
      await helper.waitFor('winnr', ['$'], 3)
      await nvim.command('wincmd k')
      let res = await nvim.call('getmatches')
      expect(res.length).toBe(1)
      expect(res[0]['pos1']).toBeDefined()
      expect(res[0]['pos2']).toBeDefined()
    })

    it('should do open action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.doAction('open')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('sources.test.ts')
    })

    it('should do quickfix action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.session.ui.selectAll()
      await manager.doAction('quickfix')
      let buftype = await nvim.eval('&buftype')
      expect(buftype).toBe('quickfix')
    })

    it('should do refactor action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.session.ui.selectAll()
      await manager.doAction('refactor')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('coc_refactor')
    })

    it('should do tabe action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.doAction('tabe')
      let tabs = await nvim.tabpages
      expect(tabs.length).toBe(2)
    })

    it('should do drop action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.doAction('drop')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('sources.test.ts')
    })

    it('should do vsplit action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.doAction('vsplit')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('sources.test.ts')
    })

    it('should do split action', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await manager.doAction('split')
      let name = await nvim.eval('bufname("%")')
      expect(name).toMatch('sources.test.ts')
    })
  })

  describe('commands', () => {
    it('should load commands source', async () => {
      await manager.start(['commands'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
    })

    it('should do run action', async () => {
      await manager.start(['commands'])
      await manager.session?.ui.ready
      await manager.doAction()
    })
  })

  describe('diagnostics', () => {

    function createDiagnostic(msg: string, range?: Range, severity?: DiagnosticSeverity, code?: number): Diagnostic {
      range = range ? range : Range.create(0, 0, 0, 1)
      return Diagnostic.create(range, msg, severity || DiagnosticSeverity.Error, code)
    }

    async function createDocument(name?: string): Promise<Document> {
      let doc = await helper.createDocument(name)
      let collection = diagnosticManager.create('test')
      disposables.push({
        dispose: () => {
          collection.clear()
          collection.dispose()
        }
      })
      let diagnostics: Diagnostic[] = []
      await doc.buffer.setLines(['foo bar foo bar', 'foo bar', 'foo', 'bar'], {
        start: 0,
        end: -1,
        strictIndexing: false
      })
      diagnostics.push(createDiagnostic('error', Range.create(0, 2, 0, 4), DiagnosticSeverity.Error, 1001))
      diagnostics.push(createDiagnostic('warning', Range.create(0, 5, 0, 6), DiagnosticSeverity.Warning, 1002))
      diagnostics.push(createDiagnostic('information', Range.create(1, 0, 1, 1), DiagnosticSeverity.Information, 1003))
      diagnostics.push(createDiagnostic('hint', Range.create(1, 2, 1, 3), DiagnosticSeverity.Hint, 1004))
      diagnostics.push(createDiagnostic('error', Range.create(2, 0, 2, 2), DiagnosticSeverity.Error, 1005))
      collection.set(doc.uri, diagnostics)
      await doc.synchronize()
      return doc
    }

    it('should load diagnostics source', async () => {
      await createDocument('a')
      await manager.start(['diagnostics'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
    })

    it('should not include code', async () => {
      let fn = helper.updateConfiguration('list.source.diagnostics.includeCode', false)
      disposables.push({ dispose: fn })
      await createDocument('a')
      await manager.start(['diagnostics'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let line = await nvim.line
      expect(line.match(/100/)).toBeNull()
    })

    it('should hide file path', async () => {
      helper.updateConfiguration('list.source.diagnostics.pathFormat', 'hidden')
      await createDocument('foo')
      await manager.start(['diagnostics'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let line = await nvim.line
      expect(line.match(/foo/)).toBeNull()
    })

    it('should refresh on diagnostics refresh', async () => {
      let doc = await createDocument('bar')
      await manager.start(['diagnostics'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let diagnostics: Diagnostic[] = []
      let collection = diagnosticManager.create('test')
      diagnostics.push(createDiagnostic('error', Range.create(2, 0, 2, 2), DiagnosticSeverity.Error, 1009))
      collection.set(doc.uri, diagnostics)
      await helper.wait(50)
      let buf = await nvim.buffer
      let lines = await buf.lines
      expect(lines.length).toBeGreaterThan(0)
    })
  })

  describe('extensions', () => {
    it('should load extensions source', async () => {
      await manager.start(['extensions'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
    })
  })

  describe('folders', () => {
    it('should load folders source', async () => {
      await manager.start(['folders'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
    })
  })

  describe('lists', () => {
    it('should load lists source', async () => {
      await manager.start(['lists'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      await helper.listInput('<cr>')
      await helper.wait(50)
      let s = manager.getSession()
      expect(s.name != 'lists').toBe(true)
    })
  })

  describe('outline', () => {
    it('should load outline source', async () => {
      await manager.start(['outline'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
    })
  })

  describe('services', () => {
    function createService(name: string): IServiceProvider {
      let _onServcieReady = new Emitter<void>()
      // public readonly onServcieReady: Event<void> = this.
      let service: IServiceProvider = {
        id: name,
        name,
        selector: [{ language: 'vim' }],
        state: ServiceStat.Initial,
        start(): Promise<void> {
          service.state = ServiceStat.Running
          _onServcieReady.fire()
          return Promise.resolve()
        },
        dispose(): void {
          service.state = ServiceStat.Stopped
        },
        stop(): void {
          service.state = ServiceStat.Stopped
        },
        restart(): void {
          service.state = ServiceStat.Running
          _onServcieReady.fire()
        },
        onServiceReady: _onServcieReady.event
      }
      disposables.push(services.regist(service))
      return service
    }

    it('should load services source', async () => {
      createService('foo')
      createService('bar')
      await manager.start(['services'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let lines = await nvim.call('getline', [1, '$']) as string[]
      expect(lines.length).toBe(2)
    })

    it('should toggle service state', async () => {
      let service = createService('foo')
      await service.start()
      await manager.start(['services'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let ses = manager.session
      expect(ses.name).toBe('services')
      await ses.doAction('toggle')
      expect(service.state).toBe(ServiceStat.Stopped)
      await ses.doAction('toggle')
    })
  })

  describe('sources', () => {
    it('should load sources source', async () => {
      await manager.start(['sources'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let session = manager.getSession()
      await session.doAction('open')
      let bufname = await nvim.call('bufname', '%')
      expect(bufname).toMatch(/native/)
    })

    it('should toggle source state', async () => {
      await manager.start(['sources'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let session = manager.getSession()
      await session.doAction('toggle')
      await session.doAction('toggle')
    })

    it('should refresh source', async () => {
      await manager.start(['sources'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      let session = manager.getSession()
      await session.doAction('refresh')
    })
  })

  describe('symbols', () => {
    it('should load symbols source', async () => {
      await helper.createDocument()
      let disposable = languages.registerWorkspaceSymbolProvider({
        provideWorkspaceSymbols: () => []
      })
      await manager.start(['--interactive', 'symbols'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      disposable.dispose()
    })
  })

  describe('links', () => {
    it('should load links source', async () => {
      let disposable = languages.registerDocumentLinkProvider([{ scheme: 'file' }, { scheme: 'untitled' }], {
        provideDocumentLinks: () => []
      })
      await manager.start(['links'])
      await manager.session?.ui.ready
      expect(manager.isActivated).toBe(true)
      disposable.dispose()
    })
  })
})
