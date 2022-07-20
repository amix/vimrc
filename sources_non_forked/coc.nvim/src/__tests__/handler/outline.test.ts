import { Buffer, Neovim } from '@chemzqm/neovim'
import { CodeAction, CodeActionKind, Disposable, DocumentSymbol, Range, SymbolKind, SymbolTag, TextEdit } from 'vscode-languageserver-protocol'
import events from '../../events'
import Symbols from '../../handler/symbols/index'
import languages from '../../languages'
import { ProviderResult } from '../../provider'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper from '../helper'
import Parser from './parser'

let nvim: Neovim
let symbols: Symbols
let disposables: Disposable[] = []

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  symbols = helper.plugin.getHandler().symbols
})

beforeEach(() => {
  disposables.push(languages.registerDocumentSymbolProvider([{ language: 'javascript' }], {
    provideDocumentSymbols: document => {
      let content = document.getText()
      let showDetail = content.includes('detail')
      let parser = new Parser(content, showDetail)
      let res: DocumentSymbol[] = parser.parse()
      if (res.length) {
        res[0].tags = [SymbolTag.Deprecated]
      }
      return Promise.resolve(res)
    }
  }))
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  disposables = []
  await helper.reset()
  await nvim.command(`let w:cocViewId = ''`)

})

async function getOutlineBuffer(): Promise<Buffer | undefined> {
  let winid = await nvim.call('coc#window#find', ['cocViewId', 'OUTLINE'])
  if (winid == -1) return undefined
  let bufnr = await nvim.call('winbufnr', [winid])
  if (bufnr == -1) return undefined
  return nvim.createBuffer(bufnr)
}

describe('symbols outline', () => {

  let defaultCode = `class myClass {
  fun1() { }
  fun2() {}
}`

  async function createBuffer(code = defaultCode): Promise<Buffer> {
    await helper.edit()
    let buf = await nvim.buffer
    await nvim.command('setf javascript')
    await buf.setLines(code.split('\n'), { start: 0, end: -1, strictIndexing: false })
    let doc = await workspace.document
    await doc.synchronize()
    return buf
  }

  describe('configuration', () => {
    it('should follow cursor', async () => {
      await createBuffer()
      let curr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(0)
      let bufnr = await nvim.call('bufnr', ['%'])
      await nvim.command('wincmd p')
      await nvim.command('exe 3')
      await events.fire('CursorHold', [curr])
      await helper.wait(50)
      let buf = nvim.createBuffer(bufnr)
      let lines = await buf.getLines()
      expect(lines.slice(1)).toEqual([
        '- c myClass 1', '    m fun1 2', '    m fun2 3'
      ])
      let signs = await buf.getSigns({ group: 'CocTree' })
      expect(signs.length).toBe(1)
      expect(signs[0]).toEqual({
        lnum: 2,
        id: 3001,
        name: 'CocTreeSelected',
        priority: 10,
        group: 'CocTree'
      })
    })

    it('should not follow cursor', async () => {
      workspace.configurations.updateUserConfig({
        'outline.followCursor': false,
      })
      await createBuffer()
      let curr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(0)
      let bufnr = await nvim.call('bufnr', ['%'])
      await nvim.command('wincmd p')
      await nvim.command('exe 3')
      await events.fire('CursorHold', [curr])
      await helper.wait(50)
      let buf = nvim.createBuffer(bufnr)
      let signs = await buf.getSigns({ group: 'CocTree' })
      expect(signs.length).toBe(0)
    })

    it('should keep current window', async () => {
      workspace.configurations.updateUserConfig({
        'outline.keepWindow': true,
      })
      await createBuffer()
      let curr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline()
      let bufnr = await nvim.call('bufnr', ['%'])
      expect(curr).toBe(bufnr)
    })

    it('should check on buffer switch', async () => {
      workspace.configurations.updateUserConfig({
        'outline.checkBufferSwitch': true,
      })
      await createBuffer()
      await symbols.showOutline(1)
      await helper.edit('unnamed')
      await helper.wait(200)
      let buf = await getOutlineBuffer()
      let lines = await buf.lines
      expect(lines[0]).toMatch('Document symbol provider not found')
    })

    it('should not check on buffer switch', async () => {
      workspace.configurations.updateUserConfig({
        'outline.checkBufferSwitch': false
      })
      await helper.wait(30)
      await createBuffer()
      await symbols.showOutline(1)
      await helper.edit('unnamed')
      await helper.wait(100)
      let buf = await getOutlineBuffer()
      let lines = await buf.lines
      expect(lines.slice(1)).toEqual([
        '- c myClass 1', '    m fun1 2', '    m fun2 3'
      ])
    })

    it('should not check on buffer reload', async () => {
      workspace.configurations.updateUserConfig({
        'outline.checkBufferSwitch': false
      })
      await symbols.showOutline(1)
      await helper.wait(50)
      await createBuffer()
      await helper.wait(50)
      let buf = await getOutlineBuffer()
      expect(buf).toBeDefined()
    })

    it('should sort by position', async () => {
      let code = `class myClass {
  fun2() { }
  fun1() {}
}`
      workspace.configurations.updateUserConfig({
        'outline.sortBy': 'position',
      })
      await createBuffer(code)
      await symbols.showOutline(1)
      let buf = await getOutlineBuffer()
      let lines = await buf.lines
      expect(lines).toEqual([
        'OUTLINE Position', '- c myClass 1', '    m fun2 2', '    m fun1 3'
      ])
    })

    it('should sort by name', async () => {
      let code = `class myClass {
  fun2() {}
  fun1() {}
}`
      workspace.configurations.updateUserConfig({
        'outline.sortBy': 'name',
      })
      await createBuffer(code)
      await symbols.showOutline(1)
      let buf = await getOutlineBuffer()
      let lines = await buf.lines
      expect(lines).toEqual([
        'OUTLINE Name', '- c myClass 1', '    m fun1 3', '    m fun2 2'
      ])
    })

    it('should change sort method', async () => {
      workspace.configurations.updateUserConfig({
        'outline.detailAsDescription': false
      })
      let code = `class detail {
  fun2() {}
  fun1() {}
}`
      await createBuffer(code)
      await symbols.showOutline(0)
      await helper.wait(30)
      await nvim.input('<C-s>')
      await helper.waitFloat()
      await nvim.input('<esc>')
      await helper.wait(30)
      await nvim.input('<C-s>')
      await helper.waitFloat()
      await nvim.input('3')
      await helper.waitFor('getline', [1], 'OUTLINE Position')
    })

    it('should show detail as description', async () => {
      workspace.configurations.updateUserConfig({
        'outline.detailAsDescription': true
      })
      let code = `class detail {
  fun2() {}
}`
      await createBuffer(code)
      await symbols.showOutline(1)
      let buf = await getOutlineBuffer()
      let lines = await buf.lines
      expect(lines.slice(1)).toEqual([
        '- c detail 1', '    m fun2 () 2'
      ])
    })
  })

  describe('events', () => {

    it('should not close TreeView on buffer reload', async () => {
      await createBuffer()
      await symbols.showOutline(0)
      await nvim.command('edit')
      await helper.wait(30)
      let winid = await nvim.call('coc#window#find', ['cocViewId', 'OUTLINE'])
      expect(winid).toBeGreaterThan(0)
    })

    it('should dispose on buffer unload', async () => {
      await createBuffer()
      let curr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(0)
      await nvim.command('tabe')
      await nvim.command(`bd! ${curr}`)
      await helper.wait(30)
      let buf = await getOutlineBuffer()
      expect(buf).toBeUndefined()
    })

    it('should check current window on BufEnter', async () => {
      await createBuffer()
      await symbols.showOutline(1)
      let winid = await nvim.call('win_getid', [])
      await nvim.command('enew')
      await helper.wait(100)
      let win = await nvim.window
      expect(win.id).toBe(winid)
    })

    it('should recreated when original window exists', async () => {
      await symbols.showOutline(1)
      await helper.wait(50)
      await createBuffer()
      await helper.wait(50)
      let buf = await getOutlineBuffer()
      expect(buf).toBeDefined()
    })

    it('should keep old outline when new buffer not attached', async () => {
      await createBuffer()
      await symbols.showOutline(1)
      await nvim.command(`vnew +setl\\ buftype=nofile`)
      await helper.wait(50)
      let buf = await getOutlineBuffer()
      expect(buf).toBeDefined()
      let lines = await buf.lines
      expect(lines.slice(1)).toEqual([
        '- c myClass 1', '    m fun1 2', '    m fun2 3'
      ])
    })

    it('should not reload when switch to original buffer', async () => {
      await createBuffer()
      await symbols.showOutline(0)
      let buf = await getOutlineBuffer()
      let name = await buf.name
      await nvim.command('wincmd p')
      await helper.wait(50)
      buf = await getOutlineBuffer()
      let curr = await buf.name
      expect(curr).toBe(name)
    })
  })

  describe('show()', () => {
    it('should not throw when document not attached', async () => {
      await nvim.command(`edit +setl\\ buftype=nofile t`)
      await workspace.document
      await symbols.showOutline(1)
    })

    it('should not throw when provider does not exist', async () => {
      await symbols.showOutline(1)
      let buf = await getOutlineBuffer()
      expect(buf).toBeDefined()
    })

    it('should not throw when symbols is empty', async () => {
      await createBuffer('')
      await symbols.showOutline(1)
      let buf = await getOutlineBuffer()
      expect(buf).toBeDefined()
    })

    it('should jump to selected symbol', async () => {
      await createBuffer()
      let bufnr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(0)
      await helper.waitFor('getline', [3], '    m fun1 2')
      await nvim.command('exe 3')
      await nvim.input('<cr>')
      await helper.wait(50)
      let curr = await nvim.call('bufnr', ['%'])
      expect(curr).toBe(bufnr)
      let cursor = await nvim.call('coc#cursor#position')
      expect(cursor).toEqual([1, 2])
    })

    it('should update symbols', async () => {
      await createBuffer()
      let doc = await workspace.document
      let bufnr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(1)
      await helper.wait(10)
      let buf = nvim.createBuffer(bufnr)
      let code = 'class foo{}'
      await buf.setLines(code.split('\n'), {
        start: 0,
        end: -1,
        strictIndexing: false
      })
      await doc.synchronize()
      buf = await getOutlineBuffer()
      await helper.waitFor('eval', [`getbufline(${buf.id},1)[0]`], /No\sresults/)
      let lines = await buf.lines
      expect(lines).toEqual([
        'No results',
        '',
        'OUTLINE Category'
      ])
    })

    it('should show label in description', async () => {
      disposables.push(languages.registerDocumentSymbolProvider([{ language: 'vim' }], {
        meta: {
          label: 'vimlsp'
        },
        provideDocumentSymbols: _ => {
          let res: DocumentSymbol[] = [{
            name: 'let',
            range: Range.create(0, 0, 0, 3),
            kind: SymbolKind.Constant,
            selectionRange: Range.create(0, 0, 0, 3),
            tags: [SymbolTag.Deprecated]
          }]
          return Promise.resolve(res)
        }
      }))
      let doc = await helper.createDocument('t.vim')
      await nvim.command('setf vim')
      let buf = await nvim.buffer
      await buf.setLines(['let'], { start: 0, end: -1, strictIndexing: false })
      await doc.synchronize()
      await symbols.showOutline(0)
      await helper.waitFor('getline', [1], 'OUTLINE vimlsp')
    })
  })

  describe('actions', () => {
    it('should invoke visual select', async () => {
      await createBuffer()
      let bufnr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(0)
      await helper.waitFor('getline', [3], /fun1/)
      await nvim.command('exe 3')
      await nvim.input('<tab>')
      await helper.waitFloat()
      await nvim.input('<cr>')
      await helper.waitFor('mode', [], 'v')
      let buf = await nvim.buffer
      expect(buf.id).toBe(bufnr)
    })

    it('should invoke selected code action', async () => {
      const codeAction = CodeAction.create('my action', CodeActionKind.Refactor)
      let uri: string
      disposables.push(languages.registerCodeActionProvider([{ language: '*' }], {
        provideCodeActions: () => [codeAction],
        resolveCodeAction: (action): ProviderResult<CodeAction> => {
          action.edit = {
            changes: {
              [uri]: [TextEdit.del(Range.create(0, 0, 0, 5))]
            }
          }
          return action
        }
      }, undefined))
      await createBuffer()
      let bufnr = await nvim.call('bufnr', ['%'])
      let doc = workspace.getDocument(bufnr)
      uri = doc.uri
      await symbols.showOutline(0)
      await helper.wait(200)
      await nvim.command('exe 3')
      await nvim.input('<tab>')
      await helper.wait(50)
      await nvim.input('<cr>')
      await helper.wait(200)
      let buf = await nvim.buffer
      let lines = await buf.lines
      expect(lines[0]).toBe(' myClass {')
    })
  })

  describe('hide()', () => {
    it('should hide outline', async () => {
      await createBuffer('')
      await symbols.showOutline(1)
      await helper.wait(50)
      await symbols.hideOutline()
      let buf = await getOutlineBuffer()
      expect(buf).toBeUndefined()
    })

    it('should not throw when outline does not exist', async () => {
      await symbols.hideOutline()
      let buf = await getOutlineBuffer()
      expect(buf).toBeUndefined()
    })
  })

  describe('dispose', () => {
    it('should dispose provider and views', async () => {
      await createBuffer('')
      let bufnr = await nvim.call('bufnr', ['%'])
      await symbols.showOutline(1)
      symbols.dispose()
      await helper.wait(50)
      expect(symbols.hasOutline(bufnr)).toBe(false)
      let buf = await getOutlineBuffer()
      expect(buf).toBeUndefined()
    })
  })
})
