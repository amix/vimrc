import { Buffer, Neovim } from '@chemzqm/neovim'
import { Disposable, SymbolInformation, SymbolKind, Range } from 'vscode-languageserver-protocol'
import Symbols from '../../handler/symbols/index'
import languages from '../../languages'
import workspace from '../../workspace'
import window from '../../window'
import events from '../../events'
import { disposeAll } from '../../util'
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
      let text = document.getText()
      let parser = new Parser(text, text.includes('detail'))
      let res = parser.parse()
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
})

describe('Parser', () => {
  it('should parse content', async () => {
    let code = `class myClass {
      fun1() { }
    }`
    let parser = new Parser(code)
    let res = parser.parse()
    expect(res.length).toBeGreaterThan(0)
  })
})

describe('symbols handler', () => {

  async function createBuffer(code: string): Promise<Buffer> {
    let buf = await nvim.buffer
    await nvim.command('setf javascript')
    await buf.setLines(code.split('\n'), { start: 0, end: -1, strictIndexing: false })
    let doc = await workspace.document
    doc.forceSync()
    return buf
  }

  describe('configuration', () => {
    it('should get configuration', async () => {
      let functionUpdate = symbols.functionUpdate
      expect(functionUpdate).toBe(false)
      helper.updateConfiguration('coc.preferences.currentFunctionSymbolAutoUpdate', true)
      functionUpdate = symbols.functionUpdate
      expect(functionUpdate).toBe(true)
    })

    it('should update symbols automatically', async () => {
      helper.updateConfiguration('coc.preferences.currentFunctionSymbolAutoUpdate', true)
      let code = `class myClass {
      fun1() {
      }
    }`
      let buf = await createBuffer(code)
      await nvim.call('cursor', [2, 8])
      await events.fire('CursorHold', [buf.id])
      let val = await buf.getVar('coc_current_function')
      expect(val).toBe('fun1')
      await nvim.call('cursor', [1, 8])
      await events.fire('CursorHold', [buf.id])
      val = await buf.getVar('coc_current_function')
      expect(val).toBe('myClass')
    })
  })

  describe('documentSymbols', () => {
    it('should get symbols of current buffer', async () => {
      let code = `class detail {
      fun1() { }
    }`
      await createBuffer(code)
      let res = await helper.plugin.cocAction('documentSymbols')
      expect(res.length).toBe(2)
      expect(res[1].detail).toBeDefined()
    })

    it('should get current function symbols', async () => {
      let code = `class myClass {
      fun1() {
      }
      fun2() {
      }
    }
    `
      await createBuffer(code)
      await nvim.call('cursor', [3, 0])
      let res = await helper.doAction('getCurrentFunctionSymbol')
      expect(res).toBe('fun1')
      await nvim.command('normal! G')
      res = await helper.doAction('getCurrentFunctionSymbol')
      expect(res).toBe('')
    })

    it('should reset coc_current_function when symbols do not exist', async () => {
      let code = `class myClass {
      fun1() {
      }
    }`
      await createBuffer(code)
      await nvim.call('cursor', [3, 0])
      let res = await helper.doAction('getCurrentFunctionSymbol')
      expect(res).toBe('fun1')
      await nvim.command('normal! ggdG')
      res = await symbols.getCurrentFunctionSymbol()
      expect(res).toBe('')
    })

    it('should support SymbolInformation', async () => {
      disposables.push(languages.registerDocumentSymbolProvider(['*'], {
        provideDocumentSymbols: () => {
          return [
            SymbolInformation.create('root', SymbolKind.Function, Range.create(0, 0, 0, 10)),
            SymbolInformation.create('child', SymbolKind.Function, Range.create(0, 0, 0, 10), '', 'root')
          ]
        }
      }))
      await helper.createDocument()
      let res = await symbols.getDocumentSymbols()
      expect(res.length).toBe(2)
      expect(res[0].text).toBe('root')
      expect(res[1].text).toBe('child')
    })
  })

  describe('selectSymbolRange', () => {
    it('should show warning when no symbols exist', async () => {
      disposables.push(languages.registerDocumentSymbolProvider(['*'], {
        provideDocumentSymbols: () => {
          return []
        }
      }))
      await helper.createDocument()
      await nvim.call('cursor', [3, 0])
      await symbols.selectSymbolRange(false, '', ['Function'])
      let msg = await helper.getCmdline()
      expect(msg).toMatch(/No symbols found/)
    })

    it('should select symbol range at cursor position', async () => {
      let code = `class myClass {
      fun1() {
      }
    }`
      await createBuffer(code)
      await nvim.call('cursor', [3, 0])
      await helper.doAction('selectSymbolRange', false, '', ['Function', 'Method'])
      let mode = await nvim.mode
      expect(mode.mode).toBe('v')
      await nvim.input('<esc>')
      let res = await window.getSelectedRange('v')
      expect(res).toEqual({ start: { line: 1, character: 6 }, end: { line: 2, character: 6 } })
    })

    it('should select inner range', async () => {
      let code = `class myClass {
      fun1() {
        let foo;
      }
}`
      let buf = await createBuffer(code)
      await nvim.call('cursor', [3, 3])
      await symbols.selectSymbolRange(true, '', ['Method'])
      let mode = await nvim.mode
      expect(mode.mode).toBe('v')
      await nvim.input('<esc>')
      let res = await window.getSelectedRange('v')
      expect(res).toEqual({
        start: { line: 2, character: 8 }, end: { line: 2, character: 16 }
      })
    })

    it('should reset visualmode when selection not found', async () => {
      let code = `class myClass {}`
      await createBuffer(code)
      await nvim.call('cursor', [1, 1])
      await nvim.command('normal! gg0v$')
      let mode = await nvim.mode
      expect(mode.mode).toBe('v')
      await nvim.input('<esc>')
      await symbols.selectSymbolRange(true, 'v', ['Method'])
      mode = await nvim.mode
      expect(mode.mode).toBe('v')
    })

    it('should select symbol range from select range', async () => {
      let code = `class myClass {
      fun1() {
      }
    }`
      let buf = await createBuffer(code)
      await nvim.call('cursor', [2, 8])
      await nvim.command('normal! viw')
      await nvim.input('<esc>')
      await helper.doAction('selectSymbolRange', false, 'v', ['Class'])
      let mode = await nvim.mode
      expect(mode.mode).toBe('v')
      let doc = workspace.getDocument(buf.id)
      await nvim.input('<esc>')
      let res = await window.getSelectedRange('v')
      expect(res).toEqual({ start: { line: 0, character: 0 }, end: { line: 3, character: 4 } })
    })
  })

  describe('cancel', () => {
    it('should cancel symbols request on insert', async () => {
      let cancelled = false
      disposables.push(languages.registerDocumentSymbolProvider([{ language: 'text' }], {
        provideDocumentSymbols: (_doc, token) => {
          return new Promise(s => {
            token.onCancellationRequested(() => {
              if (timer) clearTimeout(timer)
              cancelled = true
              s(undefined)
            })
            let timer = setTimeout(() => {
              s(undefined)
            }, 3000)
          })
        }
      }))
      let doc = await helper.createDocument('t.txt')
      let p = symbols.getDocumentSymbols(doc.bufnr)
      setTimeout(async () => {
        await nvim.input('i')
      }, 500)
      await p
      expect(cancelled).toBe(true)
    })
  })

  describe('workspaceSymbols', () => {
    it('should get workspace symbols', async () => {
      disposables.push(languages.registerWorkspaceSymbolProvider({
        provideWorkspaceSymbols: (_query, _token) => {
          return [SymbolInformation.create('far', SymbolKind.Class, Range.create(0, 0, 0, 0))]
        },
        resolveWorkspaceSymbol: sym => {
          let res = Object.assign({}, sym)
          res.location.uri = 'test:///foo'
          return res
        }
      }))
      disposables.push(languages.registerWorkspaceSymbolProvider({
        provideWorkspaceSymbols: (_query, _token) => {
          return [SymbolInformation.create('bar', SymbolKind.Function, Range.create(0, 0, 0, 0))]
        }
      }))
      let res = await symbols.getWorkspaceSymbols('a')
      expect(res.length).toBe(2)
      let resolved = await symbols.resolveWorkspaceSymbol(res[0])
      expect(resolved?.location?.uri).toBe('test:///foo')
    })
  })
})
