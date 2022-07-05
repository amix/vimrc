import { Neovim } from '@chemzqm/neovim'
import Editors, { TextEditor } from '../../core/editors'
import workspace from '../../workspace'
import window from '../../window'
import events from '../../events'
import helper from '../helper'
import { disposeAll } from '../../util'
import { Disposable } from 'vscode-languageserver-protocol'

let editors: Editors
let nvim: Neovim
let disposables: Disposable[] = []

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  editors = workspace.editors
})

afterEach(async () => {
  await helper.reset()
})

afterAll(async () => {
  disposeAll(disposables)
  await helper.shutdown()
})

describe('editors', () => {

  function assertEditor(editor: TextEditor, tabpagenr: number, winid: number) {
    expect(editor).toBeDefined()
    expect(editor.tabpagenr).toBe(tabpagenr)
    expect(editor.winid).toBe(winid)
  }

  it('should have active editor', async () => {
    let winid = await nvim.call('win_getid')
    let editor = window.activeTextEditor
    assertEditor(editor, 1, winid)
    let editors = window.visibleTextEditors
    expect(editors.length).toBe(1)
  })

  it('should change active editor on split', async () => {
    let promise = new Promise<TextEditor>(resolve => {
      editors.onDidChangeActiveTextEditor(e => {
        resolve(e)
      }, null, disposables)
    })
    await nvim.command('vnew')
    let editor = await promise
    let winid = await nvim.call('win_getid')
    expect(editor.winid).toBe(winid)
  })

  it('should change active editor on tabe', async () => {
    let promise = new Promise<TextEditor>(resolve => {
      editors.onDidChangeActiveTextEditor(e => {
        if (e.document.uri.includes('foo')) {
          resolve(e)
        }
      }, null, disposables)
    })
    await nvim.command('tabe a | tabe b | tabe foo')
    let editor = await promise
    let winid = await nvim.call('win_getid')
    expect(editor.winid).toBe(winid)
  })

  it('should change active editor on edit', async () => {
    await nvim.call('win_getid')
    let fn = jest.fn()
    window.onDidChangeVisibleTextEditors(() => {
      fn()
    }, null, disposables)
    let promise = new Promise<TextEditor>(resolve => {
      editors.onDidChangeActiveTextEditor(e => {
        resolve(e)
      })
    })
    await nvim.command('edit foo')
    let editor = await promise
    expect(editor.document.uri).toMatch('foo')
    expect(fn).toBeCalled()
  })

  it('should change active editor on window switch', async () => {
    let winid = await nvim.call('win_getid')
    await nvim.command('vs foo')
    await nvim.command('wincmd p')
    let curr = editors.activeTextEditor
    expect(curr.winid).toBe(winid)
    expect(editors.visibleTextEditors.length).toBe(2)
  })

  it('should not create editor for float window', async () => {
    let fn = jest.fn()
    await nvim.call('win_getid')
    editors.onDidChangeActiveTextEditor(e => {
      fn()
    })
    let res = await nvim.call('coc#float#create_float_win', [0, 0, {
      relative: 'editor',
      row: 1,
      col: 1,
      width: 10,
      height: 1,
      lines: ['foo']
    }])
    await nvim.call('win_gotoid', [res[0]])
    await events.fire('CursorHold', [res[1]])
    await nvim.command('wincmd p')
    expect(fn).toBeCalledTimes(0)
    expect(editors.visibleTextEditors.length).toBe(1)
  })

  it('should cleanup on CursorHold', async () => {
    let winid = await nvim.call('win_getid')
    let promise = new Promise<TextEditor>(resolve => {
      editors.onDidChangeActiveTextEditor(e => {
        if (e.document.uri.includes('foo')) {
          resolve(e)
        }
      }, null, disposables)
    })
    await nvim.command('tabe foo')
    await promise
    await nvim.call('win_execute', [winid, 'noa close'])
    let bufnr = await nvim.eval("bufnr('%')")
    await events.fire('CursorHold', [bufnr])
    expect(editors.visibleTextEditors.length).toBe(1)
  })

  it('should cleanup on create', async () => {
    let winid = await nvim.call('win_getid')
    let promise = new Promise<TextEditor>(resolve => {
      editors.onDidChangeActiveTextEditor(e => {
        if (e.document.uri.includes('foo')) {
          resolve(e)
        }
      }, null, disposables)
    })
    await nvim.command('tabe foo')
    await promise
    await nvim.call('win_execute', [winid, 'noa close'])
    await nvim.command('edit bar')
    expect(editors.visibleTextEditors.length).toBe(2)
  })

  it('should have corrent tabnr after tab changed', async () => {
    await nvim.command('tabe')
    await helper.waitValue(() => {
      return editors.visibleTextEditors.length
    }, 2)
    let editor = editors.visibleTextEditors.find(o => o.tabpagenr == 2)
    await nvim.command('normal! 1gt')
    await nvim.command('tabe')
    await helper.waitValue(() => {
      return editors.visibleTextEditors.length
    }, 3)
    expect(editor.tabpagenr).toBe(3)
    await nvim.command('tabc')
    await helper.waitValue(() => {
      return editors.visibleTextEditors.length
    }, 2)
    expect(editor.tabpagenr).toBe(2)
  })
})
