import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource } from 'vscode-languageserver-protocol'
import Menu from '../../model/menu'
import helper from '../helper'

let nvim: Neovim
let menu: Menu

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  menu.dispose()
  await helper.reset()
})

describe('Menu', () => {
  it('should cancel by <esc>', async () => {
    menu = new Menu(nvim, { items: [{ text: 'foo' }, { text: 'bar', disabled: true }] })
    let p = new Promise(resolve => {
      menu.onDidClose(v => {
        resolve(v)
      })
    })
    await menu.show()
    await helper.wait(30)
    await nvim.input('<esc>')
    let res = await p
    expect(res).toBe(-1)
  })

  it('should cancel before float window shown', async () => {
    let tokenSource: CancellationTokenSource = new CancellationTokenSource()
    menu = new Menu(nvim, { items: [{ text: 'foo' }] }, tokenSource.token)
    let p = new Promise(resolve => {
      menu.onDidClose(v => {
        resolve(v)
      })
    })
    let promise = menu.show()
    tokenSource.cancel()
    await promise
    let res = await p
    expect(res).toBe(-1)
  })

  it('should support menu shortcut', async () => {
    menu = new Menu(nvim, { items: [{ text: 'foo' }, { text: 'bar' }, { text: 'baba' }], shortcuts: true, title: 'Actions' })
    let p = new Promise(resolve => {
      menu.onDidClose(v => {
        resolve(v)
      })
    })
    await menu.show()
    await helper.wait(30)
    await nvim.input('b')
    let res = await p
    expect(res).toBe(1)
  })

  it('should support content', async () => {
    menu = new Menu(nvim, { items: [{ text: 'foo' }, { text: 'bar' }], content: 'content' })
    await menu.show()
    let lines = await menu.buffer.lines
    menu.dispose()
    expect(lines[0]).toBe('content')
  })

  it('should select by CR', async () => {
    menu = new Menu(nvim, { items: ['foo', 'bar'] })
    let p = new Promise(resolve => {
      menu.onDidClose(v => {
        resolve(v)
      })
    })
    await menu.show()
    await helper.wait(30)
    await nvim.input('j')
    await helper.wait(30)
    await nvim.input('<cr>')
    let res = await p
    expect(res).toBe(1)
  })

  it('should show menu in center', async () => {
    menu = new Menu(nvim, { items: ['foo', 'bar'], position: 'center' })
    await menu.show()
    expect(menu.buffer).toBeDefined()
  })

  it('should ignore invalid index', async () => {
    menu = new Menu(nvim, { items: ['foo', 'bar'] })
    await menu.show()
    await helper.wait(30)
    await nvim.input('0')
    await helper.wait(30)
    let exists = await nvim.call('coc#float#has_float', [])
    expect(exists).toBe(1)
  })

  it('should select by index number', async () => {
    menu = new Menu(nvim, { items: ['foo', 'bar'] })
    let p = new Promise(resolve => {
      menu.onDidClose(v => {
        resolve(v)
      })
    })
    await menu.show()
    await helper.wait(30)
    await nvim.input('1')
    let res = await p
    expect(res).toBe(0)
  })

  it('should navigate by j, k, g & G', async () => {
    menu = new Menu(nvim, { items: ['one', 'two', 'three'] })
    await menu.show()
    await helper.wait(50)
    let id = await nvim.call('GetFloatWin')
    expect(id).toBeGreaterThan(0)
    let win = nvim.createWindow(id)
    await nvim.input('j')
    await helper.wait(50)
    let cursor = await win.cursor
    expect(cursor[0]).toBe(2)
    await nvim.input('k')
    await helper.wait(50)
    cursor = await win.cursor
    expect(cursor[0]).toBe(1)
    await nvim.input('G')
    await helper.wait(50)
    cursor = await win.cursor
    expect(cursor[0]).toBe(3)
    await nvim.input('g')
    await helper.wait(50)
    cursor = await win.cursor
    expect(cursor[0]).toBe(1)
  })

  it('should select by numbers', async () => {
    let selected: number
    menu = new Menu(nvim, { items: ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10'] })
    await menu.show()
    let promise = new Promise(resolve => {
      menu.onDidClose(n => {
        selected = n
        resolve(undefined)
      })
    })
    await helper.wait(50)
    await nvim.input('1')
    await helper.wait(50)
    await nvim.input('0')
    await promise
    expect(selected).toBe(9)
  })
})
