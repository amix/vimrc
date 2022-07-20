import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable } from 'vscode-languageserver-protocol'
import { QuickPickItem } from '../../types'
import { disposeAll } from '../../util'
import events from '../../events'
import window from '../../window'
import workspace from '../../workspace'
import helper from '../helper'
export type Item = QuickPickItem | string

let nvim: Neovim
let disposables: Disposable[] = []

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})
afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  disposables = []
})

describe('window', () => {
  async function getTitleLine(): Promise<string> {
    let winids = await nvim.call('coc#float#get_float_win_list') as number[]
    let winid = Math.min(...winids)
    let id = await nvim.call('coc#float#get_related', [winid, 'border'])
    let win = nvim.createWindow(id)
    let buf = await win.buffer
    let lines = await buf.lines
    return lines[0]
  }

  describe('showQuickPick', () => {
    async function testQuickPick(items: Item[], canPickMany: boolean, cancel: boolean, res: any) {
      let p = window.showQuickPick(items, { canPickMany })
      await helper.waitFloat()
      await nvim.input('b')
      if (canPickMany) {
        await nvim.input('<C-space>')
      }
      await helper.wait(50)
      if (cancel) {
        await nvim.input('<esc>')
      } else {
        await nvim.input('<cr>')
      }
      let result = await p
      if (res == null) {
        expect(result).toBe(res)
      } else {
        expect(res).toEqual(res)
      }
    }

    it('should throw when dialog not supported ', async () => {
      Object.assign(workspace.env, { dialog: false })
      disposables.push({
        dispose: () => {
          Object.assign(workspace.env, { dialog: true })
        }
      })
      let fn = async () => {
        await window.showQuickPick(['foo', 'bar'])
      }
      await expect(fn()).rejects.toThrow(Error)
    })

    it('should resolve undefined when token cancelled', async () => {
      let tokenSource = new CancellationTokenSource()
      let token = tokenSource.token
      tokenSource.cancel()
      let res = await window.showQuickPick(['foo', 'bar'], undefined, token)
      expect(res).toBeUndefined()
      let release = await window.mutex.acquire()
      tokenSource = new CancellationTokenSource()
      token = tokenSource.token
      let p = window.showQuickPick(['foo', 'bar'], undefined, token)
      tokenSource.cancel()
      release()
      res = await p
      expect(res).toBeUndefined()
    })

    it('should show quickfix with items or texts', async () => {
      await testQuickPick(['foo', 'bar'], false, false, 'bar')
      await testQuickPick(['foo', 'bar'], true, false, ['bar'])
      await testQuickPick(['foo', 'bar'], false, true, undefined)
      let items: QuickPickItem[] = [{ label: 'foo', description: 'desc' }, { label: 'bar', picked: true }]
      await testQuickPick(items, false, false, { label: 'bar', picked: true })
      await testQuickPick(items, true, false, [{ label: 'bar', picked: true }])
    })

    it('should use title option', async () => {
      let p = window.showQuickPick(['foo', 'bar'], { title: 'title' })
      await helper.waitFloat()
      let line = await getTitleLine()
      expect(line).toMatch('title')
      await nvim.input('<esc>')
      await p
    })

    it('should match on description', async () => {
      let items: QuickPickItem[] = [{ label: 'foo', description: 'desc' }, { label: 'bar', picked: true }]
      let p = window.showQuickPick(items, { matchOnDescription: true })
      await helper.waitFloat()
      await nvim.input('d')
      await helper.wait(30)
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBeDefined()
    })
  })

  describe('createQuickPick', () => {
    it('should throw when unable to open input window', async () => {
      let fn = nvim.call
      nvim.call = (...args: any) => {
        if (args[0] === 'coc#dialog#create_prompt_win') return undefined
        return fn.apply(nvim, args)
      }
      disposables.push(Disposable.create(() => {
        nvim.call = fn
      }))
      let fun = async () => {
        await window.createQuickPick({
          items: [{ label: 'foo' }, { label: 'bar' }],
        })
      }
      await expect(fun()).rejects.toThrow(/Unable to open/)
    })

    it('should throw when unable to open list window', async () => {
      let fn = nvim.call
      nvim.call = (...args: any) => {
        if (args[0] === 'coc#dialog#create_list') return undefined
        return fn.apply(nvim, args)
      }
      disposables.push(Disposable.create(() => {
        nvim.call = fn
      }))
      let fun = async () => {
        await window.createQuickPick({
          items: [{ label: 'foo' }, { label: 'bar' }],
        })
      }
      await expect(fun()).rejects.toThrow(/Unable to open/)
    })

    it('should respect initial value', async () => {
      await window.createQuickPick({
        items: [{ label: 'foo' }, { label: 'bar' }],
        value: 'value'
      })
      let winids = await nvim.call('coc#float#get_float_win_list') as number[]
      let winid = Math.min(...winids)
      let buf = await (nvim.createWindow(winid)).buffer
      let lines = await buf.lines
      expect(lines[0]).toBe('value')
      await nvim.input('<esc>')
    })

    it('should respect maxHeight', async () => {
      await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
        value: 'value',
        maxHeight: 2
      })
      let winids = await nvim.call('coc#float#get_float_win_list') as number[]
      let winid = Math.max(...winids)
      let win = nvim.createWindow(winid)
      let h = await win.height
      expect(h).toBe(2)
      await nvim.input('<esc>')
    })

    it('should scroll by <C-f> and <C-b>', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
        value: 'value',
        maxHeight: 2
      })
      disposables.push(quickpick)
      let winids = await nvim.call('coc#float#get_float_win_list') as number[]
      let winid = Math.max(...winids)
      await nvim.input('<C-f>')
      await helper.wait(30)
      await nvim.input('<C-f>')
      await helper.wait(30)
      let info = await nvim.call('getwininfo', [winid])
      expect(info[0].topline).toBe(2)
      await nvim.input('<C-b>')
      await helper.wait(30)
      await nvim.input('<C-b>')
      await helper.wait(30)
      info = await nvim.call('getwininfo', [winid])
      expect(info[0].topline).toBe(1)
    })

    it('should change current line by <C-j> and <C-k>', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
        value: 'value',
        maxHeight: 2
      })
      disposables.push(quickpick)
      await nvim.input('<C-j>')
      await helper.wait(30)
      await nvim.input('<C-j>')
      await helper.wait(30)
      expect(quickpick.currIndex).toBe(2)
      await nvim.input('<C-k>')
      await helper.wait(30)
      await nvim.input('<C-k>')
      await helper.wait(30)
      expect(quickpick.currIndex).toBe(0)
    })

    it('should toggle selected item by <C-space>', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
        value: 'value',
        maxHeight: 2
      })
      disposables.push(quickpick)
      await nvim.input('<C-sapce>')
      await helper.wait(30)
      await nvim.input('<C-k>')
      await helper.wait(30)
      await nvim.input('<C-sapce>')
      await helper.wait(30)
      expect(quickpick.selectedItems.length).toBe(0)
    })

    it('should not handle events from other buffer', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
      })
      disposables.push(quickpick)
      await events.fire('BufWinLeave', [quickpick.buffer.id + 1])
      await events.fire('PromptKeyPress', [quickpick.buffer.id + 1, 'C-f'])
      expect(quickpick.currIndex).toBe(0)
    })

    it('should respect configurations', async () => {
      helper.updateConfiguration('dialog.maxWidth', 30)
      helper.updateConfiguration('dialog.rounded', false)
      helper.updateConfiguration('dialog.floatHighlight', 'Normal')
      helper.updateConfiguration('dialog.floatBorderHighlight', 'Normal')
      helper.updateConfiguration('dialog.maxHeight', 2)
      await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }, { label: 'three' }],
        value: 'value',
        maxHeight: 2
      })
      let winids = await nvim.call('coc#float#get_float_win_list') as number[]
      let winid = Math.max(...winids)
      let win = nvim.createWindow(winid)
      let h = await win.height
      expect(h).toBe(2)
      await nvim.input('<esc>')
    })

    it('should change title', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }],
        title: 'from'
      })
      disposables.push(quickpick)
      quickpick.title = 'to'
      await helper.wait(30)
      expect(quickpick.title).toBe('to')
      let line = await getTitleLine()
      expect(line).toMatch(/to/)
    })

    it('should change loading', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }]
      })
      disposables.push(quickpick)
      quickpick.loading = true
      expect(quickpick.loading).toBe(true)
      quickpick.loading = false
      expect(quickpick.loading).toBe(false)
    })

    it('should change items', async () => {
      let quickpick = await window.createQuickPick({
        items: [{ label: 'one' }, { label: 'two' }]
      })
      disposables.push(quickpick)
      quickpick.onDidChangeValue(val => {
        if (val == '>') {
          quickpick.items = [{ label: 'three' }]
        }
      })
      await nvim.input('>')
      await helper.wait(30)
      let lines = await quickpick.buffer.lines
      expect(lines).toEqual(['three'])
    })

    it('should change activeItems', async () => {
      let quickpick = await window.createQuickPick<QuickPickItem>({
        items: [{ label: 'one' }]
      })
      disposables.push(quickpick)
      quickpick.onDidChangeValue(val => {
        if (val == 'f') {
          quickpick.activeItems = [{ label: 'foo', description: 'description' }, { label: 'foot' }]
        }
      })
      await nvim.input('f')
      await helper.wait(30)
      let lines = await quickpick.buffer.lines
      expect(lines).toEqual(['foo description', 'foot'])
    })
  })
})
