import { Neovim } from '@chemzqm/neovim'
import { EventEmitter } from 'events'
import { Disposable } from 'vscode-languageserver-protocol'
import BasicList from '../../list/basic'
import events from '../../events'
import manager from '../../list/manager'
import { ListItem, IList, ListTask } from '../../types'
import { disposeAll } from '../../util'
import helper from '../helper'

let labels: string[] = []
let lastItem: string

class SimpleList extends BasicList {
  public name = 'simple'
  constructor(nvim: Neovim) {
    super(nvim)
    this.addAction('open', item => {
      lastItem = item.label
    })
  }
  public loadItems(): Promise<ListItem[]> {
    return Promise.resolve(labels.map(s => {
      return { label: s, ansiHighlights: [{ span: [0, 1], hlGroup: 'MoreMsg' }] } as ListItem
    }))
  }
}

class SlowTask extends EventEmitter implements ListTask {
  private interval: NodeJS.Timer
  constructor() {
    super()
    let i = 0
    let interval = this.interval = setInterval(() => {
      i++
      this.emit('data', {
        label: i.toString(), highlights: {
          spans: [[0, 1]],
          hlGroup: 'Search'
        }
      })
      if (i == 5) {
        this.emit('end')
        clearInterval(interval)
      }
    }, 50)
  }

  public dispose(): void {
    clearInterval(this.interval)
    this.removeAllListeners()
  }
}

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
  disposeAll(disposables)
  manager.reset()
  await helper.reset()
})

describe('list ui', () => {
  describe('selectLines()', () => {
    it('should select lines', async () => {
      labels = ['foo', 'bar']
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['simple'])
      let ui = manager.session.ui
      await ui.ready
      await ui.selectLines(3, 1)
      let buf = await nvim.buffer
      let res = await buf.getSigns({ group: 'coc-list' })
      expect(res.length).toBe(2)
    })
  })

  describe('preselect', () => {
    it('should select preselect item', async () => {
      let list: IList = {
        actions: [{
          name: 'open',
          execute: () => {}
        }],
        name: 'preselect',
        defaultAction: 'open',
        loadItems: () => {
          return Promise.resolve([{ label: 'foo' }, { label: 'bar', preselect: true }])
        }
      }
      disposables.push(manager.registerList(list))
      await manager.start(['preselect'])
      let ui = manager.session.ui
      await ui.ready
      let line = await nvim.line
      expect(line).toBe('bar')
    })
  })

  describe('resume()', () => {
    it('should resume with selected lines', async () => {
      labels = ['foo', 'bar']
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['simple'])
      let ui = manager.session.ui
      await ui.ready
      await ui.selectLines(1, 2)
      await nvim.call('coc#window#close', [ui.winid])
      await helper.wait(100)
      await manager.session.resume()
      await helper.wait(100)
      let buf = await nvim.buffer
      let res = await buf.getSigns({ group: 'coc-list' })
      expect(res.length).toBe(2)
    })
  })

  describe('events', () => {
    async function mockMouse(winid: number, lnum: number): Promise<void> {
      await nvim.command(`let v:mouse_winid = ${winid}`)
      await nvim.command(`let v:mouse_lnum = ${lnum}`)
      await nvim.command('let v:mouse_col = 1')
    }

    it('should fire action on double click', async () => {
      labels = ['foo', 'bar']
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['simple'])
      let ui = manager.session.ui
      await ui.ready
      await mockMouse(ui.winid, 1)
      await manager.session.onMouseEvent('<2-LeftMouse>')
      await helper.wait(100)
      expect(lastItem).toBe('foo')
    })

    it('should select clicked line', async () => {
      labels = ['foo', 'bar']
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['simple'])
      let ui = manager.session.ui
      await ui.ready
      await mockMouse(ui.winid, 2)
      await ui.onMouse('mouseDown')
      await helper.wait(50)
      await mockMouse(ui.winid, 2)
      await ui.onMouse('mouseUp')
      await helper.wait(50)
      let item = await ui.item
      expect(item.label).toBe('bar')
    })

    it('should jump to original window on click', async () => {
      labels = ['foo', 'bar']
      let win = await nvim.window
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['simple'])
      let ui = manager.session.ui
      await ui.ready
      await mockMouse(win.id, 1)
      await ui.onMouse('mouseUp')
      await helper.wait(50)
      let curr = await nvim.window
      expect(curr.id).toBe(win.id)
    })

    it('should highlights items on CursorMoved', async () => {
      labels = (new Array(400)).fill('a')
      disposables.push(manager.registerList(new SimpleList(nvim)))
      await manager.start(['--normal', 'simple'])
      let ui = manager.session.ui
      await ui.ready
      await nvim.call('cursor', [350, 1])
      await events.fire('CursorMoved', [ui.bufnr, [350, 1]])
      await helper.wait(100)
      let res = await nvim.call('coc#highlight#get_highlights', [ui.bufnr, 'list'])
      expect(res.length).toBeGreaterThan(300)
    })
  })
})

describe('reversed list', () => {
  it('should render and add highlights', async () => {
    labels = ['a', 'b', 'c', 'd']
    disposables.push(manager.registerList(new SimpleList(nvim)))
    await manager.start(['--reverse', 'simple'])
    let ui = manager.session.ui
    await ui.ready
    let buf = nvim.createBuffer(ui.bufnr)
    let lines = await buf.lines
    expect(lines).toEqual(['d', 'c', 'b', 'a'])
    await helper.listInput('a')
    await helper.wait(50)
    lines = await buf.lines
    expect(lines).toEqual(['a'])
    let res = await nvim.call('coc#highlight#get_highlights', [ui.bufnr, 'list'])
    expect(res.length).toBe(2)
    let win = nvim.createWindow(ui.winid)
    let height = await win.height
    expect(height).toBe(1)
  })

  it('should moveUp and moveDown', async () => {
    labels = ['a', 'b', 'c', 'd']
    disposables.push(manager.registerList(new SimpleList(nvim)))
    await manager.start(['--reverse', 'simple'])
    let ui = manager.session.ui
    await ui.ready
    ui.moveUp()
    await helper.waitFor('line', ['.'], 3)
    ui.moveDown()
    await helper.waitFor('line', ['.'], 4)
  })

  it('should toggle selection', async () => {
    labels = ['a', 'b', 'c', 'd']
    disposables.push(manager.registerList(new SimpleList(nvim)))
    await manager.start(['--reverse', '--normal', 'simple'])
    let ui = manager.session.ui
    await ui.ready
    await ui.toggleSelection()
    let items = ui.selectedItems
    expect(items.length).toBeGreaterThan(0)
    expect(items[0].label).toBe('a')
    let lnum = await nvim.call('line', ['.'])
    expect(lnum).toBe(3)
    await helper.listInput('j')
    await ui.toggleSelection()
    items = ui.selectedItems
    expect(items.length).toBe(0)
  })

  it('should prepend list items', async () => {
    let o: any
    let p = new Promise(resolve => {
      let list: IList = {
        actions: [{
          name: 'open',
          execute: item => {
            o = item
          }
        }],
        name: 'slow',
        defaultAction: 'open',
        loadItems: () => {
          let task = new SlowTask()
          task.on('end', () => {
            resolve(undefined)
          })
          return Promise.resolve(task)
        }
      }
      disposables.push(manager.registerList(list))
      void manager.start(['--reverse', '--normal', 'slow'])
    })
    await p
    await helper.wait(50)
    let ui = manager.session.ui
    let buf = nvim.createBuffer(ui.bufnr)
    let lines = await buf.lines
    expect(lines).toEqual(['5', '4', '3', '2', '1'])
    let lnum = await nvim.call('line', ['.'])
    expect(lnum).toBe(5)
  })
})
