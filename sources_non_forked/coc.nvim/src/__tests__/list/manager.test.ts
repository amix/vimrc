import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import manager from '../../list/manager'
import events from '../../events'
import { QuickfixItem, IList, ListItem } from '../../types'
import helper from '../helper'

let nvim: Neovim
const locations: ReadonlyArray<QuickfixItem> = [{
  filename: __filename,
  col: 2,
  lnum: 1,
  text: 'foo'
}, {
  filename: __filename,
  col: 1,
  lnum: 2,
  text: 'Bar'
}, {
  filename: __filename,
  col: 1,
  lnum: 3,
  text: 'option'
}]

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  await nvim.setVar('coc_jump_locations', locations)
})

afterEach(async () => {
  manager.reset()
  await helper.reset()
})

afterAll(async () => {
  await helper.shutdown()
})

describe('list', () => {
  describe('events', () => {
    it('should cancel and enable prompt', async () => {
      let winid = await nvim.call('win_getid')
      await manager.start(['location'])
      await manager.session.ui.ready
      await nvim.call('win_gotoid', [winid])
      await helper.wait(50)
      let res = await nvim.call('coc#prompt#activated')
      expect(res).toBe(0)
      await nvim.command('wincmd p')
      await helper.wait(50)
      res = await nvim.call('coc#prompt#activated')
      expect(res).toBe(1)
    })
  })

  describe('list commands', () => {
    it('should be activated', async () => {
      await manager.start(['location'])
      await manager.session.ui.ready
      await helper.wait(50)
      expect(manager.isActivated).toBe(true)
      let line = await nvim.getLine()
      expect(line).toMatch(/manager.test.ts/)
    })

    it('should get list names', () => {
      let names = manager.names
      expect(names.length > 0).toBe(true)
    })

    it('should resume list', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await helper.wait(30)
      await nvim.eval('feedkeys("j", "in")')
      await helper.wait(30)
      let line = await nvim.call('line', '.')
      expect(line).toBe(2)
      await manager.cancel()
      await helper.wait(30)
      await manager.resume()
      await helper.wait(30)
      line = await nvim.call('line', '.')
      expect(line).toBe(2)
    })

    it('should not quit list with --no-quit', async () => {
      await manager.start(['--normal', '--no-quit', 'location'])
      await manager.session.ui.ready
      let winnr = await nvim.eval('win_getid()') as number
      await manager.doAction()
      await helper.wait(100)
      let wins = await nvim.windows
      let ids = wins.map(o => o.id)
      expect(ids).toContain(winnr)
    })

    it('should do default action for first item', async () => {
      await manager.start(['--normal', '--first', 'location'])
      await helper.wait(300)
      let name = await nvim.eval('bufname("%")') as string
      let filename = path.basename(__filename)
      expect(name.includes(filename)).toBe(true)
      let pos = await nvim.eval('getcurpos()')
      expect(pos[1]).toBe(1)
      expect(pos[2]).toBe(2)
    })

    it('should goto next & previous', async () => {
      await manager.start(['location'])
      await manager.session?.ui.ready
      await helper.wait(60)
      await manager.doAction()
      await manager.cancel()
      let bufname = await nvim.eval('expand("%:p")')
      expect(bufname).toMatch('manager.test.ts')
      await manager.next()
      let line = await nvim.call('line', '.')
      expect(line).toBe(2)
      await helper.wait(60)
      await manager.previous()
      line = await nvim.call('line', '.')
      expect(line).toBe(1)
    })

    it('should parse arguments', async () => {
      await manager.start(['--input=test', '--reverse', '--normal', '--no-sort', '--ignore-case', '--top', '--number-select', '--auto-preview', '--strict', 'location'])
      await helper.wait(30)
      let opts = manager.session?.listOptions
      expect(opts).toEqual({
        reverse: true,
        numberSelect: true,
        autoPreview: true,
        first: false,
        input: 'test',
        interactive: false,
        matcher: 'strict',
        ignorecase: true,
        position: 'top',
        mode: 'normal',
        noQuit: false,
        sort: false
      })
    })
  })

  describe('list configuration', () => {
    it('should change indicator', async () => {
      helper.updateConfiguration('list.indicator', '>>')
      await manager.start(['location'])
      await manager.session.ui.ready
      await helper.wait(200)
      let line = await helper.getCmdline()
      expect(line).toMatch('>>')
    })

    it('should split right for preview window', async () => {
      helper.updateConfiguration('list.previewSplitRight', true)
      let win = await nvim.window
      await manager.start(['location'])
      await helper.wait(100)
      await manager.doAction('preview')
      await helper.wait(100)
      manager.prompt.cancel()
      await helper.wait(10)
      await nvim.call('win_gotoid', [win.id])
      await nvim.command('wincmd l')
      let curr = await nvim.window
      let isPreview = await curr.getVar('previewwindow')
      expect(isPreview).toBe(1)
    })

    it('should toggle selection mode', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session?.ui.ready
      await nvim.input('V')
      await helper.wait(30)
      await nvim.input('1')
      await helper.wait(30)
      await nvim.input('j')
      await helper.wait(100)
      await manager.session?.ui.toggleSelection()
      let items = await manager.session?.ui.getItems()
      expect(items.length).toBe(2)
    })

    it('should change next/previous keymap', async () => {
      helper.updateConfiguration('list.nextKeymap', '<tab>')
      helper.updateConfiguration('list.previousKeymap', '<s-tab>')
      await manager.start(['location'])
      await manager.session.ui.ready
      await helper.wait(100)
      await nvim.eval('feedkeys("\\<tab>", "in")')
      await helper.wait(100)
      let line = await nvim.line
      expect(line).toMatch('Bar')
      await nvim.eval('feedkeys("\\<s-tab>", "in")')
      await helper.wait(100)
      line = await nvim.line
      expect(line).toMatch('foo')
    })

    it('should respect mouse events', async () => {
      async function setMouseEvent(line: number): Promise<void> {
        let winid = manager.session?.ui.winid
        await nvim.command(`let v:mouse_winid = ${winid}`)
        await nvim.command(`let v:mouse_lnum = ${line}`)
        await nvim.command(`let v:mouse_col = 1`)
      }
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      await helper.wait(100)
      await setMouseEvent(1)
      await manager.onNormalInput('<LeftMouse>')
      await setMouseEvent(2)
      await manager.onNormalInput('<LeftDrag>')
      await setMouseEvent(3)
      await manager.onNormalInput('<LeftRelease>')
      await helper.wait(100)
      let items = await manager.session?.ui.getItems()
      expect(items.length).toBe(3)
    })

    it('should toggle preview', async () => {
      await manager.start(['--normal', '--auto-preview', 'location'])
      await manager.session.ui.ready
      await helper.wait(100)
      await manager.togglePreview()
      await helper.wait(100)
      await manager.togglePreview()
      await helper.wait(100)
      let has = await nvim.call('coc#list#has_preview')
      expect(has).toBeGreaterThan(0)
    })

    it('should show help of current list', async () => {
      await manager.start(['--normal', '--auto-preview', 'location'])
      await helper.wait(200)
      await manager.session?.showHelp()
      let bufname = await nvim.call('bufname', '%')
      expect(bufname).toBe('[LIST HELP]')
    })

    it('should resolve list item', async () => {
      let list: IList = {
        name: 'test',
        actions: [{
          name: 'open', execute: _item => {
            // noop
          }
        }],
        defaultAction: 'open',
        loadItems: () => Promise.resolve([{ label: 'foo' }, { label: 'bar' }]),
        resolveItem: item => {
          item.label = item.label.slice(0, 1)
          return Promise.resolve(item)
        }
      }
      let disposable = manager.registerList(list)
      await manager.start(['--normal', 'test'])
      await manager.session.ui.ready
      await helper.wait(50)
      let line = await nvim.line
      expect(line).toBe('f')
      disposable.dispose()
    })
  })

  describe('descriptions', () => {
    it('should get descriptions', async () => {
      let res = manager.descriptions
      expect(res).toBeDefined()
      expect(res.location).toBeDefined()
    })
  })

  describe('loadItems()', () => {
    it('should load items for list', async () => {
      let res = await manager.loadItems('location')
      expect(res.length).toBeGreaterThan(0)
        ; (manager as any).lastSession = undefined
      manager.toggleMode()
      manager.stop()
    })
  })

  describe('onInsertInput()', () => {
    it('should handle insert input', async () => {
      await manager.onInsertInput('k')
      await manager.onInsertInput('<LeftMouse>')
      await manager.start(['--number-select', 'location'])
      await manager.session.ui.ready
      await manager.onInsertInput('1')
      await helper.wait(300)
      let bufname = await nvim.call('expand', ['%:p'])
      expect(bufname).toMatch('manager.test.ts')
    })

    it('should ignore invalid input', async () => {
      await manager.start(['location'])
      await manager.session.ui.ready
      await manager.onInsertInput('<X-y>')
      await manager.onInsertInput(String.fromCharCode(65533))
      await manager.onInsertInput(String.fromCharCode(30))
      expect(manager.isActivated).toBe(true)
    })

    it('should ignore <plug> insert', async () => {
      await manager.start(['location'])
      await manager.session.ui.ready
      await nvim.eval('feedkeys("\\<plug>x", "in")')
      await helper.wait(50)
      expect(manager.isActivated).toBe(true)
    })
  })

  describe('parseArgs()', () => {
    it('should show error for bad option', async () => {
      manager.parseArgs(['$x', 'location'])
      let msg = await helper.getCmdline()
      expect(msg).toMatch('Invalid list option')
    })

    it('should show error for option that does not exist', async () => {
      manager.parseArgs(['-xyz', 'location'])
      let msg = await helper.getCmdline()
      expect(msg).toMatch('Invalid option')
    })

    it('should show error for interactive with list not support interactive', async () => {
      manager.parseArgs(['--interactive', 'location'])
      let msg = await helper.getCmdline()
      expect(msg).toMatch('not supported')
    })
  })

  describe('resume()', () => {
    it('should resume by name', async () => {
      await events.fire('FocusGained', [])
      await manager.start(['location'])
      await manager.session.ui.ready
      await manager.session.hide()
      await helper.wait(100)
      await manager.resume('location')
      expect(manager.isActivated).toBe(true)
    })
  })

  describe('first(), last()', () => {
    it('should get session by name', async () => {
      let last: string
      let list: IList = {
        name: 'test',
        actions: [{
          name: 'open',
          execute: (item: ListItem) => {
            last = item.label
          }
        }],
        defaultAction: 'open',
        loadItems: () => Promise.resolve([{ label: 'foo' }, { label: 'bar' }])
      }
      manager.registerList(list)
      await manager.start(['test'])
      await manager.session.ui.ready
      await manager.first('a')
      await manager.last('a')
      await manager.first('test')
      expect(last).toBe('foo')
      await manager.last('test')
      expect(last).toBe('bar')
    })
  })

  describe('registerList()', () => {
    it('should recreat list', async () => {
      let list: IList = {
        name: 'test',
        actions: [{
          name: 'open', execute: _item => {
            // noop
          }
        }],
        defaultAction: 'open',
        loadItems: () => Promise.resolve([{ label: 'foo' }, { label: 'bar' }])
      }
      manager.registerList(list)
      helper.updateConfiguration('list.source.test.defaultAction', 'open')
      let disposable = manager.registerList(list)
      disposable.dispose()
      await helper.wait(30)
      let msg = await helper.getCmdline()
      expect(msg).toMatch('recreated')
    })
  })

  describe('start()', () => {
    it('should show error when loadItems throws', async () => {
      let list: IList = {
        name: 'test',
        actions: [{
          name: 'open',
          execute: (_item: ListItem) => {
          }
        }],
        defaultAction: 'open',
        loadItems: () => {
          throw new Error('test error')
        }
      }
      manager.registerList(list)
      await manager.start(['test'])
      await helper.wait(100)
    })
  })

  describe('list options', () => {
    it('should respect auto preview option', async () => {
      await manager.start(['--auto-preview', 'location'])
      await manager.session.ui.ready
      await helper.waitFor('winnr', ['$'], 3)
      let previewWinnr = await nvim.call('coc#list#has_preview')
      expect(previewWinnr).toBe(2)
      let bufnr = await nvim.call('winbufnr', previewWinnr)
      let buf = nvim.createBuffer(bufnr)
      let name = await buf.name
      expect(name).toMatch('manager.test.ts')
      await nvim.eval('feedkeys("j", "in")')
      await helper.wait(100)
      let winnr = await nvim.call('coc#list#has_preview')
      expect(winnr).toBe(previewWinnr)
    })

    it('should respect input option', async () => {
      await manager.start(['--input=foo', 'location'])
      await manager.session.ui.ready
      await helper.wait(30)
      let line = await helper.getCmdline()
      expect(line).toMatch('foo')
      expect(manager.isActivated).toBe(true)
    })

    it('should respect regex filter', async () => {
      await manager.start(['--input=f.o', '--regex', 'location'])
      await helper.wait(200)
      let item = await manager.session?.ui.item
      expect(item.label).toMatch('foo')
    })

    it('should respect normal option', async () => {
      await manager.start(['--normal', 'location'])
      await manager.session.ui.ready
      let line = await helper.getCmdline()
      expect(line).toBe('')
    })

    it('should respect nosort option', async () => {
      await manager.start(['--ignore-case', '--no-sort', 'location'])
      await manager.session.ui.ready
      expect(manager.isActivated).toBe(true)
      await nvim.input('oo')
      await helper.wait(100)
      let line = await nvim.call('getline', ['.'])
      expect(line).toMatch('foo')
    })

    it('should respect ignorecase option', async () => {
      await manager.start(['--ignore-case', '--strict', 'location'])
      await manager.session.ui.ready
      expect(manager.isActivated).toBe(true)
      await nvim.input('bar')
      await helper.wait(100)
      let n = manager.session?.ui.length
      expect(n).toBe(1)
      let line = await nvim.line
      expect(line).toMatch('Bar')
    })

    it('should respect top option', async () => {
      await manager.start(['--top', 'location'])
      await manager.session.ui.ready
      let nr = await nvim.call('winnr')
      expect(nr).toBe(1)
    })

    it('should respect number select option', async () => {
      await manager.start(['--number-select', 'location'])
      await manager.session.ui.ready
      await helper.wait(100)
      await nvim.eval('feedkeys("2", "in")')
      await helper.wait(100)
      let lnum = locations[1].lnum
      let curr = await nvim.call('line', '.')
      expect(lnum).toBe(curr)
    })

    it('should respect tab option', async () => {
      await manager.start(['--tab', '--auto-preview', 'location'])
      await manager.session.ui.ready
      await helper.wait(100)
      await nvim.command('wincmd l')
      let previewwindow = await nvim.eval('w:previewwindow')
      expect(previewwindow).toBe(1)
    })
  })
})
