import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { CancellationToken, Disposable } from 'vscode-languageserver-protocol'
import BasicList from '../../list/basic'
import manager from '../../list/manager'
import { IList, ListContext, ListItem, QuickfixItem } from '../../types'
import { disposeAll } from '../../util/index'
import window from '../../window'
import helper from '../helper'

class TestList extends BasicList {
  public name = 'test'
  public timeout = 3000
  public text = 'test'
  public detail = 'detail'
  public loadItems(_context: ListContext, token: CancellationToken): Promise<ListItem[]> {
    return new Promise(resolve => {
      let timer = setTimeout(() => {
        resolve([{ label: this.text }])
      }, this.timeout)
      token.onCancellationRequested(() => {
        if (timer) {
          clearTimeout(timer)
          resolve([])
        }
      })
    })
  }
}

let nvim: Neovim
let disposables: Disposable[] = []
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

const lineList: IList = {
  name: 'lines',
  actions: [{
    name: 'open',
    execute: async item => {
      await window.moveTo({
        line: (item as ListItem).data.line,
        character: 0
      })
      // noop
    }
  }],
  defaultAction: 'open',
  async loadItems(_context, _token): Promise<ListItem[]> {
    let lines = []
    for (let i = 0; i < 100; i++) {
      lines.push(i.toString())
    }
    return lines.map((line, idx) => ({
      label: line,
      data: { line: idx }
    }))
  }
}

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  await nvim.setVar('coc_jump_locations', locations)
})

afterAll(async () => {
  disposeAll(disposables)
  await helper.shutdown()
})

afterEach(async () => {
  manager.reset()
  await helper.reset()
})

describe('isValidAction()', () => {
  it('should check invalid action', async () => {
    let mappings = manager.mappings
    expect(mappings.isValidAction('foo')).toBe(false)
    expect(mappings.isValidAction('do:switch')).toBe(true)
    expect(mappings.isValidAction('eval:@*')).toBe(true)
    expect(mappings.isValidAction('undefined:undefined')).toBe(false)
  })
})

describe('User mappings', () => {
  it('should show warning for invalid key', async () => {
    let revert = helper.updateConfiguration('list.insertMappings', {
      xy: 'action:tabe',
    })
    await helper.wait(30)
    let msg = await helper.getCmdline()
    revert()
    await nvim.command('echo ""')
    expect(msg).toMatch('Invalid configuration')
    revert = helper.updateConfiguration('list.insertMappings', {
      '<M-x>': 'action:tabe',
    })
    await helper.wait(30)
    msg = await helper.getCmdline()
    revert()
    expect(msg).toMatch('Invalid configuration')
    revert = helper.updateConfiguration('list.insertMappings', {
      '<C-a>': 'foo:bar',
    })
    await helper.wait(30)
    msg = await helper.getCmdline()
    revert()
    expect(msg).toMatch('Invalid configuration')
  })

  it('should execute action keymap', async () => {
    let revert = helper.updateConfiguration('list.insertMappings', {
      '<C-d>': 'action:quickfix',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-d>')
    let buftype = await nvim.eval('&buftype')
    expect(buftype).toBe('quickfix')
    revert()
  })

  it('should execute expr keymap', async () => {
    await helper.mockFunction('TabOpen', 'quickfix')
    helper.updateConfiguration('list.insertMappings', {
      '<C-t>': 'expr:TabOpen',
    })
    helper.updateConfiguration('list.normalMappings', {
      t: 'expr:TabOpen',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-t>')
    let buftype = await nvim.eval('&buftype')
    expect(buftype).toBe('quickfix')
    await nvim.command('close')
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('t')
    buftype = await nvim.eval('&buftype')
    expect(buftype).toBe('quickfix')
  })

  it('should execute do mappings', async () => {
    helper.updateConfiguration('list.previousKeymap', '<C-j>')
    helper.updateConfiguration('list.nextKeymap', '<C-k>')
    helper.updateConfiguration('list.insertMappings', {
      '<C-n>': 'do:next',
      '<C-p>': 'do:previous',
      '<C-d>': 'do:exit',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-n>')
    let item = await manager.session?.ui.item
    expect(item.label).toMatch(locations[1].text)
    await helper.listInput('<C-p>')
    item = await manager.session?.ui.item
    expect(item.label).toMatch(locations[0].text)
    await helper.listInput('<C-k>')
    item = await manager.session?.ui.item
    expect(item.label).toMatch(locations[1].text)
    await helper.listInput('<C-j>')
    item = await manager.session?.ui.item
    expect(item.label).toMatch(locations[0].text)
    await helper.listInput('<C-d>')
    expect(manager.isActivated).toBe(false)
  })

  it('should execute prompt mappings', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-p>': 'prompt:previous',
      '<C-n>': 'prompt:next',
      '<C-a>': 'prompt:start',
      '<C-e>': 'prompt:end',
      '<Left>': 'prompt:left',
      '<Right>': 'prompt:right',
      '<backspace>': 'prompt:deleteforward',
      '<C-x>': 'prompt:deletebackward',
      '<C-k>': 'prompt:removetail',
      '<C-u>': 'prompt:removeahead',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    for (let key of ['<C-p>', '<C-n>', '<C-a>', '<C-e>', '<Left>', '<Right>', '<backspace>', '<C-x>', '<C-k>', '<C-u>']) {
      await helper.listInput(key)
    }
    expect(manager.isActivated).toBe(true)
  })

  it('should execute feedkeys keymap', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-f>': 'feedkeys:\\<C-f>',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-f>')
    let line = await nvim.call('line', '.')
    expect(line).toBe(locations.length)
  })

  it('should execute normal keymap', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-g>': 'normal:G',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-g>')
    let line = await nvim.call('line', '.')
    expect(line).toBe(locations.length)
  })

  it('should execute command keymap', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-w>': 'command:wincmd p',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-w>')
    expect(manager.isActivated).toBe(true)
    let winnr = await nvim.call('winnr')
    expect(winnr).toBe(1)
  })

  it('should execute call keymap', async () => {
    await helper.mockFunction('Test', 1)
    helper.updateConfiguration('list.insertMappings', {
      '<C-t>': 'call:Test',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-t>')
    expect(manager.isActivated).toBe(true)
  })

  it('should insert clipboard register to prompt', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-r>': 'prompt:paste',
    })
    await nvim.command('let @* = "foobar"')
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-r>')
    let { input } = manager.prompt
    expect(input).toMatch('foobar')
    await nvim.command('let @* = ""')
    await helper.listInput('<C-r>')
    expect(manager.prompt.input).toMatch('foobar')
  })

  it('should insert text from default register to prompt', async () => {
    helper.updateConfiguration('list.insertMappings', {
      '<C-v>': 'eval:@@',
    })
    await nvim.command('let @@ = "bar"')
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-v>')
    let { input } = manager.prompt
    expect(input).toMatch('bar')
  })
})

describe('doAction()', () => {
  it('should throw when action not found', async () => {
    let mappings = manager.mappings
    let fn = async () => {
      await mappings.doAction('foo:bar')
    }
    await expect(fn()).rejects.toThrow(/doesn't exist/)
  })

  it('should not throw when session does not exist', async () => {
    let mappings = manager.mappings
    await mappings.doAction('do:selectall')
    await mappings.doAction('do:help')
    await mappings.doAction('do:refresh')
    await mappings.doAction('do:toggle')
    await mappings.doAction('do:jumpback')
    await mappings.doAction('prompt:previous')
    await mappings.doAction('prompt:next')
    await mappings.doAction('do:refresh')
  })

  it('should not throw when action name does not exist', async () => {
    await helper.mockFunction('MyExpr', '')
    let mappings = manager.mappings
    await mappings.doAction('expr', 'MyExpr')
  })
})

describe('getAction()', () => {
  it('should throw for invalid action', async () => {
    let mappings = manager.mappings
    let fn = () => {
      mappings.getAction('foo')
    }
    expect(fn).toThrow(Error)
    fn = () => {
      mappings.getAction('do:bar')
    }
    expect(fn).toThrow(Error)
  })
})

describe('Default normal mappings', () => {
  it('should invoke action', async () => {
    await manager.start(['--normal', '--no-quit', 'location'])
    await manager.session.ui.ready
    let winid = manager.session.ui.winid
    await helper.listInput('t')
    let nr = await nvim.call('tabpagenr')
    expect(nr).toBe(2)
    await nvim.call('win_gotoid', [winid])
    await helper.listInput('s')
    let winnr = await nvim.call('winnr', ['$'])
    expect(winnr).toBe(3)
    await nvim.call('win_gotoid', [winid])
    await helper.listInput('d')
    let filename = await nvim.call('expand', ['%'])
    expect(filename).toMatch(path.basename(__filename))
    await nvim.call('win_gotoid', [winid])
    await helper.listInput('<cr>')
    filename = await nvim.call('expand', ['%'])
    expect(filename).toMatch(path.basename(__filename))
  })

  it('should select all items by <C-a>', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('<C-a>')
    let selected = manager.session?.ui.selectedItems
    expect(selected.length).toBe(locations.length)
  })

  it('should stop by <C-c>', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('<C-c>')
    let loading = manager.session?.worker.isLoading
    expect(loading).toBe(false)
  })

  it('should jump back by <C-o>', async () => {
    let doc = await helper.createDocument()
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('<C-o>')
    let bufnr = await nvim.call('bufnr', ['%'])
    expect(bufnr).toBe(doc.bufnr)
  })

  it('should scroll preview window by <C-e>, <C-y>', async () => {
    await helper.createDocument()
    await manager.start(['--auto-preview', '--normal', 'location'])
    await manager.session.ui.ready
    await helper.waitPreviewWindow()
    let winnr = await nvim.call('coc#list#has_preview') as number
    let winid = await nvim.call('win_getid', [winnr])
    await helper.listInput('<C-e>')
    let res = await nvim.call('getwininfo', [winid])
    expect(res[0].topline).toBeGreaterThan(1)
    await helper.listInput('<C-y>')
    res = await nvim.call('getwininfo', [winid])
    expect(res[0].topline).toBeLessThan(7)
  })

  it('should insert command by :', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput(':')
    await nvim.eval('feedkeys("let g:x = 1\\<cr>", "in")')
    let res = await nvim.getVar('x')
    expect(res).toBe(1)
  })

  it('should select action by <tab>', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    let p = helper.listInput('<tab>')
    await helper.wait(50)
    await nvim.input('t')
    await p
    let nr = await nvim.call('tabpagenr')
    expect(nr).toBe(2)
  })

  it('should preview by p', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('p')
    let winnr = await nvim.call('coc#list#has_preview')
    expect(winnr).toBe(2)
  })

  it('should stop task by <C-c>', async () => {
    disposables.push(manager.registerList(new TestList(nvim)))
    let p = manager.start(['--normal', 'test'])
    await helper.wait(50)
    await nvim.input('<C-c>')
    await p
    let len = manager.session?.ui.length
    expect(len).toBe(0)
  })

  it('should cancel list by <esc>', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await nvim.eval('feedkeys("\\<esc>", "in")')
    await helper.waitValue(() => {
      return manager.isActivated
    }, false)
  })

  it('should reload list by <C-l>', async () => {
    let list = new TestList(nvim)
    list.timeout = 0
    disposables.push(manager.registerList(list))
    await manager.start(['--normal', 'test'])
    await manager.session.ui.ready
    list.text = 'new'
    await helper.listInput('<C-l>')
    await helper.wait(30)
    let line = await nvim.line
    expect(line).toMatch('new')
  })

  it('should toggle selection <space>', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput(' ')
    let selected = manager.session?.ui.selectedItems
    expect(selected.length).toBe(1)
    await helper.listInput('k')
    await helper.listInput(' ')
    selected = manager.session?.ui.selectedItems
    expect(selected.length).toBe(0)
  })

  it('should change to insert mode by i, o, a', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    let keys = ['i', 'I', 'o', 'O', 'a', 'A']
    for (let key of keys) {
      await helper.listInput(key)
      let mode = manager.prompt.mode
      expect(mode).toBe('insert')
      await helper.listInput('<C-o>')
      mode = manager.prompt.mode
      expect(mode).toBe('normal')
    }
  })

  it('should show help by ?', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('?')
    let bufname = await nvim.call('bufname', '%')
    expect(bufname).toBe('[LIST HELP]')
  })
})

describe('list insert mappings', () => {
  it('should open by <cr>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<cr>')
    let bufname = await nvim.call('expand', ['%:p'])
    expect(bufname).toMatch('mappings.test.ts')
  })

  it('should paste input by <C-v>', async () => {
    await nvim.command('let @* = "foo"')
    await nvim.command('let @@ = "foo"')
    await nvim.call('setreg', ['*', 'foo'])
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-v>')
    let input = manager.prompt.input
    expect(input).toBe('foo')
  })

  it('should insert register content by <C-r>', async () => {
    await nvim.command('let @* = "foo"')
    await nvim.command('let @@ = "foo"')
    await nvim.call('setreg', ['*', 'foo'])
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-r>')
    await helper.listInput('*')
    let input = manager.prompt.input
    expect(input).toBe('foo')
    await helper.listInput('<C-r>')
    await helper.listInput('<')
    input = manager.prompt.input
    expect(input).toBe('foo')
    manager.prompt.reset()
  })

  it('should cancel by <esc>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<esc>')
    expect(manager.isActivated).toBe(false)
  })

  it('should select action by insert <tab>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    let p = helper.listInput('<tab>')
    await helper.wait(50)
    await nvim.input('d')
    await p
    let bufname = await nvim.call('bufname', ['%'])
    expect(bufname).toMatch(path.basename(__filename))
  })

  it('should select action for visual selected items', async () => {
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.wait(50)
    await nvim.input('V')
    await helper.wait(30)
    await nvim.input('2')
    await helper.wait(30)
    await nvim.input('j')
    await helper.wait(30)
    await manager.doAction('quickfix')
    let buftype = await nvim.eval('&buftype')
    expect(buftype).toBe('quickfix')
  })

  it('should stop loading by <C-c>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-c>')
    expect(manager.isActivated).toBe(true)
  })

  it('should reload by <C-l>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-l>')
    expect(manager.isActivated).toBe(true)
  })

  it('should change to normal mode by <C-o>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-o>')
    expect(manager.isActivated).toBe(true)
  })

  it('should select line by <down> and <up>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await nvim.eval('feedkeys("\\<down>", "in")')
    await nvim.eval('feedkeys("\\<up>", "in")')
    expect(manager.isActivated).toBe(true)
    let line = await nvim.line
    expect(line).toMatch('foo')
  })

  it('should move cursor by <left> and <right>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('f')
    await helper.listInput('<left>')
    await helper.listInput('<left>')
    await helper.listInput('a')
    await helper.listInput('<right>')
    await helper.listInput('<right>')
    await helper.listInput('c')
    let input = manager.prompt.input
    expect(input).toBe('afc')
  })

  it('should move cursor by <end> and <home>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<home>')
    await helper.listInput('<end>')
    await helper.listInput('a')
    let input = manager.prompt.input
    expect(input).toBe('a')
  })

  it('should move cursor by <PageUp> <PageDown> <C-d>', async () => {
    disposables.push(manager.registerList(lineList))
    await manager.start(['lines'])
    await manager.session.ui.ready
    await helper.listInput('<PageDown>')
    await helper.listInput('<PageUp>')
    await helper.listInput('<C-d>')
  })

  it('should scroll window by <C-f> and <C-b>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-f>')
    await helper.listInput('<C-b>')
  })

  it('should change input by <Backspace>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('f')
    await helper.listInput('<backspace>')
    let input = manager.prompt.input
    expect(input).toBe('')
  })

  it('should change input by <C-b>', async () => {
    let revert = helper.updateConfiguration('list.insertMappings', {
      '<C-b>': 'prompt:removetail',
    })
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('f')
    await helper.listInput('o')
    await helper.listInput('o')
    await helper.listInput('<C-a>')
    await helper.listInput('<C-b>')
    expect(manager.mappings.hasUserMapping('insert', '<C-b>')).toBe(true)
    let input = manager.prompt.input
    revert()
    expect(input).toBe('')
  })

  it('should change input by <C-h>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('f')
    await helper.listInput('<C-h>')
    let input = manager.prompt.input
    expect(input).toBe('')
  })

  it('should change input by <C-w>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('f')
    await helper.listInput('a')
    await helper.listInput('<C-w>')
    let input = manager.prompt.input
    expect(input).toBe('')
  })

  it('should change input by <C-u>', async () => {
    await manager.start(['--input=a', 'location'])
    await manager.session.ui.ready
    await helper.listInput('<C-u>')
    let input = manager.prompt.input
    expect(input).toBe('')
  })

  it('should change input by <C-n> and <C-p>', async () => {
    async function session(input: string): Promise<void> {
      await manager.start(['location'])
      await manager.session.ui.ready
      for (let ch of input) {
        await helper.listInput(ch)
      }
      await manager.cancel()
    }
    await session('foo')
    await session('bar')
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-n>')
    let input = manager.prompt.input
    expect(input.length).toBeGreaterThan(0)
    await helper.listInput('<C-p>')
    input = manager.prompt.input
    expect(input.length).toBeGreaterThan(0)
  })

  it('should change matcher by <C-s>', async () => {
    await manager.start(['location'])
    await manager.session.ui.ready
    await helper.listInput('<C-s>')
    let matcher = manager.session?.listOptions.matcher
    expect(matcher).toBe('strict')
    await helper.listInput('<C-s>')
    matcher = manager.session?.listOptions.matcher
    expect(matcher).toBe('regex')
    await helper.listInput('f')
    let len = manager.session?.ui.length
    expect(len).toBeGreaterThan(0)
  })
})

describe('evalExpression', () => {
  it('should exit list', async () => {
    helper.updateConfiguration('list.normalMappings', {
      t: 'do:exit',
    })
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    expect(manager.mappings.hasUserMapping('normal', 't')).toBe(true)
    await helper.listInput('t')
    expect(manager.isActivated).toBe(false)
  })

  it('should cancel prompt', async () => {
    helper.updateConfiguration('list.normalMappings', {
      t: 'do:cancel',
    })
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('t')
    let res = await nvim.call('coc#prompt#activated')
    expect(res).toBe(0)
  })

  it('should invoke normal command', async () => {
    let revert = helper.updateConfiguration('list.normalMappings', {
      x: 'normal!:G'
    })
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput('x')
    revert()
    let lnum = await nvim.call('line', ['.'])
    expect(lnum).toBeGreaterThan(1)
  })

  it('should toggle, scroll preview', async () => {
    let revert = helper.updateConfiguration('list.normalMappings', {
      '<space>': 'do:toggle',
      a: 'do:toggle',
      b: 'do:previewtoggle',
      c: 'do:previewup',
      d: 'do:previewdown',
      e: 'prompt:insertregister',
      f: 'do:stop',
      g: 'do:togglemode',
    })
    await manager.start(['--normal', 'location'])
    await manager.session.ui.ready
    await helper.listInput(' ')
    for (let key of ['a', 'b', 'c', 'd', 'e', 'f', 'g']) {
      await helper.listInput(key)
    }
    revert()
    expect(manager.isActivated).toBe(true)
  })
})
