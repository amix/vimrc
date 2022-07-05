import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import CommandsHandler from '../../handler/commands'
import commandManager from '../../commands'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let commands: CommandsHandler
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  commands = (helper.plugin as any).handler.commands
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  await helper.createDocument()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('Commands', () => {
  describe('addVimCommand', () => {
    it('should register global vim commands', async () => {
      await commandManager.executeCommand('vim.config')
      await helper.wait(50)
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toMatch('coc-settings.json')
      let list = commands.getCommandList()
      expect(list.includes('vim.config')).toBe(true)
    })

    it('should add vim command with title', async () => {
      commands.addVimCommand({ id: 'list', cmd: 'CocList', title: 'list of coc.nvim' })
      let res = commandManager.titles.get('vim.list')
      expect(res).toBe('list of coc.nvim')
      commandManager.unregister('vim.list')
    })
  })

  describe('getCommands', () => {
    it('should get command items', async () => {
      let res = commands.getCommands()
      let idx = res.findIndex(o => o.id == 'workspace.showOutput')
      expect(idx != -1).toBe(true)
    })
  })

  describe('repeat', () => {
    it('should repeat command', async () => {
      // let buf = await nvim.buffer
      await nvim.call('setline', [1, ['a', 'b', 'c']])
      await nvim.call('cursor', [1, 1])
      commands.addVimCommand({ id: 'remove', cmd: 'normal! dd' })
      await commands.runCommand('vim.remove')
      await helper.wait(50)
      let res = await nvim.call('getline', [1, '$'])
      expect(res).toEqual(['b', 'c'])
      await commands.repeat()
      await helper.wait(50)
      res = await nvim.call('getline', [1, '$'])
      expect(res).toEqual(['c'])
    })
  })

  describe('runCommand', () => {
    it('should open command list without id', async () => {
      await commands.runCommand()
      await helper.wait(100)
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toBe('list:///commands')
    })
  })
})
