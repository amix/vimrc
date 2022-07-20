'use strict'
import { Neovim } from '@chemzqm/neovim'
import commandManager from '../commands'
import listManager from '../list/manager'
import { Env } from '../types'
const logger = require('../util/logger')('handler-commands')

interface CommandItem {
  id: string
  title: string
}

export default class Commands {
  constructor(private nvim: Neovim, private env: Readonly<Env>) {
    for (let item of env.vimCommands) {
      this.addVimCommand(item)
    }
  }

  public addVimCommand(cmd: { id: string; cmd: string; title?: string }): void {
    let id = `vim.${cmd.id}`
    commandManager.registerCommand(id, () => {
      this.nvim.command(cmd.cmd, true)
      this.nvim.redrawVim()
    })
    if (cmd.title) commandManager.titles.set(id, cmd.title)
  }

  public getCommandList(): string[] {
    return commandManager.commandList.map(o => o.id)
  }

  public async repeat(): Promise<void> {
    await commandManager.repeatCommand()
  }

  public async runCommand(id?: string, ...args: any[]): Promise<unknown> {
    if (id) return await commandManager.fireCommand(id, ...args)
    await listManager.start(['commands'])
  }

  public getCommands(): CommandItem[] {
    let list = commandManager.commandList
    let res: CommandItem[] = []
    let { titles } = commandManager
    for (let item of list) {
      res.push({
        id: item.id,
        title: titles.get(item.id) || ''
      })
    }
    return res
  }
}
