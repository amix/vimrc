'use strict'
import { Neovim } from '@chemzqm/neovim'
import commandManager from '../../commands'
import Mru from '../../model/mru'
import { ListContext, ListItem } from '../../types'
import workspace from '../../workspace'
import BasicList from '../basic'
import { formatListItems, UnformattedListItem } from '../formatting'

export default class CommandsList extends BasicList {
  public defaultAction = 'run'
  public description = 'registered commands of coc.nvim'
  public readonly name = 'commands'
  private mru: Mru

  constructor(nvim: Neovim) {
    super(nvim)
    this.mru = workspace.createMru('commands')
    this.addAction('run', async item => {
      await commandManager.fireCommand(item.data.cmd)
    })
    this.addAction('append', async item => {
      let { cmd } = item.data
      await nvim.feedKeys(`:CocCommand ${cmd} `, 'n', false)
    })
  }

  public async loadItems(_context: ListContext): Promise<ListItem[]> {
    let items: UnformattedListItem[] = []
    let mruList = await this.mru.load()
    let { commandList, onCommandList, titles } = commandManager
    let ids = commandList.map(c => c.id).concat(onCommandList)
    for (const id of [...new Set(ids)]) {
      items.push({
        label: [id, ...(titles.get(id) ? [titles.get(id)] : [])],
        filterText: id,
        data: { cmd: id, score: score(mruList, id) }
      })
    }
    items.sort((a, b) => b.data.score - a.data.score)
    return formatListItems(this.alignColumns, items)
  }

  public doHighlight(): void {
    let { nvim } = this
    nvim.pauseNotification()
    nvim.command('syntax match CocCommandsTitle /\\t.*$/ contained containedin=CocCommandsLine', true)
    nvim.command('highlight default link CocCommandsTitle Comment', true)
    nvim.resumeNotification(false, true)
  }
}

function score(list: string[], key: string): number {
  let idx = list.indexOf(key)
  return idx == -1 ? -1 : list.length - idx
}
