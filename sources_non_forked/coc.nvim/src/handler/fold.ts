'use strict'
import { Neovim } from '@chemzqm/neovim'
import languages from '../languages'
import { HandlerDelegate } from '../types'

export default class FoldHandler {
  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
  }

  public async fold(kind?: string | 'comment' | 'region'): Promise<boolean> {
    let { doc, winid } = await this.handler.getCurrentState()
    this.handler.checkProvier('foldingRange', doc.textDocument)
    await doc.synchronize()
    let win = this.nvim.createWindow(winid)
    let foldlevel = await this.nvim.eval('&foldlevel') as number
    let ranges = await this.handler.withRequestToken('foldingrange', token => {
      return languages.provideFoldingRanges(doc.textDocument, {}, token)
    }, true)
    if (!ranges || !ranges.length) return false
    if (kind) ranges = ranges.filter(o => o.kind == kind)
    ranges.sort((a, b) => b.startLine - a.startLine)
    this.nvim.pauseNotification()
    win.setOption('foldmethod', 'manual', true)
    this.nvim.command('normal! zE', true)
    for (let range of ranges) {
      let { startLine, endLine } = range
      let cmd = `${startLine + 1}, ${endLine + 1}fold`
      this.nvim.command(cmd, true)
    }
    win.setOption('foldenable', true, true)
    win.setOption('foldlevel', foldlevel, true)
    await this.nvim.resumeNotification(true)
    return true
  }
}
