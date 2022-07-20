'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Range, WorkspaceEdit } from 'vscode-languageserver-protocol'
import languages from '../languages'
import { HandlerDelegate } from '../types'
import { emptyRange } from '../util/position'
import window from '../window'
import workspace from '../workspace'
const logger = require('../util/logger')('handler-rename')

export default class Rename {
  constructor(
    private nvim: Neovim,
    private handler: HandlerDelegate) {
  }

  public async getWordEdit(): Promise<WorkspaceEdit> {
    let { doc, position } = await this.handler.getCurrentState()
    let range = doc.getWordRangeAtPosition(position)
    if (!range || emptyRange(range)) return null
    let curname = doc.textDocument.getText(range)
    if (languages.hasProvider('rename', doc.textDocument)) {
      await doc.synchronize()
      let requestTokenSource = new CancellationTokenSource()
      let res = await languages.prepareRename(doc.textDocument, position, requestTokenSource.token)
      if (res === false) return null
      let newName = curname.startsWith('a') ? 'b' : 'a'
      let edit = await languages.provideRenameEdits(doc.textDocument, position, newName, requestTokenSource.token)
      if (edit) return edit
    }
    window.showMessage('Rename provider not found, extract word ranges from current buffer', 'more')
    let ranges = doc.getSymbolRanges(curname)
    return {
      changes: {
        [doc.uri]: ranges.map(r => ({ range: r, newText: curname }))
      }
    }
  }

  public async rename(newName?: string): Promise<boolean> {
    let { doc, position } = await this.handler.getCurrentState()
    this.handler.checkProvier('rename', doc.textDocument)
    await doc.synchronize()
    let token = (new CancellationTokenSource()).token
    let res = await languages.prepareRename(doc.textDocument, position, token)
    if (res === false) {
      void window.showWarningMessage('Invalid position for rename')
      return false
    }
    let curname: string
    if (!newName) {
      if (Range.is(res)) {
        curname = doc.textDocument.getText(res)
        await window.moveTo(res.start)
      } else if (res && typeof res.placeholder === 'string') {
        curname = res.placeholder
      } else {
        curname = await this.nvim.eval('expand("<cword>")') as string
      }
      const config = workspace.getConfiguration('coc.preferences')
      newName = await window.requestInput('New name', config.get<boolean>('renameFillCurrent', true) ? curname : undefined)
    }
    if (newName === '') void window.showWarningMessage('Empty word, rename canceled')
    if (!newName) return false
    let edit = await languages.provideRenameEdits(doc.textDocument, position, newName, token)
    if (token.isCancellationRequested || !edit) return false
    await workspace.applyEdit(edit)
    this.nvim.redrawVim()
    return true
  }
}
