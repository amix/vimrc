'use strict'
import { CancellationToken } from 'vscode-languageserver-protocol'
import { CompleteOption, CompleteResult, ExtendedCompleteItem } from '../types'
import { fuzzyChar } from '../util/fuzzy'
import { byteSlice } from '../util/string'
import workspace from '../workspace'
import window from '../window'
import Source from './source'
const logger = require('../util/logger')('sources-source-vim')

export default class VimSource extends Source {

  private async callOptionalFunc(fname: string, args: any[]): Promise<any> {
    let exists = this.optionalFns.includes(fname)
    if (!exists) return null
    let name = `coc#source#${this.name}#${fname}`
    let res
    try {
      res = await this.nvim.call(name, args)
    } catch (e) {
      window.showMessage(`Vim error from source ${this.name}: ${e}`, 'error')
      return null
    }
    return res
  }

  public async shouldComplete(opt: CompleteOption): Promise<boolean> {
    let shouldRun = await super.shouldComplete(opt)
    if (!shouldRun) return false
    if (!this.optionalFns.includes('should_complete')) return true
    let res = await this.callOptionalFunc('should_complete', [opt])
    return !!res
  }

  public async refresh(): Promise<void> {
    await this.callOptionalFunc('refresh', [])
  }

  public async onCompleteDone(item: ExtendedCompleteItem, _opt: CompleteOption): Promise<void> {
    if (!this.optionalFns.includes('on_complete')) return
    await this.callOptionalFunc('on_complete', [item])
  }

  public onEnter(bufnr: number): void {
    if (!this.optionalFns.includes('on_enter')) return
    let doc = workspace.getDocument(bufnr)
    if (!doc) return
    let { filetypes } = this
    if (filetypes && !filetypes.includes(doc.filetype)) return
    this.callOptionalFunc('on_enter', [{
      bufnr,
      uri: doc.uri,
      languageId: doc.filetype
    }]).logError()
  }

  public async doComplete(opt: CompleteOption, token: CancellationToken): Promise<CompleteResult | null> {
    let { col, input, line, colnr } = opt
    let startcol: number | null = await this.callOptionalFunc('get_startcol', [opt])
    if (token.isCancellationRequested) return
    if (startcol) {
      if (startcol < 0) return null
      startcol = Number(startcol)
      // invalid startcol
      if (isNaN(startcol) || startcol < 0) startcol = col
      if (startcol !== col) {
        input = byteSlice(line, startcol, colnr - 1)
        opt = Object.assign({}, opt, {
          col: startcol,
          changed: col - startcol,
          input
        })
      }
    }
    let items: ExtendedCompleteItem[] = await this.nvim.callAsync('coc#util#do_complete', [this.name, opt])
    if (!items || items.length == 0 || token.isCancellationRequested) return null
    if (this.firstMatch && input.length) {
      let ch = input[0]
      items = items.filter(item => {
        let cfirst = item.filterText ? item.filterText[0] : item.word[0]
        return fuzzyChar(ch, cfirst)
      })
    }
    items = items.map(item => {
      if (typeof item == 'string') {
        return { word: item, menu: this.menu, isSnippet: this.isSnippet }
      }
      let menu = item.menu ? item.menu + ' ' : ''
      item.menu = `${menu}${this.menu}`
      item.isSnippet = this.isSnippet
      delete item.user_data
      return item
    })
    let res: CompleteResult = { items }
    if (startcol) res.startcol = startcol
    return res
  }
}
