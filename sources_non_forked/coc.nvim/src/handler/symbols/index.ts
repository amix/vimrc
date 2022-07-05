'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable, Range, SymbolInformation } from 'vscode-languageserver-protocol'
import events from '../../events'
import languages from '../../languages'
import BufferSync from '../../model/bufferSync'
import { HandlerDelegate } from '../../types'
import { disposeAll, wait } from '../../util/index'
import { equals } from '../../util/object'
import { positionInRange, rangeInRange } from '../../util/position'
import window from '../../window'
import workspace from '../../workspace'
import SymbolsBuffer from './buffer'
import Outline from './outline'
import { convertSymbols, SymbolInfo } from './util'

export default class Symbols {
  private buffers: BufferSync<SymbolsBuffer>
  private disposables: Disposable[] = []
  private outline: Outline
  private autoUpdateBufnrs: Set<number> = new Set()

  constructor(
    private nvim: Neovim,
    private handler: HandlerDelegate
  ) {
    this.buffers = workspace.registerBufferSync(doc => {
      if (doc.buftype != '') return undefined
      let buf = new SymbolsBuffer(doc.bufnr, this.autoUpdateBufnrs)
      buf.onDidUpdate(symbols => {
        if (!this.outline) return
        this.outline.onSymbolsUpdate(buf.bufnr, symbols)
      })
      return buf
    })
    this.outline = new Outline(nvim, this.buffers, handler)
    events.on('CursorHold', async (bufnr: number) => {
      if (!this.functionUpdate || !this.buffers.getItem(bufnr)) return
      await this.getCurrentFunctionSymbol(bufnr)
    }, null, this.disposables)
    events.on('InsertEnter', (bufnr: number) => {
      let buf = this.buffers.getItem(bufnr)
      if (buf) buf.cancel()
    }, null, this.disposables)
  }

  public get functionUpdate(): boolean {
    let config = workspace.getConfiguration('coc.preferences')
    return config.get<boolean>('currentFunctionSymbolAutoUpdate', false)
  }

  public get labels(): { [key: string]: string } {
    return workspace.getConfiguration('suggest').get<any>('completionItemKindLabels', {})
  }

  public async getWorkspaceSymbols(input: string): Promise<SymbolInformation[]> {
    this.handler.checkProvier('workspaceSymbols', null)
    let tokenSource = new CancellationTokenSource()
    return await languages.getWorkspaceSymbols(input, tokenSource.token)
  }

  public async resolveWorkspaceSymbol(symbolInfo: SymbolInformation): Promise<SymbolInformation> {
    if (symbolInfo.location?.uri) return symbolInfo
    let tokenSource = new CancellationTokenSource()
    return await languages.resolveWorkspaceSymbol(symbolInfo, tokenSource.token)
  }

  public async getDocumentSymbols(bufnr?: number): Promise<SymbolInfo[] | undefined> {
    if (!bufnr) {
      let doc = await workspace.document
      if (!doc || doc.isCommandLine || !doc.attached) return undefined
      await wait(1)
      bufnr = doc.bufnr
    }
    let buf = this.buffers.getItem(bufnr)
    if (!buf) return
    let res = await buf.getSymbols()
    return res ? convertSymbols(res) : undefined
  }

  public async getCurrentFunctionSymbol(bufnr?: number): Promise<string> {
    if (!bufnr) bufnr = await this.nvim.call('bufnr', ['%'])
    let doc = workspace.getDocument(bufnr)
    if (!doc || !doc.attached) return
    if (!languages.hasProvider('documentSymbol', doc.textDocument)) return
    let position = await window.getCursorPosition()
    let symbols = await this.getDocumentSymbols(bufnr)
    let buffer = this.nvim.createBuffer(bufnr)
    if (!symbols || symbols.length === 0) {
      buffer.setVar('coc_current_function', '', true)
      this.nvim.call('coc#util#do_autocmd', ['CocStatusChange'], true)
      return ''
    }
    symbols = symbols.filter(s => [
      'Class',
      'Method',
      'Function',
      'Struct',
    ].includes(s.kind))
    let functionName = ''
    for (let sym of symbols.reverse()) {
      if (sym.range
        && positionInRange(position, sym.range) == 0
        && !sym.text.endsWith(') callback')) {
        functionName = sym.text
        let label = this.labels[sym.kind.toLowerCase()]
        if (label) functionName = `${label} ${functionName}`
        break
      }
    }
    if (this.functionUpdate) {
      buffer.setVar('coc_current_function', functionName, true)
      this.nvim.call('coc#util#do_autocmd', ['CocStatusChange'], true)
    }
    return functionName
  }

  /*
   * supportedSymbols must be string values of symbolKind
   */
  public async selectSymbolRange(inner: boolean, visualmode: string, supportedSymbols: string[]): Promise<void> {
    let { doc } = await this.handler.getCurrentState()
    this.handler.checkProvier('documentSymbol', doc.textDocument)
    let range: Range
    if (visualmode) {
      range = await window.getSelectedRange(visualmode)
    } else {
      let pos = await window.getCursorPosition()
      range = Range.create(pos, pos)
    }
    let symbols = await this.getDocumentSymbols(doc.bufnr)
    if (!symbols || symbols.length === 0) {
      window.showMessage('No symbols found', 'warning')
      return
    }
    symbols = symbols.filter(s => supportedSymbols.includes(s.kind))
    let selectRange: Range
    for (let sym of symbols.reverse()) {
      if (sym.range && !equals(sym.range, range) && rangeInRange(range, sym.range)) {
        selectRange = sym.range
        break
      }
    }
    if (inner && selectRange) {
      let { start, end } = selectRange
      let line = doc.getline(start.line + 1)
      let endLine = doc.getline(end.line - 1)
      selectRange = Range.create(start.line + 1, line.match(/^\s*/)[0].length, end.line - 1, endLine.length)
    }
    if (selectRange) {
      await window.selectRange(selectRange)
    } else if (['v', 'V', '\x16'].includes(visualmode)) {
      await this.nvim.command('normal! gv')
    }
  }

  public async showOutline(keep?: number): Promise<void> {
    await this.outline.show(keep)
  }

  public async hideOutline(): Promise<void> {
    await this.outline.hide()
  }

  public hasOutline(bufnr: number): boolean {
    return this.outline.has(bufnr)
  }

  public dispose(): void {
    this.outline.dispose()
    this.buffers.dispose()
    disposeAll(this.disposables)
  }
}
