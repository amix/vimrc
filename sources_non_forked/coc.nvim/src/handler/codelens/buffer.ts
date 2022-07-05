'use strict'
import { Neovim } from '@chemzqm/neovim'
import debounce from 'debounce'
import { CancellationTokenSource, CodeLens, Command } from 'vscode-languageserver-protocol'
import { SyncItem } from '../../model/bufferSync'
import commandManager from '../../commands'
import languages from '../../languages'
import Document from '../../model/document'
import window from '../../window'
import workspace from '../../workspace'
import { DidChangeTextDocumentParams } from '../../types'
const logger = require('../../util/logger')('codelens-buffer')

export interface CodeLensInfo {
  codeLenses: CodeLens[]
  version: number
}

export interface CodeLensConfig {
  position: 'top' | 'eol' | 'right_align'
  enabled: boolean
  separator: string
  subseparator: string
  srcId?: number
}

/**
 * CodeLens buffer
 */
export default class CodeLensBuffer implements SyncItem {
  private codeLenses: CodeLensInfo
  private tokenSource: CancellationTokenSource
  private resolveTokenSource: CancellationTokenSource
  public resolveCodeLens: (() => void) & { clear(): void }
  constructor(
    private nvim: Neovim,
    public readonly document: Document,
    private config: CodeLensConfig
  ) {
    this.resolveCodeLens = debounce(() => {
      void this._resolveCodeLenses()
    }, global.__TEST__ ? 20 : 200)
    void this.fetchCodeLenses()
  }

  private get bufnr(): number {
    return this.document.bufnr
  }

  public onChange(e: DidChangeTextDocumentParams): void {
    if (e.contentChanges.length === 0 && this.codeLenses != null) {
      void this._resolveCodeLenses()
    } else {
      this.cancel()
      void this.fetchCodeLenses()
    }
  }

  public get currentCodeLens(): CodeLens[] | undefined {
    return this.codeLenses?.codeLenses
  }

  private get enabled(): boolean {
    if (!this.document?.attached) return false
    return this.config.enabled && languages.hasProvider('codeLens', this.document.textDocument)
  }

  public async forceFetch(): Promise<void> {
    if (!this.enabled) return
    await this.document.synchronize()
    this.cancel()
    await this.fetchCodeLenses()
  }

  private async fetchCodeLenses(): Promise<void> {
    if (!this.enabled) return
    this.cancel()
    let noFetch = this.codeLenses?.version == this.document.version
    if (!noFetch) {
      let { textDocument } = this.document
      let version = textDocument.version
      let tokenSource = this.tokenSource = new CancellationTokenSource()
      let token = tokenSource.token
      let codeLenses = await languages.getCodeLens(textDocument, token)
      codeLenses = Array.isArray(codeLenses) ? codeLenses.filter(o => o != null) : []
      this.tokenSource = undefined
      if (token.isCancellationRequested || codeLenses.length == 0) return
      this.codeLenses = { version, codeLenses }
    }
    await this._resolveCodeLenses()
  }

  /**
   * Resolve visible codeLens
   */
  private async _resolveCodeLenses(): Promise<void> {
    if (!this.enabled || !this.codeLenses || this.isChanged) return
    if (!workspace.has('nvim-0.4.0')) return
    let { codeLenses } = this.codeLenses
    let [bufnr, start, end] = await this.nvim.eval(`[bufnr('%'),line('w0'),line('w$')]`) as [number, number, number]
    // only resolve current buffer
    if (this.isChanged || bufnr != this.bufnr) return
    if (this.resolveTokenSource) this.resolveTokenSource.cancel()
    codeLenses = codeLenses.filter(o => {
      let lnum = o.range.start.line + 1
      return lnum >= start && lnum <= end
    })
    if (codeLenses.length) {
      let tokenSource = this.resolveTokenSource = new CancellationTokenSource()
      let token = tokenSource.token
      await Promise.all(codeLenses.map(codeLens => languages.resolveCodeLens(codeLens, token)))
      this.resolveTokenSource = undefined
      if (token.isCancellationRequested || this.isChanged) return
    }
    this.nvim.pauseNotification()
    this.clear(start - 1, end)
    this.setVirtualText(codeLenses)
    this.nvim.resumeNotification(false, true)
  }

  private get isChanged(): boolean {
    if (!this.codeLenses || this.document.dirty) return true
    let { version } = this.codeLenses
    return this.document.textDocument.version !== version
  }

  /**
   * Attach resolved codeLens
   */
  private setVirtualText(codeLenses: CodeLens[]): void {
    let { document } = this
    if (!document || !codeLenses.length) return
    let list: Map<number, CodeLens[]> = new Map()
    let { position } = this.config
    for (let codeLens of codeLenses) {
      let { range, command } = codeLens
      if (!command) continue
      let { line } = range.start
      if (list.has(line)) {
        list.get(line).push(codeLens)
      } else {
        list.set(line, [codeLens])
      }
    }
    for (let lnum of list.keys()) {
      let codeLenses = list.get(lnum)
      let commands = codeLenses.map(codeLens => codeLens.command)
      commands = commands.filter(c => c && c.title)
      let chunks: [string, string][] = []
      let n_commands = commands.length
      for (let i = 0; i < n_commands; i++) {
        let c = commands[i]
        chunks.push([c.title.replace(/(\r\n|\r|\n|\s)+/g, " "), 'CocCodeLens'] as [string, string])
        if (i != n_commands - 1) {
          chunks.push([this.config.subseparator, 'CocCodeLens'] as [string, string])
        }
      }
      if (this.config.separator) {
        chunks.unshift([`${this.config.separator} `, 'CocCodeLens'])
      }
      let { srcId } = this.config
      if (workspace.has('nvim-0.6.0')) {
        let buf = this.document.buffer
        let line = document.getline(lnum)
        if (position == 'top') {
          let indent = line.match(/^\s*/)[0]
          if (indent.length > 0) chunks.unshift([indent, 'Normal'])
          buf.setExtMark(srcId, lnum, 0, {
            virt_lines: [chunks],
            virt_lines_above: true
          })
        } else {
          buf.setExtMark(srcId, lnum, 0, {
            hl_mode: 'combine',
            virt_text: chunks,
            virt_text_pos: position
          })
        }
      } else {
        this.nvim.call('nvim_buf_set_virtual_text', [this.bufnr, srcId, lnum, chunks, {}], true)
      }
    }
  }

  public clear(start = 0, end = -1): void {
    let { srcId } = this.config
    if (!srcId) return
    let buf = this.nvim.createBuffer(this.bufnr)
    buf.clearNamespace(srcId, start, end)
  }

  public async doAction(line: number): Promise<void> {
    let commands = getCommands(line, this.codeLenses?.codeLenses)
    if (commands.length == 1) {
      await commandManager.execute(commands[0])
    } else if (commands.length > 1) {
      let res = await window.showMenuPicker(commands.map(c => c.title))
      if (res != -1) await commandManager.execute(commands[res])
    }
  }

  private cancel(): void {
    this.resolveCodeLens.clear()
    if (this.resolveTokenSource) {
      this.resolveTokenSource.cancel()
      this.resolveTokenSource.dispose()
      this.resolveTokenSource = null
    }
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource.dispose()
      this.tokenSource = null
    }
  }

  public dispose(): void {
    this.cancel()
    this.codeLenses = undefined
  }
}

export function getCommands(line: number, codeLenses: CodeLens[] | undefined): Command[] {
  if (!codeLenses?.length) return []
  let commands: Command[] = []
  for (let codeLens of codeLenses) {
    let { range, command } = codeLens
    if (!command) continue
    if (line == range.start.line) {
      commands.push(command)
    }
  }
  return commands
}
