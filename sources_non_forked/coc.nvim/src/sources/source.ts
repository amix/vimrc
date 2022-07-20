'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationToken } from 'vscode-languageserver-protocol'
import { CompleteOption, CompleteResult, ISource, SourceConfig, SourceType, VimCompleteItem } from '../types'
import { byteSlice } from '../util/string'
import workspace from '../workspace'
const logger = require('../util/logger')('sources-source')

export default class Source implements ISource {
  public readonly name: string
  public readonly filepath: string
  public readonly sourceType: SourceType
  public readonly isSnippet: boolean
  protected readonly nvim: Neovim
  private _disabled = false
  private defaults: any
  constructor(option: SourceConfig) {
    this.nvim = workspace.nvim
    // readonly properties
    this.name = option.name
    this.filepath = option.filepath || ''
    this.sourceType = option.sourceType || SourceType.Native
    this.isSnippet = !!option.isSnippet
    this.defaults = option
  }

  /**
   * Priority of source, higher priority makes items lower index.
   */
  public get priority(): number {
    return this.getConfig('priority', 1)
  }

  /**
   * When triggerOnly is true, not trigger completion on keyword character insert.
   */
  public get triggerOnly(): boolean {
    let triggerOnly = this.defaults['triggerOnly']
    if (typeof triggerOnly == 'boolean') return triggerOnly
    if (!this.triggerCharacters && !this.triggerPatterns) return false
    return Array.isArray(this.triggerPatterns) && this.triggerPatterns.length != 0
  }

  public get triggerCharacters(): string[] {
    return this.getConfig('triggerCharacters', null)
  }

  // exists opitonnal function names for remote source
  public get optionalFns(): string[] {
    return this.defaults['optionalFns'] || []
  }

  public get triggerPatterns(): RegExp[] | null {
    let patterns = this.getConfig<any[]>('triggerPatterns', null)
    if (!patterns || patterns.length == 0) return null
    return patterns.map(s => (typeof s === 'string') ? new RegExp(s + '$') : s)
  }

  public get shortcut(): string {
    let shortcut = this.getConfig('shortcut', '')
    return shortcut ? shortcut : this.name.slice(0, 3)
  }

  public get enable(): boolean {
    if (this._disabled) return false
    return this.getConfig('enable', true)
  }

  public get filetypes(): string[] | null {
    return this.getConfig('filetypes', null)
  }

  public get disableSyntaxes(): string[] {
    return this.getConfig('disableSyntaxes', [])
  }

  public getConfig<T>(key: string, defaultValue?: T): T | null {
    let config = workspace.getConfiguration(`coc.source.${this.name}`)
    defaultValue = this.defaults.hasOwnProperty(key) ? this.defaults[key] : defaultValue
    return config.get(key, defaultValue)
  }

  public toggle(): void {
    this._disabled = !this._disabled
  }

  public get firstMatch(): boolean {
    return this.getConfig('firstMatch', true)
  }

  public get menu(): string {
    let { shortcut } = this
    return shortcut ? `[${shortcut}]` : ''
  }

  /**
   * fix start column for new valid characters
   *
   * @protected
   * @param {CompleteOption} opt
   * @param {string[]} valids - valid charscters
   * @returns {number}
   */
  protected fixStartcol(opt: CompleteOption, valids: string[]): number {
    let { col, input, line, bufnr } = opt
    let start = byteSlice(line, 0, col)
    let document = workspace.getDocument(bufnr)
    if (!document) return col
    let { chars } = document
    for (let i = start.length - 1; i >= 0; i--) {
      let c = start[i]
      if (!chars.isKeywordChar(c) && !valids.includes(c)) {
        break
      }
      input = `${c}${input}`
      col = col - 1
    }
    opt.col = col
    opt.input = input
    return col
  }

  public async shouldComplete(opt: CompleteOption): Promise<boolean> {
    let { disableSyntaxes } = this
    if (opt.synname && disableSyntaxes && disableSyntaxes.length) {
      let synname = (opt.synname || '').toLowerCase()
      if (disableSyntaxes.findIndex(s => synname.includes(s.toLowerCase())) !== -1) {
        return false
      }
    }
    let fn = this.defaults['shouldComplete']
    if (typeof fn === 'function') return await Promise.resolve(fn.call(this, opt))
    return true
  }

  public async refresh(): Promise<void> {
    let fn = this.defaults['refresh']
    if (typeof fn === 'function') await Promise.resolve(fn.call(this))
  }

  public async onCompleteDone(item: VimCompleteItem, opt: CompleteOption): Promise<void> {
    let fn = this.defaults['onCompleteDone']
    if (typeof fn === 'function') await Promise.resolve(fn.call(this, item, opt))
  }

  public async doComplete(opt: CompleteOption, token: CancellationToken): Promise<CompleteResult | null> {
    let fn = this.defaults['doComplete']
    if (typeof fn === 'function') return await Promise.resolve(fn.call(this, opt, token))
    return null
  }
}
