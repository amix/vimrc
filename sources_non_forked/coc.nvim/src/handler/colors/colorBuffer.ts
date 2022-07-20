'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import debounce from 'debounce'
import { CancellationTokenSource, Color, ColorInformation, Position, Range } from 'vscode-languageserver-protocol'
import languages from '../../languages'
import { SyncItem } from '../../model/bufferSync'
import { HighlightItem } from '../../types'
import { isDark, toHexString } from '../../util/color'
import { comparePosition, positionInRange } from '../../util/position'
import window from '../../window'
import workspace from '../../workspace'
const logger = require('../../util/logger')('colors-buffer')
const NAMESPACE = 'color'

export interface ColorRanges {
  color: Color
  ranges: Range[]
}

export interface ColorConfig {
  filetypes: string[]
  highlightPriority: number
}

export default class ColorBuffer implements SyncItem {
  private _colors: ColorInformation[] = []
  private tokenSource: CancellationTokenSource
  public highlight: Function & { clear(): void }
  // last highlight version
  constructor(
    private nvim: Neovim,
    private bufnr: number,
    private config: ColorConfig,
    private usedColors: Set<string>) {
    this.highlight = debounce(() => {
      this.doHighlight().logError()
    }, global.hasOwnProperty('__TEST__') ? 10 : 300)
    this.highlight()
  }

  public get enabled(): boolean {
    let { filetypes } = this.config
    let doc = workspace.getDocument(this.bufnr)
    if (!doc) return false
    if (filetypes.includes('*')) return true
    if (!languages.hasProvider('documentColor', doc.textDocument)) return false
    return filetypes.includes(doc.filetype)
  }

  public onChange(): void {
    this.cancel()
    this.highlight()
  }

  public get buffer(): Buffer {
    return this.nvim.createBuffer(this.bufnr)
  }

  public get colors(): ColorInformation[] {
    return this._colors
  }

  public hasColor(): boolean {
    return this._colors.length > 0
  }

  public async doHighlight(): Promise<void> {
    if (!this.enabled) return
    let { nvim } = this
    let doc = workspace.getDocument(this.bufnr)
    this.tokenSource = new CancellationTokenSource()
    let { token } = this.tokenSource
    let colors: ColorInformation[]
    colors = await languages.provideDocumentColors(doc.textDocument, token)
    if (token.isCancellationRequested) return
    colors = colors || []
    colors.sort((a, b) => comparePosition(a.range.start, b.range.start))
    this._colors = colors
    let items: HighlightItem[] = []
    colors.forEach(o => {
      let hlGroup = getHighlightGroup(o.color)
      doc.addHighlights(items, hlGroup, o.range, { combine: false })
    })
    let diff = await window.diffHighlights(this.bufnr, NAMESPACE, items)
    if (token.isCancellationRequested || !diff) return
    nvim.pauseNotification()
    this.defineColors(colors)
    nvim.resumeNotification(false, true)
    await window.applyDiffHighlights(this.bufnr, NAMESPACE, this.config.highlightPriority, diff, true)
  }

  private defineColors(colors: ColorInformation[]): void {
    for (let color of colors) {
      let hex = toHexString(color.color)
      if (!this.usedColors.has(hex)) {
        this.nvim.command(`hi BG${hex} guibg=#${hex} guifg=#${isDark(color.color) ? 'ffffff' : '000000'}`, true)
        this.usedColors.add(hex)
      }
    }
  }

  public hasColorAtPosition(position: Position): boolean {
    return this.colors.some(o => positionInRange(position, o.range) == 0)
  }

  public clearHighlight(): void {
    this.highlight.clear()
    this._colors = []
    this.buffer.clearNamespace('color')
  }

  public cancel(): void {
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource.dispose()
      this.tokenSource = null
    }
  }

  public dispose(): void {
    this._colors = []
    this.highlight.clear()
    this.cancel()
  }
}

function getHighlightGroup(color: Color): string {
  return `BG${toHexString(color)}`
}
