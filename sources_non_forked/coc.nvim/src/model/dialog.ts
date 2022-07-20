'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { HighlightItem } from '../types'
import { disposeAll } from '../util'
const logger = require('../util/logger')('model-dialog')

export interface DialogButton {
  /**
   * Used by callback, should >= 0
   */
  index: number
  text: string
  /**
   * Not shown when true
   */
  disabled?: boolean
}

export interface DialogPreferences {
  rounded?: boolean
  maxWidth?: number
  maxHeight?: number
  floatHighlight?: string
  floatBorderHighlight?: string
  pickerButtons?: boolean
  pickerButtonShortcut?: boolean
  confirmKey?: string
  shortcutHighlight?: string
}

export interface DialogConfig {
  content: string
  /**
   * Optional title text.
   */
  title?: string
  /**
   * show close button, default to true when not specified.
   */
  close?: boolean
  /**
   * highlight group for dialog window, default to `"dialog.floatHighlight"` or 'CocFloating'
   */
  highlight?: string
  /**
   * highlight items of content.
   */
  highlights?: ReadonlyArray<HighlightItem>
  /**
   * highlight groups for border, default to `"dialog.borderhighlight"` or 'CocFloating'
   */
  borderhighlight?: string
  /**
   * Buttons as bottom of dialog.
   */
  buttons?: DialogButton[]
  /**
   * index is -1 for window close without button click
   */
  callback?: (index: number) => void
}

export default class Dialog {
  private disposables: Disposable[] = []
  private bufnr: number
  private readonly _onDidClose = new Emitter<void>()
  public readonly onDidClose: Event<void> = this._onDidClose.event
  constructor(private nvim: Neovim, private config: DialogConfig) {
    events.on('BufWinLeave', bufnr => {
      if (bufnr == this.bufnr) {
        this.dispose()
        if (config.callback) config.callback(-1)
      }
    }, null, this.disposables)
    events.on('FloatBtnClick', (bufnr, idx) => {
      if (bufnr == this.bufnr) {
        this.dispose()
        let btns = config?.buttons.filter(o => o.disabled != true)
        if (config.callback) config.callback(btns[idx].index)
      }
    }, null, this.disposables)
  }

  private get lines(): string[] {
    return [...this.config.content.split(/\r?\n/)]
  }

  public async show(preferences: DialogPreferences): Promise<void> {
    let { nvim } = this
    let { title, close, highlights, buttons } = this.config
    let borderhighlight = this.config.borderhighlight || preferences.floatBorderHighlight
    let highlight = this.config.highlight || preferences.floatHighlight
    let opts: any = { maxwidth: preferences.maxWidth || 80, }
    if (title) opts.title = title
    if (close || typeof close === 'undefined') opts.close = 1
    if (preferences.maxHeight) opts.maxHeight = preferences.maxHeight
    if (preferences.maxWidth) opts.maxWidth = preferences.maxWidth
    if (highlight) opts.highlight = highlight
    if (highlights) opts.highlights = highlights
    if (borderhighlight) opts.borderhighlight = [borderhighlight]
    if (buttons) opts.buttons = buttons.filter(o => !o.disabled).map(o => o.text)
    if (preferences.rounded) opts.rounded = 1
    if (Array.isArray(opts.buttons)) opts.getchar = 1
    let res = await nvim.call('coc#dialog#create_dialog', [this.lines, opts])
    if (!res) throw new Error('Unable to open dialog window.')
    this.bufnr = res[1]
    nvim.command('redraw', true)
  }

  public get winid(): Promise<number | null> {
    if (!this.bufnr) return Promise.resolve(null)
    return this.nvim.call('bufwinid', [this.bufnr])
  }

  public dispose(): void {
    this._onDidClose.fire()
    this.bufnr = undefined
    disposeAll(this.disposables)
    this.disposables = []
  }
}
