'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import events from '../events'
import { disposeAll } from '../util'
import { DialogButton } from './dialog'
const logger = require('../util/logger')('model-notification')

export interface NotificationPreferences {
  disabled: boolean
  maxWidth: number
  maxHeight: number
  highlight: string
  winblend: number
  broder: boolean
  timeout: number
  marginRight: number
  focusable: boolean
  minWidth?: number
  source?: string
}

export type NotificationKind = 'error' | 'info' | 'warning' | 'progress'

export interface NotificationConfig {
  kind?: NotificationKind

  content?: string
  /**
   * Optional title text.
   */
  title?: string
  /**
   * Buttons as bottom of dialog.
   */
  buttons?: DialogButton[]
  /**
   * index is -1 for window close without button click
   */
  callback?: (index: number) => void
}

export default class Notification {
  protected disposables: Disposable[] = []
  protected bufnr: number
  protected _winid: number
  protected _disposed = false
  constructor(protected nvim: Neovim, protected config: NotificationConfig, attachEvents = true) {
    if (attachEvents) {
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
  }

  protected get lines(): string[] {
    return this.config.content ? this.config.content.split(/\r?\n/) : []
  }

  public async show(preferences: Partial<NotificationPreferences>): Promise<void> {
    let { nvim } = this
    let { buttons, kind, title } = this.config
    let opts: any = Object.assign({}, preferences)
    opts.kind = kind ?? ''
    if (title) opts.title = title
    if (preferences.broder) {
      opts.borderhighlight = kind ? `CocNotification${kind[0].toUpperCase()}${kind.slice(1)}` : preferences.highlight
    }
    if (Array.isArray(buttons)) {
      let actions: string[] = buttons.filter(o => !o.disabled).map(o => o.text)
      if (actions.length) opts.actions = actions
    }
    let res = await nvim.call('coc#notify#create', [this.lines, opts]) as [number, number]
    if (!res) throw new Error(`Unable to create notification window`)
    this._winid = res[0]
    this.bufnr = res[1]
  }

  public get winid(): number | undefined {
    return this._winid
  }

  public dispose(): void {
    if (this._disposed) return
    this._disposed = true
    let { winid } = this
    if (winid) {
      this.nvim.call('coc#notify#close', [winid], true)
      this.nvim.redrawVim()
    }
    this.bufnr = undefined
    this._winid = undefined
    disposeAll(this.disposables)
  }
}
