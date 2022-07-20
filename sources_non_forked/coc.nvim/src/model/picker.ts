'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import { CancellationToken, Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { HighlightItem, QuickPickItem } from '../types'
import { disposeAll } from '../util'
import { byteLength } from '../util/string'
import { DialogPreferences } from './dialog'
import Popup from './popup'
const logger = require('../util/logger')('model-dialog')
const isVim = process.env.VIM_NODE_RPC == '1'

interface PickerConfig {
  title: string
  items: QuickPickItem[]
}

/**
 * Pick multiple items from dialog
 */
export default class Picker {
  private bufnr: number
  private win: Popup | undefined
  private picked: Set<number> = new Set()
  private total: number
  private disposables: Disposable[] = []
  private keyMappings: Map<string, (character: string) => void> = new Map()
  private readonly _onDidClose = new Emitter<number[] | undefined>()
  public readonly onDidClose: Event<number[] | undefined> = this._onDidClose.event
  constructor(private nvim: Neovim, private config: PickerConfig, token?: CancellationToken) {
    for (let i = 0; i < config.items.length; i++) {
      let item = config.items[i]
      if (item.picked) this.picked.add(i)
    }
    this.total = config.items.length
    if (token) {
      token.onCancellationRequested(() => {
        this.win?.close()
      })
    }
    this.disposables.push(this._onDidClose)
    this.addKeymappings()
  }

  public get currIndex(): number {
    return this.win ? this.win.currIndex : 0
  }

  private attachEvents(): void {
    events.on('InputChar', this.onInputChar.bind(this), null, this.disposables)
    events.on('BufWinLeave', bufnr => {
      if (bufnr == this.bufnr) {
        this._onDidClose.fire(undefined)
        this.bufnr = undefined
        this.win = undefined
        this.dispose()
      }
    }, null, this.disposables)
    events.on('FloatBtnClick', (bufnr, idx) => {
      if (bufnr == this.bufnr) {
        if (idx == 0) {
          let selected = Array.from(this.picked)
          this._onDidClose.fire(selected.length ? selected : undefined)
        } else {
          this._onDidClose.fire(undefined)
        }
        this.dispose()
      }
    }, null, this.disposables)
  }

  private addKeymappings(): void {
    let { nvim } = this
    const toggleSelect = idx => {
      if (this.picked.has(idx)) {
        this.picked.delete(idx)
      } else {
        this.picked.add(idx)
      }
    }
    this.addKeys('<LeftRelease>', async () => {
      // not work on vim
      if (isVim || !this.win) return
      let [winid, lnum, col] = await nvim.eval('[v:mouse_winid,v:mouse_lnum,v:mouse_col]') as [number, number, number]
      // can't simulate vvar.
      if (global.hasOwnProperty('__TEST__')) {
        let res = await nvim.getVar('mouse_position')
        winid = res[0]
        lnum = res[1]
        col = res[2]
      }
      nvim.pauseNotification()
      if (winid == this.win.winid) {
        if (col <= 3) {
          toggleSelect(lnum - 1)
          this.changeLine(lnum - 1)
        } else {
          this.setCursor(lnum - 1)
        }
      }
      nvim.call('win_gotoid', [winid], true)
      nvim.call('cursor', [lnum, col], true)
      nvim.call('coc#float#nvim_float_click', [], true)
      nvim.command('redraw', true)
      await nvim.resumeNotification()
    })
    this.addKeys(['<esc>', '<C-c>'], () => {
      this._onDidClose.fire(undefined)
      this.dispose()
    })
    this.addKeys('<cr>', () => {
      if (this.picked.size == 0) {
        this._onDidClose.fire(undefined)
      } else {
        let selected = Array.from(this.picked)
        this._onDidClose.fire(selected)
      }
      this.dispose()
    })
    this.addKeys(['j', '<down>', '<tab>', '<C-n>'], () => {
      this.win.setCursor(this.currIndex + 1, true)
    })
    this.addKeys(['k', '<up>', '<s-tab>', '<C-p>'], () => {
      this.win.setCursor(this.currIndex - 1, true)
    })
    this.addKeys(['g'], () => {
      this.win.setCursor(0, true)
    })
    this.addKeys(['G'], () => {
      this.win.setCursor(this.total - 1, true)
    })
    this.addKeys(' ', async () => {
      let idx = this.currIndex
      toggleSelect(idx)
      nvim.pauseNotification()
      this.changeLine(idx)
      this.setCursor(this.currIndex + 1)
      nvim.command('redraw', true)
      await nvim.resumeNotification()
    })
    this.addKeys('<C-f>', async () => {
      await this.win?.scrollForward()
    })
    this.addKeys('<C-b>', async () => {
      await this.win?.scrollBackward()
    })
  }

  public async show(preferences: DialogPreferences = {}): Promise<number> {
    let { nvim } = this
    let { title, items } = this.config
    let opts: any = { close: 1, cursorline: 1 }
    if (preferences.maxHeight) opts.maxHeight = preferences.maxHeight
    if (preferences.maxWidth) opts.maxWidth = preferences.maxWidth
    if (title) opts.title = title
    if (preferences.floatHighlight) opts.highlight = preferences.floatHighlight
    if (preferences.floatBorderHighlight) opts.borderhighlight = [preferences.floatBorderHighlight]
    if (preferences.pickerButtons) {
      let shortcut = preferences.pickerButtonShortcut
      opts.buttons = ['Submit' + (shortcut ? ' <cr>' : ''), 'Cancel' + (shortcut ? ' <esc>' : '')]
    }
    if (preferences.rounded) opts.rounded = 1
    if (preferences.confirmKey && preferences.confirmKey != '<cr>') {
      this.addKeys(preferences.confirmKey, () => {
        this._onDidClose.fire(undefined)
        this.dispose()
      })
    }
    let lines = []
    let highlights: HighlightItem[] = []
    for (let i = 0; i < items.length; i++) {
      let item = items[i]
      let line = `[${item.picked ? 'x' : ' '}] ${item.label}`
      if (item.description) {
        let start = byteLength(line)
        line = line + ` ${item.description}`
        highlights.push({ hlGroup: 'Comment', lnum: i, colStart: start, colEnd: byteLength(line) })
      }
      lines.push(line)
    }
    if (highlights.length) opts.highlights = highlights
    let res = await nvim.call('coc#dialog#create_dialog', [lines, opts]) as [number, number]
    this.win = new Popup(nvim, res[0], res[1], lines.length)
    this.bufnr = res[1]
    nvim.call('coc#prompt#start_prompt', ['picker'], true)
    this.attachEvents()
    this.win.setCursor(0, true)
    return res[0]
  }

  public get buffer(): Buffer {
    return this.bufnr ? this.nvim.createBuffer(this.bufnr) : undefined
  }

  public dispose(): void {
    this.picked.clear()
    this.keyMappings.clear()
    disposeAll(this.disposables)
    this.nvim.call('coc#prompt#stop_prompt', ['picker'], true)
    this.win?.close()
    this.win = undefined
  }

  private async onInputChar(session: string, character: string): Promise<void> {
    if (session != 'picker' || !this.win) return
    let fn = this.keyMappings.get(character)
    if (fn) {
      await Promise.resolve(fn(character))
    } else {
      logger.warn(`Ignored key press: ${character}`)
    }
  }

  private changeLine(index: number): void {
    let { nvim } = this
    let item = this.config.items[index]
    if (!item) return
    let line = `[${this.picked.has(index) ? 'x' : ' '}] ${item.label}`
    let col = byteLength(line)
    if (item.description) line = line + ` ${item.description}`
    nvim.call('setbufline', [this.bufnr, index + 1, line], true)
    let buf = nvim.createBuffer(this.bufnr)
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    buf.addHighlight({ hlGroup: 'Comment', line: index, srcId: 1, colStart: col, colEnd: -1 })
  }

  private setCursor(index: number): void {
    if (!this.win) return
    this.win.setCursor(index)
  }

  private addKeys(keys: string | string[], fn: (character: string) => void): void {
    if (Array.isArray(keys)) {
      for (let key of keys) {
        this.keyMappings.set(key, fn)
      }
    } else {
      this.keyMappings.set(keys, fn)
    }
  }
}
