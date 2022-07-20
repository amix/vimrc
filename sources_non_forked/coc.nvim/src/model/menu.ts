'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import { CancellationToken, Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { HighlightItem } from '../types'
import { disposeAll } from '../util'
import { byteLength, isAlphabet } from '../util/string'
import { DialogPreferences } from './dialog'
import Popup from './popup'
const logger = require('../util/logger')('model-menu')

export interface MenuItem {
  text: string
  disabled?: boolean | { reason: string }
}

export interface MenuConfig {
  items: string[] | MenuItem[]
  title?: string
  content?: string
  shortcuts?: boolean
  position?: 'cursor' | 'center'
  borderhighlight?: string
}

export function isMenuItem(item: any): item is MenuItem {
  if (!item) return false
  return typeof item.text === 'string'
}

/**
 * Select single item from menu at cursor position.
 */
export default class Menu {
  private bufnr: number
  private win: Popup
  private currIndex = 0
  private contentHeight = 0
  private total: number
  private disposables: Disposable[] = []
  private keyMappings: Map<string, (character: string) => void> = new Map()
  private shortcutIndexes: Set<number> = new Set()
  private _disposed = false
  private readonly _onDidClose = new Emitter<number>()
  public readonly onDidClose: Event<number> = this._onDidClose.event
  constructor(private nvim: Neovim, private config: MenuConfig, token?: CancellationToken) {
    this.total = config.items.length
    if (token) {
      token.onCancellationRequested(() => {
        if (this.win) {
          this.win?.close()
        } else {
          this._onDidClose.fire(-1)
          this.dispose()
        }
      })
    }
    this.disposables.push(this._onDidClose)
    this.addKeymappings()
  }

  private attachEvents(): void {
    events.on('InputChar', this.onInputChar.bind(this), null, this.disposables)
    events.on('BufWinLeave', bufnr => {
      if (bufnr == this.bufnr) {
        this._onDidClose.fire(-1)
        this.dispose()
      }
    }, null, this.disposables)
  }

  private addKeymappings(): void {
    let { nvim } = this
    this.addKeys(['<esc>', '<C-c>'], () => {
      this._onDidClose.fire(-1)
      this.dispose()
    })
    this.addKeys(['\r', '<cr>'], () => {
      this.selectCurrent()
    })
    let setCursorIndex = idx => {
      if (!this.win) return
      nvim.pauseNotification()
      this.setCursor(idx + this.contentHeight)
      this.win?.refreshScrollbar()
      nvim.command('redraw', true)
      nvim.resumeNotification(false, true)
    }
    this.addKeys('<C-f>', async () => {
      await this.win?.scrollForward()
    })
    this.addKeys('<C-b>', async () => {
      await this.win?.scrollBackward()
    })
    this.addKeys(['j', '<down>', '<tab>', '<C-n>'], () => {
      // next
      let idx = this.currIndex == this.total - 1 ? 0 : this.currIndex + 1
      setCursorIndex(idx)
    })
    this.addKeys(['k', '<up>', '<s-tab>', '<C-p>'], () => {
      // previous
      let idx = this.currIndex == 0 ? this.total - 1 : this.currIndex - 1
      setCursorIndex(idx)
    })
    this.addKeys(['g'], () => {
      setCursorIndex(0)
    })
    this.addKeys(['G'], () => {
      setCursorIndex(this.total - 1)
    })
    let timer: NodeJS.Timeout
    let firstNumber: number
    const choose = (n: number) => {
      let disabled = this.isDisabled(n)
      if (disabled) return
      this._onDidClose.fire(n)
      this.dispose()
    }
    this.addKeys(['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'], character => {
      if (timer) clearTimeout(timer)
      let n = parseInt(character, 10)
      if (isNaN(n) || n > this.total) return
      if (firstNumber == null && n == 0) return
      if (firstNumber) {
        let count = firstNumber * 10 + n
        firstNumber = undefined
        choose(count - 1)
        return
      }
      if (this.total < 10 || n * 10 > this.total) {
        choose(n - 1)
        return
      }
      timer = setTimeout(async () => {
        choose(n - 1)
      }, 200)
      firstNumber = n
    })
    if (this.config.shortcuts) {
      this.addShortcuts(choose)
    }
  }

  private addShortcuts(choose: (idx: number) => void): void {
    let { items } = this.config
    let texts: string[] = items.map(o => {
      return isMenuItem(o) ? o.text : o
    })
    texts.forEach((text, idx) => {
      if (text.length) {
        let s = text[0]
        if (isAlphabet(s.charCodeAt(0)) && !this.keyMappings.has(s)) {
          this.shortcutIndexes.add(idx)
          this.addKeys(s, () => {
            choose(idx)
          })
        }
      }
    })
  }

  private isDisabled(idx: number): boolean {
    let { items } = this.config
    let item = items[idx]
    if (isMenuItem(item) && item.disabled) {
      return true
    }
    return false
  }

  public async show(preferences: DialogPreferences = {}): Promise<void> {
    let { nvim, shortcutIndexes } = this
    let { title, items, borderhighlight, position, content } = this.config
    let opts: any = {}
    if (title) opts.title = title
    if (position === 'center') opts.relative = 'editor'
    if (preferences.maxHeight) opts.maxHeight = preferences.maxHeight
    if (preferences.maxWidth) opts.maxWidth = preferences.maxWidth
    if (preferences.floatHighlight) opts.highlight = preferences.floatHighlight
    if (borderhighlight) {
      opts.borderhighlight = [borderhighlight]
    } else if (preferences.floatBorderHighlight) {
      opts.borderhighlight = [preferences.floatBorderHighlight]
    }
    if (preferences.rounded) opts.rounded = 1
    if (typeof content === 'string') opts.content = content
    let highlights: HighlightItem[] = []
    let lines = items.map((v, i) => {
      let text: string = isMenuItem(v) ? v.text : v
      let pre = i < 99 ? `${i + 1}. ` : ''
      // if (i < 99) return `${i + 1}. ${text.trim()}`
      if (shortcutIndexes.has(i)) {
        highlights.push({
          lnum: i,
          hlGroup: preferences.shortcutHighlight || 'MoreMsg',
          colStart: byteLength(pre),
          colEnd: byteLength(pre) + 1
        })
      }
      return pre + text.trim()
    })
    lines.forEach((line, i) => {
      let item = items[i]
      if (isMenuItem(item) && item.disabled) {
        highlights.push({
          hlGroup: 'CocDisabled',
          lnum: i,
          colStart: 0,
          colEnd: byteLength(line)
        })
      }
    })
    if (highlights.length) opts.highlights = highlights
    if (preferences.confirmKey && preferences.confirmKey != '<cr>') {
      this.addKeys(preferences.confirmKey, () => {
        this.selectCurrent()
      })
    }
    let res = await nvim.call('coc#dialog#create_menu', [lines, opts]) as [number, number, number]
    if (!res) throw new Error('Unable to create menu window')
    nvim.command('redraw', true)
    if (this._disposed) return
    this.win = new Popup(nvim, res[0], res[1], lines.length + res[2], res[2])
    this.bufnr = res[1]
    this.contentHeight = res[2]
    this.attachEvents()
    nvim.call('coc#prompt#start_prompt', ['menu'], true)
  }

  private selectCurrent(): void {
    if (this.isDisabled(this.currIndex)) {
      let item = this.config.items[this.currIndex] as MenuItem
      if (item.disabled['reason']) {
        this.nvim.outWriteLine(`Item disabled: ${item.disabled['reason']}`)
      }
      return
    }
    this._onDidClose.fire(this.currIndex)
    this.dispose()
  }

  public get buffer(): Buffer {
    return this.bufnr ? this.nvim.createBuffer(this.bufnr) : undefined
  }

  public dispose(): void {
    this._disposed = true
    disposeAll(this.disposables)
    this.shortcutIndexes.clear()
    this.keyMappings.clear()
    this.nvim.call('coc#prompt#stop_prompt', ['menu'], true)
    this.win?.close()
    this.bufnr = undefined
    this.win = undefined
  }

  private async onInputChar(session: string, character: string): Promise<void> {
    if (session != 'menu' || !this.win) return
    let fn = this.keyMappings.get(character)
    if (fn) {
      await Promise.resolve(fn(character))
    } else {
      logger.warn(`Ignored key press: ${character}`)
    }
  }

  private setCursor(index: number): void {
    if (!this.win) return
    this.currIndex = index - this.contentHeight
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
