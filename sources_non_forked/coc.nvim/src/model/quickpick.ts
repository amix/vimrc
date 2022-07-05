'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import stringWidth from '@chemzqm/string-width'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { HighlightItem, QuickPickItem } from '../types'
import { disposeAll } from '../util'
import { hasMatch, positions } from '../util/fzy'
import { byteIndex, byteLength } from '../util/string'
import { DialogPreferences } from './dialog'
import InputBox from './input'
import Popup from './popup'
const logger = require('../util/logger')('model-quickpick')

export interface QuickPickConfig<T extends QuickPickItem> {
  title?: string
  items: readonly T[]
  value?: string
  canSelectMany?: boolean
  maxHeight?: number
}

/**
 * Pick single/multiple items from prompt list.
 */
export default class QuickPick<T extends QuickPickItem> {
  public title: string
  public loading: boolean
  public matchOnDescription: boolean
  public items: readonly T[]
  public activeItems: readonly T[]
  public selectedItems: T[]
  private bufnr: number
  private win: Popup
  private filteredItems: readonly T[]
  private disposables: Disposable[] = []
  private input: InputBox | undefined
  private _changed = false
  // emitted with selected items or undefined when cancelled.
  private readonly _onDidFinish = new Emitter<T[] | null>()
  private readonly _onDidChangeSelection = new Emitter<ReadonlyArray<T>>()
  private readonly _onDidChangeValue = new Emitter<string>()
  public readonly onDidFinish: Event<T[] | null> = this._onDidFinish.event
  public readonly onDidChangeSelection: Event<ReadonlyArray<T>> = this._onDidChangeSelection.event
  public readonly onDidChangeValue: Event<string> = this._onDidChangeValue.event
  constructor(private nvim: Neovim, private config: QuickPickConfig<T>) {
    let items = config.items ?? []
    Object.defineProperty(this, 'items', {
      set: (list: T[]) => {
        this._changed = true
        items = list
        this.filterItems('')
      },
      get: () => {
        return items
      }
    })
    Object.defineProperty(this, 'activeItems', {
      set: (list: T[]) => {
        this._changed = true
        this.filteredItems = list
        this.showFilteredItems()
      },
      get: () => {
        return this.filteredItems
      }
    })
    Object.defineProperty(this, 'title', {
      set: (newTitle: string) => {
        if (this.input) this.input.title = newTitle
      },
      get: () => {
        return this.input ? this.input.title : config.title
      }
    })
    Object.defineProperty(this, 'loading', {
      set: (loading: boolean) => {
        if (this.input) this.input.loading = loading
      },
      get: () => {
        return this.input ? this.input.loading : false
      }
    })
  }

  /**
   * Current input value
   */
  public get value(): string {
    return this.input ? this.input.value : this.config.value ?? ''
  }

  public get currIndex(): number {
    return this.win ? this.win.currIndex : 0
  }

  public get buffer(): Buffer {
    return this.bufnr ? this.nvim.createBuffer(this.bufnr) : undefined
  }

  private setCursor(index: number): void {
    this.win?.setCursor(index, true)
  }

  private attachEvents(inputBufnr: number): void {
    events.on('BufWinLeave', bufnr => {
      if (bufnr == this.bufnr) {
        this.dispose()
      }
    }, null, this.disposables)
    events.on('PromptKeyPress', async (bufnr, key) => {
      if (bufnr == inputBufnr) {
        if (key == 'C-f') {
          await this.win?.scrollForward()
        } else if (key == 'C-b') {
          await this.win?.scrollBackward()
        } else if (['C-j', 'C-n', 'down'].includes(key)) {
          this.setCursor(this.currIndex + 1)
        } else if (['C-k', 'C-p', 'up'].includes(key)) {
          this.setCursor(this.currIndex - 1)
        } else if (this.config.canSelectMany && key == 'C-@') {
          this.toggePicked(this.currIndex)
        }
      }
    }, null, this.disposables)
  }

  public async show(preferences: DialogPreferences = {}): Promise<void> {
    let { nvim, items } = this
    let { title, canSelectMany, value } = this.config
    let lines: string[] = []
    let highlights: HighlightItem[] = []
    let selectedItems: T[] = []
    for (let i = 0; i < items.length; i++) {
      let item = items[i]
      let line = canSelectMany ? `[${item.picked ? 'x' : ' '}] ${item.label}` : item.label
      if (item.picked) selectedItems.push(item)
      if (item.description) {
        let start = byteLength(line)
        line = line + ` ${item.description}`
        highlights.push({ hlGroup: 'Comment', lnum: i, colStart: start, colEnd: byteLength(line) })
      }
      lines.push(line)
    }
    let input = this.input = new InputBox(this.nvim, value ?? '')
    input.onDidChange(value => {
      this._onDidChangeValue.fire(value)
      // Updated by extension
      if (this._changed) {
        this._changed = false
        return
      }
      this.filterItems(value)
    }, this)
    input.onDidFinish(this.onFinish, this)
    let minWidth = Math.max(40, Math.min(80, lines.reduce<number>((p, c) => Math.max(p, stringWidth(c)), 0)))
    await input.show(title ?? '', {
      position: 'center',
      marginTop: 10,
      border: [1, 1, 0, 1],
      list: true,
      minWidth,
      maxWidth: preferences.maxWidth || 80,
      rounded: !!preferences.rounded,
      highlight: preferences.floatHighlight,
      borderhighlight: preferences.floatBorderHighlight
    })
    this.selectedItems = selectedItems
    let opts: any = { lines, rounded: !!preferences.rounded }
    opts.highlights = highlights
    if (preferences.floatHighlight) opts.highlight = preferences.floatHighlight
    if (preferences.floatBorderHighlight) opts.borderhighlight = preferences.floatBorderHighlight
    let maxHeight = this.config.maxHeight || preferences.maxHeight
    if (maxHeight) opts.maxHeight = maxHeight
    let res = await nvim.call('coc#dialog#create_list', [input.winid, input.dimension, opts])
    if (!res) throw new Error('Unable to open list window.')
    this.filteredItems = items
    // let height
    this.win = new Popup(nvim, res[0], res[1], lines.length)
    this.win.refreshScrollbar()
    this.bufnr = res[1]
    let idx = canSelectMany || selectedItems.length == 0 ? 0 : items.indexOf(selectedItems[0])
    this.setCursor(idx)
    this.attachEvents(input.bufnr)
  }

  /**
   * Filter items with input
   */
  private filterItems(input: string): void {
    let { items, win, selectedItems } = this
    if (!win) return
    let { canSelectMany } = this.config
    let lines: string[] = []
    let highlights: HighlightItem[] = []
    let idx = 0
    let filteredItems: T[] = []
    for (let item of items) {
      let filterText = this.toFilterText(item)
      if (input.length > 0 && !hasMatch(input, filterText)) continue
      let picked = selectedItems.includes(item)
      let line = canSelectMany ? `[${picked ? 'x' : ' '}] ${item.label}` : item.label
      if (item.description) {
        let start = byteLength(line)
        line = line + ` ${item.description}`
        highlights.push({ hlGroup: 'Comment', lnum: idx, colStart: start, colEnd: byteLength(line) })
      }
      let arr = positions(input, filterText)
      arr.forEach(n => {
        let colStart = byteIndex(filterText, n)
        highlights.push({
          hlGroup: 'CocSearch',
          colStart,
          colEnd: colStart + 1,
          lnum: idx
        })
      })
      filteredItems.push(item)
      lines.push(line)
      idx += 1
    }
    this.filteredItems = filteredItems
    this.win.linecount = lines.length
    this.nvim.call('coc#dialog#update_list', [this.win.winid, this.win.bufnr, lines, highlights], true)
    this.setCursor(0)
  }

  private showFilteredItems(): void {
    let { win, input, filteredItems } = this
    if (!win) return
    let { canSelectMany } = this.config
    let lines: string[] = []
    let highlights: HighlightItem[] = []
    let idx = 0
    let selectedItems: T[] = []
    for (let item of filteredItems) {
      let filterText = this.toFilterText(item)
      let line = canSelectMany ? `[${item.picked ? 'x' : ' '}] ${item.label}` : item.label
      if (item.picked) selectedItems.push(item)
      if (item.description) {
        let start = byteLength(line)
        line = line + ` ${item.description}`
        highlights.push({ hlGroup: 'Comment', lnum: idx, colStart: start, colEnd: byteLength(line) })
      }
      let arr = positions(input.value, filterText)
      arr.forEach(n => {
        let colStart = byteIndex(filterText, n)
        highlights.push({
          hlGroup: 'CocSearch',
          colStart,
          colEnd: colStart + 1,
          lnum: idx
        })
      })
      lines.push(line)
      idx += 1
    }
    this.selectedItems = selectedItems
    this.win.linecount = lines.length
    this.nvim.call('coc#dialog#update_list', [this.win.winid, this.win.bufnr, lines, highlights], true)
    this.setCursor(canSelectMany || selectedItems.length == 0 ? 0 : filteredItems.indexOf(selectedItems[0]))
  }

  private onFinish(input: string | undefined): void {
    if (input == null) {
      this._onDidChangeSelection.fire([])
      this._onDidFinish.fire(null)
      return
    }
    let selected = this.getSelectedItems()
    if (!this.config.canSelectMany) {
      this._onDidChangeSelection.fire(selected)
    }
    this._onDidFinish.fire(selected)
  }

  private getSelectedItems(): T[] {
    let { win } = this
    let { canSelectMany } = this.config
    if (canSelectMany) return this.selectedItems
    let item = this.filteredItems[win.currIndex]
    return item == null ? [] : [item]
  }

  private toggePicked(index: number): void {
    let { nvim, filteredItems, selectedItems } = this
    let item = filteredItems[index]
    if (!item) return
    let idx = selectedItems.indexOf(item)
    if (idx != -1) {
      selectedItems.splice(idx, 1)
    } else {
      selectedItems.push(item)
    }
    let text = idx == -1 ? 'x' : ' '
    nvim.pauseNotification()
    this.win.execute(`normal! ^1lr${text}`)
    this.win.setCursor(this.win.currIndex + 1)
    nvim.resumeNotification(true, true)
    this._onDidChangeSelection.fire(selectedItems)
  }

  private toFilterText(item: T): string {
    let { label, description } = item
    let { canSelectMany } = this.config
    let line = `${canSelectMany ? '    ' : ''}${label.replace(/\r?\n/, '')}`
    return this.matchOnDescription ? line + ' ' + (description ?? '') : line
  }

  public dispose(): void {
    this.bufnr = undefined
    this.input?.dispose()
    this.win?.close()
    this._onDidFinish.dispose()
    this._onDidChangeSelection.dispose()
    disposeAll(this.disposables)
  }
}
