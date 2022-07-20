'use strict'
import { Buffer, Neovim, Window } from '@chemzqm/neovim'
import debounce from 'debounce'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { HighlightItem, ListItem, ListItemWithHighlights, ListOptions } from '../types'
import { disposeAll } from '../util'
import { Mutex } from '../util/mutex'
import workspace from '../workspace'
import ListConfiguration from './configuration'
const logger = require('../util/logger')('list-ui')

export type MouseEvent = 'mouseDown' | 'mouseDrag' | 'mouseUp' | 'doubleClick'

export interface MousePosition {
  winid: number
  lnum: number
  col: number
  current: boolean
}

export interface HighlightGroup {
  hlGroup: string
  priority: number
  pos: [number, number, number]
}

export default class ListUI {
  private window: Window
  private height: number
  public tabnr: number
  private newTab = false
  private reversed = false
  private buffer: Buffer
  private currIndex = 0
  private items: ListItemWithHighlights[] = []
  private disposables: Disposable[] = []
  private signOffset: number
  private selected: Set<number> = new Set()
  private mouseDown: MousePosition
  private mutex: Mutex = new Mutex()
  private _onDidChangeLine = new Emitter<number>()
  private _onDidOpen = new Emitter<number>()
  private _onDidClose = new Emitter<number>()
  private _onDidLineChange = new Emitter<void>()
  private _onDoubleClick = new Emitter<void>()
  public readonly onDidChangeLine: Event<number> = this._onDidChangeLine.event
  public readonly onDidLineChange: Event<void> = this._onDidLineChange.event
  public readonly onDidOpen: Event<number> = this._onDidOpen.event
  public readonly onDidClose: Event<number> = this._onDidClose.event
  public readonly onDidDoubleClick: Event<void> = this._onDoubleClick.event

  constructor(
    private nvim: Neovim,
    private name: string,
    private listOptions: ListOptions,
    private config: ListConfiguration
  ) {
    this.signOffset = config.get<number>('signOffset')
    this.newTab = listOptions.position == 'tab'
    this.reversed = listOptions.reverse === true
    events.on('BufWinLeave', async bufnr => {
      if (bufnr != this.bufnr || this.window == null) return
      this.window = null
      this._onDidClose.fire(bufnr)
    }, null, this.disposables)
    events.on('CursorMoved', async (bufnr, cursor) => {
      if (bufnr != this.bufnr) return
      let idx = this.lnumToIndex(cursor[0])
      this.onLineChange(idx)
    }, null, this.disposables)
    let debounced = debounce(async bufnr => {
      if (bufnr != this.bufnr) return
      let [winid, start, end] = await nvim.eval('[win_getid(),line("w0"),line("w$")]') as number[]
      if (end < 300 || winid != this.winid) return
      let h = end - start + 1
      let s = this.lnumToIndex(start)
      let e = this.lnumToIndex(start + h * 2)
      nvim.pauseNotification()
      this.doHighlight(s, e)
      nvim.command('redraw', true)
      nvim.resumeNotification(false, true)
    }, global.hasOwnProperty('__TEST__') ? 20 : 100)
    this.disposables.push({
      dispose: () => {
        debounced.clear()
      }
    })
    events.on('CursorMoved', debounced, null, this.disposables)
  }

  public lnumToIndex(lnum: number): number {
    let { reversed, length } = this
    if (!reversed) return lnum - 1
    return Math.max(0, length - lnum)
  }

  public indexToLnum(index: number): number {
    let { reversed, length } = this
    if (!reversed) return Math.min(index + 1, length)
    return Math.max(Math.min(length, length - index), 1)
  }

  public get bufnr(): number | undefined {
    return this.buffer?.id
  }

  public get winid(): number | undefined {
    return this.window?.id
  }
  private get limitLines(): number {
    return this.config.get<number>('limitLines', Infinity)
  }

  private onLineChange(index: number): void {
    if (this.currIndex == index) return
    this.currIndex = index
    this._onDidChangeLine.fire(index)
  }

  public set index(n: number) {
    if (n < 0 || n >= this.items.length) return
    let { nvim } = this
    let lnum = this.indexToLnum(n)
    nvim.pauseNotification()
    this.setCursor(lnum)
    nvim.command('redraw', true)
    nvim.resumeNotification(false, true)
  }

  public get index(): number {
    return this.currIndex
  }

  public getItem(index: number): ListItem | undefined {
    return this.items[index]
  }

  public get item(): Promise<ListItem | null> {
    let { window } = this
    if (!window) return Promise.resolve(null)
    return window.cursor.then(cursor => {
      this.currIndex = this.lnumToIndex(cursor[0])
      return this.items[this.currIndex]
    })
  }

  public async echoMessage(item: ListItem): Promise<void> {
    let { items } = this
    let idx = items.indexOf(item)
    let msg = `[${idx + 1}/${items.length}] ${item.label || ''}`
    this.nvim.callTimer('coc#ui#echo_lines', [[msg]], true)
  }

  public updateItem(item: ListItemWithHighlights, index: number, labelChanged: boolean): void {
    if (!this.buffer || index >= this.length) return
    let prev = this.items[index]
    Object.assign(prev, item, { resolved: true })
    if (!labelChanged) return
    let { nvim } = this
    let lnum = this.indexToLnum(index)
    nvim.pauseNotification()
    this.buffer.setOption('modifiable', true, true)
    nvim.call('setbufline', [this.bufnr, lnum, prev.label], true)
    this.doHighlight(index, index + 1)
    this.buffer.setOption('modifiable', false, true)
    nvim.resumeNotification(true, true)
  }

  public async getItems(): Promise<ListItem[]> {
    if (this.length == 0 || !this.window) return []
    let mode = await this.nvim.call('mode')
    if (mode == 'v' || mode == 'V') {
      let [start, end] = await this.getSelectedRange()
      let res: ListItem[] = []
      for (let i = start; i <= end; i++) {
        let idx = this.lnumToIndex(i)
        let item = this.items[idx]
        if (item) res.push(item)
      }
      return res
    }
    let { selectedItems } = this
    if (selectedItems.length) return selectedItems
    let item = await this.item
    return item == null ? [] : [item]
  }

  public async onMouse(event: MouseEvent): Promise<void> {
    let { nvim, window } = this
    if (!window) return
    let [winid, lnum, col] = await nvim.eval(`[v:mouse_winid,v:mouse_lnum,v:mouse_col]`) as [number, number, number]
    if (event == 'mouseDown') {
      this.mouseDown = { winid, lnum, col, current: winid == window.id }
      return
    }
    let current = winid == window.id
    if (current && event == 'doubleClick') {
      this.setCursor(lnum)
      this._onDoubleClick.fire()
    }
    if (current && event == 'mouseDrag') {
      if (!this.mouseDown) return
      await this.selectLines(this.mouseDown.lnum, lnum)
    } else if (current && event == 'mouseUp') {
      if (!this.mouseDown) return
      if (this.mouseDown.lnum == lnum) {
        this.setCursor(lnum)
        nvim.command('redraw', true)
      } else {
        await this.selectLines(this.mouseDown.lnum, lnum)
      }
    } else if (!current && event == 'mouseUp') {
      nvim.pauseNotification()
      nvim.call('win_gotoid', winid, true)
      nvim.call('cursor', [lnum, col], true)
      nvim.command('redraw', true)
      nvim.resumeNotification(false, true)
    }
  }

  public async resume(): Promise<void> {
    let { items, selected, nvim } = this
    await this.drawItems(items, this.height, true)
    if (!selected.size || !this.buffer) return
    nvim.pauseNotification()
    for (let lnum of selected) {
      this.buffer?.placeSign({ lnum, id: this.signOffset + lnum, name: 'CocSelected', group: 'coc-list' })
    }
    nvim.command('redraw', true)
    nvim.resumeNotification(false, true)
  }

  public async toggleSelection(): Promise<void> {
    let { nvim, reversed } = this
    await nvim.call('win_gotoid', [this.winid])
    let lnum = await nvim.call('line', '.')
    let mode = await nvim.call('mode')
    if (mode == 'v' || mode == 'V') {
      let [start, end] = await this.getSelectedRange()
      let reverse = start > end
      if (reverse) [start, end] = [end, start]
      for (let i = start; i <= end; i++) {
        this.toggleLine(i)
      }
      this.setCursor(end)
      nvim.command('redraw', true)
      await nvim.resumeNotification()
      return
    }
    nvim.pauseNotification()
    this.toggleLine(lnum)
    this.setCursor(reversed ? lnum - 1 : lnum + 1)
    nvim.command('redraw', true)
    await nvim.resumeNotification()
  }

  private toggleLine(lnum: number): void {
    let { selected, buffer, signOffset } = this
    let exists = selected.has(lnum)
    if (!exists) {
      selected.add(lnum)
      buffer.placeSign({ lnum, id: signOffset + lnum, name: 'CocSelected', group: 'coc-list' })
    } else {
      selected.delete(lnum)
      buffer.unplaceSign({ id: signOffset + lnum, group: 'coc-list' })
    }
  }

  public async selectLines(start: number, end: number): Promise<void> {
    let { nvim, signOffset, buffer, length } = this
    this.clearSelection()
    let { selected } = this
    nvim.pauseNotification()
    let reverse = start > end
    if (reverse) [start, end] = [end, start]
    for (let i = start; i <= end; i++) {
      if (i > length) break
      selected.add(i)
      buffer.placeSign({ lnum: i, id: signOffset + i, name: 'CocSelected', group: 'coc-list' })
    }
    this.setCursor(end)
    nvim.command('redraw', true)
    await nvim.resumeNotification()
  }

  public async selectAll(): Promise<void> {
    let { length } = this
    if (length > 0) await this.selectLines(1, length)
  }

  public clearSelection(): void {
    let { selected, buffer } = this
    if (selected.size > 0) {
      buffer?.unplaceSign({ group: 'coc-list' })
      this.selected.clear()
    }
  }

  public get ready(): Promise<void> {
    if (this.window) return Promise.resolve()
    return new Promise<void>(resolve => {
      let disposable = this.onDidLineChange(() => {
        disposable.dispose()
        resolve()
      })
    })
  }

  public async drawItems(items: ListItem[], height: number, reload = false): Promise<void> {
    const { nvim, name, listOptions } = this
    await this.mutex.use(async () => {
      this.items = items.length > this.limitLines ? items.slice(0, this.limitLines) : items
      if (!this.window) {
        let { position, numberSelect } = listOptions
        let [bufnr, winid, tabnr] = await nvim.call('coc#list#create', [position, height, name, numberSelect])
        this.tabnr = tabnr
        this.height = height
        this.buffer = nvim.createBuffer(bufnr)
        let win = this.window = nvim.createWindow(winid)
        let statusSegments = this.config.get<string[]>('statusLineSegments')
        if (statusSegments) win.setOption('statusline', statusSegments.join(" "), true)
        this._onDidOpen.fire(this.bufnr)
      }
      const lines: string[] = []
      let selectIndex = 0
      this.items.forEach((item, idx) => {
        lines.push(item.label)
        if (!reload && selectIndex == 0 && item.preselect) selectIndex = idx
      })
      let newIndex = reload ? this.currIndex : selectIndex
      this.setLines(lines, 0, newIndex)
      this._onDidLineChange.fire()
    })
  }

  public async appendItems(items: ListItem[]): Promise<void> {
    if (!this.window || items.length === 0) return
    await this.mutex.use(async () => {
      let curr = this.items.length
      let remain = this.limitLines - curr
      if (remain > 0) {
        let append = remain < items.length ? items.slice(0, remain) : items
        this.items = this.items.concat(append)
        this.setLines(append.map(item => item.label), append.length, this.currIndex)
      }
    })
  }

  private setLines(lines: string[], append: number, index: number): void {
    let { nvim, buffer, window, reversed, newTab } = this
    if (!buffer || !window) return
    nvim.pauseNotification()
    if (!append) {
      nvim.call('coc#compat#clear_matches', [window.id], true)
      if (!lines.length) {
        lines = ['No results, press ? on normal mode to get help.']
        nvim.call('coc#compat#matchaddpos', ['Comment', [[1]], 99, window.id], true)
      }
    }
    buffer.setOption('modifiable', true, true)
    if (reversed) {
      let replacement = lines.reverse()
      if (append) {
        nvim.call('coc#compat#prepend_lines', [buffer.id, replacement], true)
      } else {
        buffer.setLines(replacement, { start: 0, end: -1, strictIndexing: false }, true)
      }
    } else {
      buffer.setLines(lines, { start: append ? -1 : 0, end: -1, strictIndexing: false }, true)
    }
    buffer.setOption('modifiable', false, true)
    if (reversed && !newTab) {
      let maxHeight = this.config.get<number>('height', 10)
      nvim.call('coc#window#set_height', [window.id, Math.max(Math.min(maxHeight, this.length), 1)], true)
    }
    if (index > this.items.length - 1) index = 0
    if (index == 0) {
      if (append == 0) {
        this.doHighlight(0, 299)
      } else {
        let s = this.length - append - 1
        if (s < 300) this.doHighlight(s, Math.min(299, this.length - 1))
      }
    } else {
      let height = newTab ? workspace.env.lines : this.height
      this.doHighlight(Math.max(0, index - height), Math.min(index + height + 1, this.length - 1))
    }
    if (!append) {
      this.currIndex = index
      let lnum = this.indexToLnum(index)
      window.setCursor([lnum, 0], true)
      nvim.call('coc#list#select', [buffer.id, lnum], true)
    }
    if (reversed) nvim.command('normal! zb', true)
    nvim.command('redraws', true)
    nvim.resumeNotification(true, true)
  }

  public restoreWindow(): void {
    if (this.newTab) return
    let { winid, height } = this
    if (winid && height) {
      this.nvim.call('coc#window#set_height', [winid, height], true)
    }
  }

  public get length(): number {
    return this.items.length
  }

  public get selectedItems(): ListItem[] {
    let { selected, items } = this
    let res: ListItem[] = []
    for (let i of selected) {
      let idx = this.lnumToIndex(i)
      if (items[i - 1]) res.push(items[idx])
    }
    return res
  }

  private doHighlight(start: number, end: number): void {
    let { items, reversed, length, buffer } = this
    if (!buffer) return
    const highlightItems: HighlightItem[] = []
    const iterate = (i: number): void => {
      let lnum = this.indexToLnum(i) - 1
      let { ansiHighlights, highlights } = items[i]
      if (ansiHighlights) {
        for (let hi of ansiHighlights) {
          let { span, hlGroup } = hi
          highlightItems.push({ hlGroup, lnum, colStart: span[0], colEnd: span[1] })
        }
      }
      if (highlights && Array.isArray(highlights.spans)) {
        let { spans, hlGroup } = highlights
        for (let span of spans) {
          hlGroup = hlGroup ?? 'CocListSearch'
          highlightItems.push({ hlGroup, lnum, colStart: span[0], colEnd: span[1] })
        }
      }
    }
    if (reversed) {
      for (let i = Math.min(end, length - 1); i >= start; i--) {
        iterate(i)
      }
    } else {
      for (let i = start; i <= Math.min(end, length - 1); i++) {
        iterate(i)
      }
    }
    start = this.indexToLnum(start) - 1
    end = this.indexToLnum(end) - 1
    if (start > end) {
      [start, end] = [end, start]
    }
    if (highlightItems.length == 0) return
    buffer.updateHighlights('list', highlightItems, { start, end: end + 1, priority: 99 })
  }

  public setCursor(lnum: number, col = 0): void {
    let { items } = this
    let max = items.length == 0 ? 1 : items.length
    if (lnum > max) return
    // change index since CursorMoved event not fired (seems bug of neovim)!
    let idx = this.lnumToIndex(lnum)
    this.onLineChange(idx)
    this.window?.setCursor([lnum, col], true)
    this.nvim.call('coc#list#select', [this.bufnr, lnum], true)
  }

  public moveUp(): void {
    let { index, reversed } = this
    this.index = reversed ? index + 1 : index - 1
  }

  public moveDown(): void {
    let { index, reversed } = this
    this.index = reversed ? index - 1 : index + 1
  }

  private async getSelectedRange(): Promise<[number, number]> {
    let { nvim } = this
    await nvim.call('coc#prompt#stop_prompt', ['list'])
    await nvim.eval('feedkeys("\\<esc>", "in")')
    let [, start] = await nvim.call('getpos', "'<")
    let [, end] = await nvim.call('getpos', "'>")
    this.nvim.call('coc#prompt#start_prompt', ['list'], true)
    return [start, end]
  }

  public reset(): void {
    if (this.window) {
      this.window = null
      this.buffer = null
      this.tabnr = undefined
    }
  }

  public dispose(): void {
    disposeAll(this.disposables)
    this.nvim.call('coc#window#close', [this.winid || -1], true)
    this.window = null
    this.buffer = null
    this.items = []
    this._onDidChangeLine.dispose()
    this._onDidOpen.dispose()
    this._onDidClose.dispose()
    this._onDidLineChange.dispose()
    this._onDoubleClick.dispose()
  }
}
