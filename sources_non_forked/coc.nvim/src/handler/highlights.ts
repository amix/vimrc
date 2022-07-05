'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable, DocumentHighlight, DocumentHighlightKind, Position, Range } from 'vscode-languageserver-protocol'
import events from '../events'
import languages from '../languages'
import Document from '../model/document'
import { ConfigurationChangeEvent, HandlerDelegate } from '../types'
import { disposeAll } from '../util'
import workspace from '../workspace'
const logger = require('../util/logger')('documentHighlight')

interface HighlightConfig {
  priority: number
  timeout: number
}

/**
 * Highlight same symbols on current window.
 * Highlights are added to window by matchaddpos.
 */
export default class Highlights {
  private config: HighlightConfig
  private disposables: Disposable[] = []
  private tokenSource: CancellationTokenSource
  private highlights: Map<number, DocumentHighlight[]> = new Map()
  private timer: NodeJS.Timer
  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
    events.on(['CursorMoved', 'CursorMovedI'], () => {
      this.cancel()
      this.clearHighlights()
    }, null, this.disposables)
    this.getConfiguration()
    workspace.onDidChangeConfiguration(this.getConfiguration, this, this.disposables)
  }

  private getConfiguration(e?: ConfigurationChangeEvent): void {
    let config = workspace.getConfiguration('documentHighlight')
    if (!e || e.affectsConfiguration('documentHighlight')) {
      this.config = Object.assign(this.config || {}, {
        priority: config.get<number>('priority', -1),
        timeout: config.get<number>('timeout', 300)
      })
    }
  }

  public isEnabled(bufnr: number, cursors: number): boolean {
    let doc = workspace.getDocument(bufnr)
    if (!doc || !doc.attached || cursors) return false
    if (!languages.hasProvider('documentHighlight', doc.textDocument)) return false
    return true
  }

  public clearHighlights(): void {
    if (this.highlights.size == 0) return
    for (let winid of this.highlights.keys()) {
      let win = this.nvim.createWindow(winid)
      win.clearMatchGroup('^CocHighlight')
    }
    this.highlights.clear()
  }

  public async highlight(): Promise<void> {
    let { nvim } = this
    this.cancel()
    let [bufnr, winid, pos, cursors] = await nvim.eval(`[bufnr("%"),win_getid(),coc#cursor#position(),get(b:,'coc_cursors_activated',0)]`) as [number, number, [number, number], number]
    if (!this.isEnabled(bufnr, cursors)) return
    let doc = workspace.getDocument(bufnr)
    let highlights = await this.getHighlights(doc, Position.create(pos[0], pos[1]))
    if (!highlights) return
    let groups: { [index: string]: Range[] } = {}
    for (let hl of highlights) {
      if (!hl.range) continue
      let hlGroup = hl.kind == DocumentHighlightKind.Text
        ? 'CocHighlightText'
        : hl.kind == DocumentHighlightKind.Read ? 'CocHighlightRead' : 'CocHighlightWrite'
      groups[hlGroup] = groups[hlGroup] || []
      groups[hlGroup].push(hl.range)
    }
    let win = nvim.createWindow(winid)
    nvim.pauseNotification()
    win.clearMatchGroup('^CocHighlight')
    for (let hlGroup of Object.keys(groups)) {
      win.highlightRanges(hlGroup, groups[hlGroup], this.config.priority, true)
    }
    nvim.resumeNotification(true, true)
    this.highlights.set(winid, highlights)
  }

  public async getSymbolsRanges(): Promise<Range[]> {
    let { doc, position } = await this.handler.getCurrentState()
    this.handler.checkProvier('documentHighlight', doc.textDocument)
    let highlights = await this.getHighlights(doc, position)
    if (!highlights) return null
    return highlights.map(o => o.range)
  }

  public hasHighlights(winid: number): boolean {
    return this.highlights.get(winid) != null
  }

  public async getHighlights(doc: Document, position: Position): Promise<DocumentHighlight[]> {
    let line = doc.getline(position.line)
    let ch = line[position.character]
    if (!ch || !doc.isWord(ch)) return null
    await doc.synchronize()
    this.cancel()
    let source = this.tokenSource = new CancellationTokenSource()
    let timer = this.timer = setTimeout(() => {
      if (source.token.isCancellationRequested) return
      source.cancel()
    }, this.config.timeout)
    let highlights = await languages.getDocumentHighLight(doc.textDocument, position, source.token)
    clearTimeout(timer)
    if (source.token.isCancellationRequested) return null
    return highlights
  }

  private cancel(): void {
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource.dispose()
      this.tokenSource = null
    }
  }

  public dispose(): void {
    if (this.timer) clearTimeout(this.timer)
    this.cancel()
    this.highlights.clear()
    disposeAll(this.disposables)
  }
}
