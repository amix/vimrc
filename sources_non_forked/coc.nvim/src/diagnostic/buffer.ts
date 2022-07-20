'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import { debounce } from 'debounce'
import { Diagnostic, DiagnosticSeverity, Position, TextEdit } from 'vscode-languageserver-protocol'
import events from '../events'
import { SyncItem } from '../model/bufferSync'
import Document from '../model/document'
import { DidChangeTextDocumentParams, HighlightItem, LocationListItem } from '../types'
import { lineInRange, positionInRange } from '../util/position'
import workspace from '../workspace'
import { adjustDiagnostics, DiagnosticConfig, getHighlightGroup, getLocationListItem, getNameFromSeverity, getSeverityType, sortDiagnostics } from './util'
const logger = require('../util/logger')('diagnostic-buffer')
const signGroup = 'CocDiagnostic'
const NAMESPACE = 'diagnostic'
// higher priority first
const hlGroups = ['CocErrorHighlight', 'CocWarningHighlight', 'CocInfoHighlight', 'CocHintHighlight', 'CocDeprecatedHighlight', 'CocUnusedHighlight']

interface DiagnosticInfo {
  /**
   * current bufnr
   */
  bufnr: number
  lnum: number
  winid: number
  locationlist: string
}

interface SignItem {
  name: string
  lnum: number
  priority?: number
}

const delay = global.__TEST__ ? 10 : 500
const aleMethod = global.__TEST__ ? 'MockAleResults' : 'ale#other_source#ShowResults'

/**
 * Manage diagnostics of buffer, including:
 *
 * - highlights
 * - variable
 * - signs
 * - location list
 * - virtual text
 */
export class DiagnosticBuffer implements SyncItem {
  private diagnosticsMap: Map<string, ReadonlyArray<Diagnostic>> = new Map()
  private _disposed = false
  private _dirty = false
  private _changeTs = 0
  public refreshHighlights: Function & { clear(): void }
  constructor(
    private readonly nvim: Neovim,
    public readonly doc: Document,
    private config: DiagnosticConfig,
    private onRefresh: (diagnostics: ReadonlyArray<Diagnostic>) => void
  ) {
    this.refreshHighlights = debounce(this._refresh.bind(this), delay)
  }

  public get dirty(): boolean {
    return this._dirty
  }

  public get bufnr(): number {
    return this.doc.bufnr
  }

  public get uri(): string {
    return this.doc.uri
  }

  public onChange(e: DidChangeTextDocumentParams): void {
    let changes = e.contentChanges
    if (changes.length > 0) {
      this._changeTs = Date.now()
      let edit = TextEdit.replace(changes[0].range, changes[0].text)
      for (let [collection, diagnostics] of this.diagnosticsMap.entries()) {
        if (diagnostics.length) {
          let arr = adjustDiagnostics(diagnostics, edit)
          this.diagnosticsMap.set(collection, arr)
        }
      }
    }
    this.refreshHighlights()
  }

  public onTextChange(): void {
    this._dirty = true
    this.refreshHighlights.clear()
  }

  private get displayByAle(): boolean {
    return this.config.displayByAle
  }

  private clearHighlight(collection: string): void {
    this.buffer.clearNamespace(NAMESPACE + collection)
  }

  private clearSigns(collection: string): void {
    this.buffer.unplaceSign({ group: signGroup + collection })
  }

  private get diagnostics(): Diagnostic[] {
    let res: Diagnostic[] = []
    for (let diags of this.diagnosticsMap.values()) {
      res.push(...diags)
    }
    return res
  }

  private get buffer(): Buffer {
    return this.nvim.createBuffer(this.bufnr)
  }

  private refreshAle(collection: string, diagnostics: ReadonlyArray<Diagnostic>): void {
    let aleItems = diagnostics.map(o => {
      let range = o.range
      return {
        text: o.message,
        code: o.code,
        lnum: range.start.line + 1,
        col: range.start.character + 1,
        end_lnum: range.end.line + 1,
        end_col: range.end.character,
        type: getSeverityType(o.severity)
      }
    })
    this.nvim.call(aleMethod, [this.bufnr, 'coc' + collection, aleItems], true)
  }

  /**
   * Update diagnostics when diagnostics change on collection.
   *
   * @param {string} collection
   * @param {Diagnostic[]} diagnostics
   */
  public async update(collection: string, diagnostics: ReadonlyArray<Diagnostic>): Promise<void> {
    let { diagnosticsMap } = this
    let curr = diagnosticsMap.get(collection) || []
    if (!this._dirty && diagnostics.length == 0 && curr.length == 0) return
    diagnosticsMap.set(collection, diagnostics)
    if (this._dirty || Date.now() - this._changeTs < delay) {
      this._dirty = true
      return
    }
    let info = await this.getDiagnosticInfo()
    // avoid highlights on invalid state or buffer hidden.
    if (this._dirty || !info || info.winid == -1) {
      this._dirty = true
      return
    }
    let map: Map<string, ReadonlyArray<Diagnostic>> = new Map()
    map.set(collection, diagnostics)
    this.refresh(map, info)
  }

  /**
   * Reset all diagnostics of current buffer
   */
  public async reset(diagnostics: { [collection: string]: Diagnostic[] }, force?: boolean): Promise<void> {
    this._changeTs = Date.now()
    let { diagnosticsMap } = this
    for (let key of diagnosticsMap.keys()) {
      // make sure clear collection when it's empty.
      if (diagnostics[key] == null) diagnostics[key] = []
    }
    for (let [key, value] of Object.entries(diagnostics)) {
      this.diagnosticsMap.set(key, value)
    }
    let info = await this.getDiagnosticInfo(force)
    if (!info) {
      this._dirty = true
      return
    }
    this.refresh(this.diagnosticsMap, info)
  }

  /**
   * Get buffer info needed for refresh.
   */
  private async getDiagnosticInfo(force?: boolean): Promise<DiagnosticInfo | undefined> {
    let { refreshOnInsertMode } = this.config
    let { nvim, bufnr } = this
    let checkInsert = !refreshOnInsertMode
    if (force) {
      checkInsert = false
    } else {
      let disabledByInsert = events.insertMode && !refreshOnInsertMode
      if (disabledByInsert) return undefined
    }
    return await nvim.call('coc#util#diagnostic_info', [bufnr, checkInsert])
  }

  /**
   * Refresh changed diagnostics to UI.
   */
  private refresh(diagnosticsMap: Map<string, ReadonlyArray<Diagnostic>>, info: DiagnosticInfo): void {
    let { nvim, displayByAle } = this
    this._dirty = false
    if (displayByAle) {
      nvim.pauseNotification()
      for (let [collection, diagnostics] of diagnosticsMap.entries()) {
        this.refreshAle(collection, diagnostics)
      }
      nvim.resumeNotification(true, true)
    } else {
      let emptyCollections: string[] = []
      nvim.pauseNotification()
      for (let [collection, diagnostics] of diagnosticsMap.entries()) {
        if (diagnostics.length == 0) emptyCollections.push(collection)
        this.addSigns(collection, diagnostics)
        this.updateHighlights(collection, diagnostics)
      }
      this.showVirtualText(info.lnum, info.bufnr)
      this.updateLocationList(info.winid, info.locationlist)
      this.setDiagnosticInfo()
      nvim.resumeNotification(true, true)
      // cleanup unnecessary collections
      emptyCollections.forEach(name => {
        this.diagnosticsMap.delete(name)
      })
      this.onRefresh(this.diagnostics)
    }
  }

  public updateLocationList(winid: number, title: string): void {
    if (!this.config.locationlistUpdate || winid == -1 || title !== 'Diagnostics of coc') return
    let items = this.toLocationListItems(this.diagnostics)
    this.nvim.call('setloclist', [winid, [], 'r', { title: 'Diagnostics of coc', items }], true)
  }

  public toLocationListItems(diagnostics: Diagnostic[]): LocationListItem[] {
    let { locationlistLevel } = this.config
    let items: LocationListItem[] = []
    let lines = this.doc.textDocument.lines
    diagnostics.sort(sortDiagnostics)
    for (let diagnostic of diagnostics) {
      if (locationlistLevel && diagnostic.severity && diagnostic.severity > locationlistLevel) continue
      items.push(getLocationListItem(this.bufnr, diagnostic, lines))
    }
    return items
  }

  public addSigns(collection: string, diagnostics: ReadonlyArray<Diagnostic>): void {
    let { enableSign, signLevel } = this.config
    if (!enableSign) return
    let group = signGroup + collection
    let signs: SignItem[] = []
    // this.buffer.unplaceSign({ group })
    let signsMap: Map<number, DiagnosticSeverity[]> = new Map()
    for (let diagnostic of diagnostics) {
      let { range, severity } = diagnostic
      if (!severity || (signLevel && severity > signLevel)) {
        continue
      }
      let line = range.start.line
      let exists = signsMap.get(line) || []
      if (exists.includes(severity)) {
        continue
      }
      exists.push(severity)
      signsMap.set(line, exists)
      let priority = this.config.signPriority + 4 - severity
      signs.push({ name: getNameFromSeverity(severity), lnum: line + 1, priority })
    }
    this.nvim.call('coc#ui#update_signs', [this.bufnr, group, signs], true)
  }

  public setDiagnosticInfo(): void {
    let lnums = [0, 0, 0, 0]
    let info = { error: 0, warning: 0, information: 0, hint: 0, lnums }
    for (let diagnostics of this.diagnosticsMap.values()) {
      for (let diagnostic of diagnostics) {
        let lnum = diagnostic.range.start.line + 1
        switch (diagnostic.severity) {
          case DiagnosticSeverity.Warning:
            info.warning = info.warning + 1
            lnums[1] = lnums[1] ? Math.min(lnums[1], lnum) : lnum
            break
          case DiagnosticSeverity.Information:
            info.information = info.information + 1
            lnums[2] = lnums[2] ? Math.min(lnums[2], lnum) : lnum
            break
          case DiagnosticSeverity.Hint:
            info.hint = info.hint + 1
            lnums[3] = lnums[3] ? Math.min(lnums[3], lnum) : lnum
            break
          default:
            lnums[0] = lnums[0] ? Math.min(lnums[0], lnum) : lnum
            info.error = info.error + 1
        }
      }
    }
    let buf = this.nvim.createBuffer(this.bufnr)
    buf.setVar('coc_diagnostic_info', info, true)
    this.nvim.call('coc#util#do_autocmd', ['CocDiagnosticChange'], true)
  }

  public showVirtualText(lnum: number, bufnr?: number): void {
    let { config } = this
    let { virtualText, virtualTextLevel } = config
    if (!virtualText) return
    let { virtualTextSrcId, virtualTextPrefix, virtualTextCurrentLineOnly } = this.config
    let { diagnostics, buffer } = this
    if (virtualTextCurrentLineOnly) {
      if (bufnr && this.bufnr != bufnr) return
      diagnostics = diagnostics.filter(d => {
        let { start, end } = d.range
        return start.line <= lnum - 1 && end.line >= lnum - 1
      })
    }
    diagnostics.sort(sortDiagnostics)
    buffer.clearNamespace(virtualTextSrcId)
    for (let i = diagnostics.length - 1; i >= 0; i--) {
      let diagnostic = diagnostics[i]
      if (virtualTextLevel && diagnostic.severity && diagnostic.severity > virtualTextLevel) {
        continue
      }
      let { line } = diagnostic.range.start
      let highlight = getNameFromSeverity(diagnostic.severity) + 'VirtualText'
      let msg = diagnostic.message.split(/\n/)
        .map((l: string) => l.trim())
        .filter((l: string) => l.length > 0)
        .slice(0, this.config.virtualTextLines)
        .join(this.config.virtualTextLineSeparator)
      if (workspace.has('nvim-0.5.1')) {
        let opts: any = {
          virt_text: [[virtualTextPrefix + msg, highlight]]
        }
        if (config.virtualTextAlignRight) {
          // opts.virt_text_pos = 'right_align'
        } else if (typeof config.virtualTextWinCol === 'number') {
          opts.virt_text_win_col = config.virtualTextWinCol
        }
        buffer.setExtMark(virtualTextSrcId, line, 0, opts)
      } else {
        void buffer.setVirtualText(virtualTextSrcId, line, [[virtualTextPrefix + msg, highlight]], {})
      }
    }
  }

  public updateHighlights(collection: string, diagnostics: ReadonlyArray<Diagnostic>): void {
    if (!diagnostics.length) {
      this.clearHighlight(collection)
    } else {
      let items = this.getHighlightItems(diagnostics)
      let priority = this.config.highlightPriority
      this.buffer.updateHighlights(NAMESPACE + collection, items, { priority })
    }
  }

  /**
   * Refresh all diagnostics
   */
  private async _refresh(): Promise<void> {
    if (!this._dirty) return
    let info = await this.getDiagnosticInfo()
    let noHighlights = !info || info.winid == -1
    if (noHighlights || this.diagnosticsMap.size == 0) return
    this.refresh(this.diagnosticsMap, info)
  }

  public getHighlightItems(diagnostics: ReadonlyArray<Diagnostic>): HighlightItem[] {
    let doc = workspace.getDocument(this.uri)
    if (!doc) return []
    let res: HighlightItem[] = []
    for (let diagnostic of diagnostics.slice(0, this.config.highlighLimit)) {
      let hlGroup = getHighlightGroup(diagnostic)
      doc.addHighlights(res, hlGroup, diagnostic.range)
    }
    // needed for iteration performance and since diagnostic highlight may cross lines.
    res.sort((a, b) => {
      if (a.lnum != b.lnum) return a.lnum - b.lnum
      if (a.colStart != b.colStart) return a.colStart - b.colStart
      return hlGroups.indexOf(b.hlGroup) - hlGroups.indexOf(a.hlGroup)
    })
    return res
  }

  /**
   * Clear all diagnostics from UI.
   */
  public clear(): void {
    let { nvim } = this
    let collections = Array.from(this.diagnosticsMap.keys())
    this.refreshHighlights.clear()
    this.diagnosticsMap.clear()
    if (this.displayByAle) {
      for (let collection of collections) {
        this.nvim.call(aleMethod, [this.bufnr, collection, []], true)
      }
    } else {
      nvim.pauseNotification()
      this.buffer.deleteVar('coc_diagnostic_info')
      for (let collection of collections) {
        this.clearHighlight(collection)
        this.clearSigns(collection)
      }
      if (this.config.virtualText) {
        this.buffer.clearNamespace(this.config.virtualTextSrcId)
      }
      nvim.resumeNotification(true, true)
    }
  }

  /**
   * Get diagnostics at cursor position.
   */
  public getDiagnosticsAt(pos: Position, checkCurrentLine: boolean): Diagnostic[] {
    let diagnostics: Diagnostic[] = []
    for (let diags of this.diagnosticsMap.values()) {
      if (checkCurrentLine) {
        diagnostics.push(...diags.filter(o => lineInRange(pos.line, o.range)))
      } else {
        diagnostics.push(...diags.filter(o => positionInRange(pos, o.range) == 0))
      }
    }
    diagnostics.sort(sortDiagnostics)
    return diagnostics
  }

  public async isEnabled(): Promise<boolean> {
    if (this._disposed) return false
    let buf = this.nvim.createBuffer(this.bufnr)
    let res = await buf.getVar('coc_diagnostic_disable')
    return res != 1
  }

  public dispose(): void {
    this.clear()
    this._disposed = true
    this.refreshHighlights.clear()
  }
}
