'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Emitter, Event, Range } from 'vscode-languageserver-protocol'
import events from '../events'
import Document from '../model/document'
import Documents from './documents'
import window from '../window'
const logger = require('../util/logger')('core-editors')

interface EditorOption {
  bufnr: number
  winid: number
  tabpagenr: number
  winnr: number
  visibleRanges: [number, number][]
  tabSize: number
  insertSpaces: boolean
  winids: number[]
}

export interface TextEditorOptions {
  tabSize: number
  insertSpaces: boolean
}

export interface TextEditor {
  readonly tabpagenr: number
  readonly winid: number
  readonly winnr: number
  readonly document: Document
  readonly visibleRanges: readonly Range[]
  options: TextEditorOptions
}

export default class Editors {
  private disposables: Disposable[] = []
  private winid: number
  private previousId: string
  private nvim: Neovim
  private editors: Map<number, TextEditor> = new Map()
  private readonly _onDidChangeActiveTextEditor = new Emitter<TextEditor | undefined>()
  private readonly _onDidChangeVisibleTextEditors = new Emitter<ReadonlyArray<TextEditor>>()
  public readonly onDidChangeActiveTextEditor: Event<TextEditor | undefined> = this._onDidChangeActiveTextEditor.event
  public readonly onDidChangeVisibleTextEditors: Event<ReadonlyArray<TextEditor>> = this._onDidChangeVisibleTextEditors.event
  constructor(private documents: Documents) {
  }

  public get activeTextEditor(): TextEditor | undefined {
    return this.editors.get(this.winid)
  }

  public get visibleTextEditors(): TextEditor[] {
    return Array.from(this.editors.values())
  }

  private onChange(editor: TextEditor | undefined): void {
    let id = `${editor.winid}-${editor.document.bufnr}-${editor.document.uri}`
    if (id == this.previousId) return
    this.previousId = id
    this._onDidChangeActiveTextEditor.fire(editor)
  }

  public async attach(nvim: Neovim): Promise<void> {
    this.nvim = nvim
    let { documents } = this
    let doc = documents.getDocument(documents.bufnr)
    if (doc && doc.winid > 0) {
      this.winid = doc.winid
      await this.createTextEditor(this.winid)
    }
    events.on('WinEnter', (winid: number) => {
      this.winid = winid
      let editor = this.editors.get(winid)
      if (editor) this.onChange(editor)
    }, null, this.disposables)
    events.on('CursorHold', async () => {
      let [winid, buftype, isFloat] = await nvim.eval(`[win_getid(),&buftype,coc#window#is_float(win_getid())]`) as [number, string, number]
      let changed = false
      if (!isFloat && ['', 'acwrite'].includes(buftype) && !this.editors.has(winid)) {
        let created = await this.createTextEditor(winid)
        if (created) changed = true
      }
      if (changed) this._onDidChangeVisibleTextEditors.fire(this.visibleTextEditors)
    }, null, this.disposables)
    events.on('WinClosed', (winid: number) => {
      if (this.editors.has(winid)) {
        this.editors.delete(winid)
        this._onDidChangeVisibleTextEditors.fire(this.visibleTextEditors)
      }
    }, null, this.disposables)
    events.on('BufWinEnter', async (_: number, winid: number) => {
      this.winid = winid
      await this.createTextEditor(winid, true)
    }, null, this.disposables)
  }

  private async createTextEditor(winid: number, check = false): Promise<boolean> {
    let { documents, nvim } = this
    let opts = await nvim.call('coc#util#get_editoroption', [winid]) as EditorOption
    if (!opts) return false
    let changed = false
    if (check) {
      for (let winid of this.editors.keys()) {
        if (!opts.winids.includes(winid)) {
          changed = true
          this.editors.delete(winid)
        }
      }
    }
    let doc = documents.getDocument(opts.bufnr)
    if (doc) {
      let editor = this.fromOptions(opts, doc)
      this.editors.set(winid, editor)
      if (winid == this.winid) this.onChange(editor)
      this._onDidChangeVisibleTextEditors.fire(this.visibleTextEditors)
      logger.debug('editor created winid & bufnr & tabnr: ', winid, opts.bufnr, opts.tabpagenr)
      return true
    } else if (changed) {
      this._onDidChangeVisibleTextEditors.fire(this.visibleTextEditors)
    }
    logger.error(`document not found for window: ${winid}`)
    return false
  }

  private fromOptions(opts: EditorOption, document: Document): TextEditor {
    let { visibleRanges } = opts
    let tid = window.getTabId(opts.tabpagenr)
    return {
      get tabpagenr() {
        return window.getTabNumber(tid)
      },
      winid: opts.winid,
      winnr: opts.winnr,
      document,
      visibleRanges: visibleRanges.map(o => Range.create(o[0] - 1, 0, o[1], 0)),
      options: {
        tabSize: opts.tabSize,
        insertSpaces: !!opts.insertSpaces
      }
    }
  }
}
