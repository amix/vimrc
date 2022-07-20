'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, InsertTextMode, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import events from '../events'
import { StatusBarItem } from '../model/status'
import { UltiSnippetOption } from '../types'
import { deepClone } from '../util/object'
import { emptyRange, rangeInRange, rangeOverlap } from '../util/position'
import window from '../window'
import workspace from '../workspace'
import { UltiSnippetContext } from './eval'
import { SnippetSession } from './session'
import { normalizeSnippetString, shouldFormat } from './snippet'
import { SnippetString } from './string'
const logger = require('../util/logger')('snippets-manager')

export class SnippetManager {
  private sessionMap: Map<number, SnippetSession> = new Map()
  private disposables: Disposable[] = []
  private statusItem: StatusBarItem
  private highlight: boolean
  private preferComplete: boolean

  constructor() {
    events.on('InsertCharPre', () => {
      // avoid update session when pumvisible
      // Update may cause completion unexpected terminated.
      this.session?.cancel()
    }, null, this.disposables)
    window.onDidChangeActiveTextEditor(e => {
      if (!this.statusItem) return
      let session = this.getSession(e.document.bufnr)
      if (session) {
        this.statusItem.show()
      } else {
        this.statusItem.hide()
      }
    }, null, this.disposables)
    events.on('InsertEnter', async bufnr => {
      let session = this.getSession(bufnr)
      if (session) await session.checkPosition()
    }, null, this.disposables)
    workspace.onDidCloseTextDocument(e => {
      let session = this.getSession(e.bufnr)
      if (session) session.deactivate()
    }, null, this.disposables)
    workspace.onDidChangeConfiguration(e => {
      if (e.affectsConfiguration('suggest') || e.affectsConfiguration('coc.preferences')) {
        this.init()
      }
    }, null, this.disposables)
  }

  private get nvim(): Neovim {
    return workspace.nvim
  }

  public init(): void {
    if (!this.statusItem) this.statusItem = window.createStatusBarItem(0)
    let config = workspace.getConfiguration('coc.preferences')
    this.statusItem.text = config.get<string>('snippetStatusText', 'SNIP')
    this.highlight = config.get<boolean>('snippetHighlight', false)
    let suggest = workspace.getConfiguration('suggest')
    this.preferComplete = suggest.get('preferCompleteThanJumpPlaceholder', false)
  }

  /**
   * Insert snippet at current cursor position
   */
  public async insertSnippet(snippet: string | SnippetString, select = true, range?: Range, insertTextMode?: InsertTextMode, ultisnip?: UltiSnippetOption): Promise<boolean> {
    let { bufnr } = workspace
    let doc = workspace.getAttachedDocument(bufnr)
    if (range && !rangeInRange(range, Range.create(0, 0, doc.lineCount + 1, 0))) {
      throw new Error(`Unable to insert snippet, invalid range.`)
    }
    let context: UltiSnippetContext
    if (events.pumvisible) this.nvim.call('coc#_cancel', [], true)
    if (!range) {
      let pos = await window.getCursorPosition()
      range = Range.create(pos, pos)
    }
    const currentLine = doc.getline(range.start.line)
    const snippetStr = SnippetString.isSnippetString(snippet) ? snippet.value : snippet
    const inserted = await this.normalizeInsertText(doc.uri, snippetStr, currentLine, insertTextMode)
    let session = this.getSession(bufnr)
    if (session) session.cancel()
    if (ultisnip != null) {
      context = Object.assign({ range: deepClone(range), line: currentLine }, ultisnip)
      if (!emptyRange(range) && inserted.includes('`!p')) {
        // same behavior as Ultisnips
        this.nvim.call('coc#cursor#move_to', [range.start.line, range.start.character], true)
        await doc.applyEdits([{ range, newText: '' }])
        range.end = Position.create(range.start.line, range.start.character)
      }
    }
    if (session) {
      await session.forceSynchronize()
      // current session could be canceled on synchronize.
      session = this.getSession(bufnr)
    } else {
      await doc.patchChange(true)
    }
    if (!session) {
      session = new SnippetSession(this.nvim, doc, this.highlight, this.preferComplete)
      session.onCancel(() => {
        this.sessionMap.delete(bufnr)
        this.statusItem.hide()
      })
    }
    let isActive = await session.start(inserted, range, select, context)
    if (isActive) {
      this.statusItem.show()
      this.sessionMap.set(bufnr, session)
    } else {
      this.statusItem.hide()
      this.sessionMap.delete(bufnr)
    }
    return isActive
  }

  public async selectCurrentPlaceholder(triggerAutocmd = true): Promise<void> {
    let { session } = this
    if (session) return await session.selectCurrentPlaceholder(triggerAutocmd)
  }

  public async nextPlaceholder(): Promise<string> {
    let { session } = this
    if (session) {
      await session.nextPlaceholder()
    } else {
      this.nvim.call('coc#snippet#disable', [], true)
      this.statusItem.hide()
    }
    return ''
  }

  public async previousPlaceholder(): Promise<string> {
    let { session } = this
    if (session) {
      await session.previousPlaceholder()
    } else {
      this.nvim.call('coc#snippet#disable', [], true)
      this.statusItem.hide()
    }
    return ''
  }

  public cancel(): void {
    let session = this.getSession(workspace.bufnr)
    if (session) return session.deactivate()
    this.nvim.call('coc#snippet#disable', [], true)
    if (this.statusItem) this.statusItem.hide()
  }

  public get session(): SnippetSession {
    return this.getSession(workspace.bufnr)
  }

  public getSession(bufnr: number): SnippetSession {
    return this.sessionMap.get(bufnr)
  }

  public jumpable(): boolean {
    let { session } = this
    if (!session) return false
    return session.placeholder != null && session.placeholder.index != 0
  }

  public async editsInsideSnippet(edits: TextEdit[]): Promise<boolean> {
    let session = this.getSession(workspace.bufnr)
    if (!session || !session.snippet) return false
    await session.forceSynchronize()
    let range = session.snippet.range
    if (edits.some(e => rangeOverlap(e.range, range))) {
      return true
    }
    return false
  }

  public async resolveSnippet(snippetString: string, ultisnip?: UltiSnippetOption): Promise<string> {
    if (ultisnip) {
      let session = this.getSession(workspace.bufnr)
      ultisnip.noPython = session != null && session.snippet.hasPython
    }
    return await SnippetSession.resolveSnippet(this.nvim, snippetString, ultisnip)
  }

  public async normalizeInsertText(uri: string, snippetString: string, currentLine: string, insertTextMode: InsertTextMode): Promise<string> {
    let inserted = ''
    if (insertTextMode === InsertTextMode.asIs || !shouldFormat(snippetString)) {
      inserted = snippetString
    } else {
      const currentIndent = currentLine.match(/^\s*/)[0]
      const formatOptions = window.activeTextEditor ? window.activeTextEditor.options : await workspace.getFormatOptions(uri)
      inserted = normalizeSnippetString(snippetString, currentIndent, formatOptions)
    }
    return inserted
  }

  public dispose(): void {
    this.cancel()
    for (let d of this.disposables) {
      d.dispose()
    }
  }
}

export default new SnippetManager()
