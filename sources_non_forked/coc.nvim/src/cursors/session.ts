'use strict'
import { Neovim } from '@chemzqm/neovim'
import fastDiff from 'fast-diff'
import { Disposable, Emitter, Event, Range, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import Document from '../model/document'
import { DidChangeTextDocumentParams, HighlightItem } from '../types'
import { disposeAll } from '../util'
import { comparePosition, emptyRange, rangeAdjacent, rangeInRange, rangeIntersect, rangeOverlap } from '../util/position'
import { lineCountChange } from '../util/textedit'
import window from '../window'
import workspace from '../workspace'
import TextRange from './textRange'
import { getBeforeCount, getChange, getDelta, SurrondChange, TextChange } from './util'
const logger = require('../util/logger')('cursors-session')

export interface Config {
  cancelKey: string
  previousKey: string
  nextKey: string
  wrapscan: boolean
}

export interface DiffItem {
  offset: number
  add?: string
  remove?: string
}

/**
 * Cursor session for single buffer
 */
export default class CursorSession {
  private readonly _onDidCancel = new Emitter<void>()
  private readonly _onDidUpdate = new Emitter<void>()
  public readonly onDidCancel: Event<void> = this._onDidCancel.event
  public readonly onDidUpdate: Event<void> = this._onDidUpdate.event
  private disposables: Disposable[] = []
  private ranges: TextRange[] = []
  private activated = true
  private changing = false
  private config: Config
  constructor(private nvim: Neovim, private doc: Document) {
    doc.buffer.setVar('coc_cursors_activated', 1, true)
    this.loadConfig()
    let { cancelKey, nextKey, previousKey } = this.config
    this.disposables.push(workspace.registerLocalKeymap('n', cancelKey, () => {
      this.cancel()
    }))
    this.disposables.push(workspace.registerLocalKeymap('n', nextKey, async () => {
      let ranges = this.ranges.map(o => o.range)
      let curr = await window.getCursorPosition()
      for (let r of ranges) {
        if (comparePosition(r.start, curr) > 0) {
          await window.moveTo(r.start)
          return
        }
      }
      let wrap = this.config.wrapscan
      if (ranges.length && wrap) await window.moveTo(ranges[0].start)
    }))
    this.disposables.push(workspace.registerLocalKeymap('n', previousKey, async () => {
      let ranges = this.ranges.map(o => o.range)
      let curr = await window.getCursorPosition()
      for (let i = ranges.length - 1; i >= 0; i--) {
        let r = ranges[i]
        if (comparePosition(r.end, curr) < 0) {
          await window.moveTo(r.start)
          return
        }
      }
      let wrap = this.config.wrapscan
      if (ranges.length && wrap) await window.moveTo(ranges[ranges.length - 1].start)
    }))
    this.doc.onDocumentChange(async e => {
      await this.onChange(e)
      if (this.activated && !this.changing) {
        this._onDidUpdate.fire()
      }
    }, this, this.disposables)
  }

  private loadConfig(): void {
    let config = workspace.getConfiguration('cursors', this.doc.uri)
    this.config = {
      nextKey: config.get('nextKey', '<C-n>'),
      previousKey: config.get('previousKey', '<C-p>'),
      cancelKey: config.get('cancelKey', '<esc>'),
      wrapscan: config.get('wrapscan', true),
    }
  }

  /**
   * Add or remove range.
   */
  public addRange(range: Range): void {
    let { ranges } = this
    let idx = ranges.findIndex(o => rangeIntersect(o.range, range))
    // remove range when intersect
    if (idx !== -1) {
      ranges.splice(idx, 1)
    } else {
      this.createRange(range)
      ranges.sort((a, b) => comparePosition(a.range.start, b.range.start))
    }
    if (this.ranges.length == 0) {
      this.cancel()
    } else {
      this.doHighlights()
    }
  }

  public addRanges(ranges: Range[]): boolean {
    this.doc._forceSync()
    // filter overlap ranges
    this.ranges = this.ranges.filter(r => {
      return !ranges.some(range => rangeOverlap(range, r.range))
    })
    for (let range of ranges) {
      this.createRange(range)
    }
    this.ranges.sort((a, b) => comparePosition(a.range.start, b.range.start))
    this.doHighlights()
    return true
  }

  private createRange(range: Range): void {
    let { textDocument } = this.doc
    let { line, character } = range.start
    let text = textDocument.getText(range)
    this.ranges.push(new TextRange(line, character, text))
  }

  public async onChange(e: DidChangeTextDocumentParams): Promise<void> {
    if (!this.activated || this.changing) return
    if (e.contentChanges.length === 0) {
      this.doHighlights()
      return
    }
    let change = e.contentChanges[0]
    let { text, range } = change
    let affected = this.ranges.filter(r => {
      if (!rangeIntersect(range, r.range)) return false
      if (rangeAdjacent(range, r.range)) {
        if (text.includes('\n') || !emptyRange(range)) return false
      }
      return true
    })
    if (emptyRange(range) && affected.length > 0) {
      affected = affected.slice(0, 1)
    }
    if (affected.length == 0) {
      logger.debug('no affected ranges')
      this.ranges.forEach(r => {
        r.adjustFromEdit({ range, newText: text })
      })
      this.doHighlights()
    } else if (affected.length == 1 && rangeInRange(range, affected[0].range)) {
      logger.debug('affected single range')
      if (text.includes('\n')) {
        this.cancel()
        return
      }
      // change textRange
      await this.applySingleEdit(affected[0], { range, newText: text })
    } else if (!text.length || !this.validChange(range, text)) {
      logger.debug('filter affected ranges.')
      let ranges = this.ranges.filter(r => !affected.includes(r))
      if (ranges.length > 0) {
        this.ranges = ranges
        ranges.forEach(r => {
          r.adjustFromEdit({ range, newText: text })
        })
        this.doHighlights()
      } else {
        this.cancel()
      }
    } else {
      logger.debug('Check undo & redo')
      let first = this.ranges[0]
      let last = this.ranges[this.ranges.length - 1]
      let originalLines = e.originalLines.slice(first.line, last.line + 1)
      let newLines = this.doc.textDocument.lines.slice(first.line, last.line + 1)
      this.applyComposedEdit(originalLines, newLines)
    }
  }

  public validChange(range: Range, text: string): boolean {
    if (lineCountChange(TextEdit.replace(range, text)) != 0) return false
    if (!rangeInRange(range, this.range)) return false
    let first = this.ranges[0]
    let last = this.ranges[this.ranges.length - 1]
    if (range.start.line != first.position.line || range.end.line != last.position.line) return false
    return true
  }

  public get range(): Range {
    let first = this.ranges[0]
    let last = this.ranges[this.ranges.length - 1]
    return Range.create(first.position, last.range.end)
  }

  private doHighlights(): void {
    let { nvim, ranges, doc } = this
    let buffer = doc.buffer
    let items: HighlightItem[] = []
    ranges.forEach(r => {
      doc.addHighlights(items, 'CocCursorRange', r.range, {
        combine: false,
        start_incl: true,
        end_incl: true
      })
    })
    items.sort((a, b) => {
      if (a.lnum != b.lnum) return a.lnum - b.lnum
      if (a.colStart != b.colStart) return a.colStart - b.colStart
      return 0
    })
    buffer.updateHighlights('cursors', items, { priority: 4096 })
    nvim.redrawVim()
  }

  public get currentRanges(): Range[] {
    return this.ranges.map(r => r.range)
  }

  /**
   * Cancel session and highlights
   */
  public cancel(): void {
    if (!this.activated) return
    logger.debug('cursors cancel')
    let { nvim, doc } = this
    let buffer = doc.buffer
    this.activated = false
    this.ranges = []
    nvim.pauseNotification()
    buffer.clearNamespace('cursors')
    buffer.setVar('coc_cursors_activated', 0, true)
    nvim.resumeNotification(true, true)
    this._onDidUpdate.fire()
    this._onDidCancel.fire()
  }

  /**
   * Called on buffer unload or cancel
   */
  public dispose(): void {
    if (!this.doc) return
    this._onDidCancel.dispose()
    this._onDidUpdate.dispose()
    disposeAll(this.disposables)
    this.ranges = []
    this.doc = null
  }

  private async applySingleEdit(textRange: TextRange, edit: TextEdit): Promise<void> {
    // single range change, calculate & apply changes for all ranges
    let { doc, ranges } = this
    let after = ranges.filter(r => r !== textRange && r.position.line == textRange.position.line)
    after.forEach(r => r.adjustFromEdit(edit))
    let change = getChange(textRange, edit.range, edit.newText)
    let delta = getDelta(change)
    ranges.forEach(r => r.applyChange(change))
    let edits = ranges.filter(r => r !== textRange).map(o => o.textEdit)
    // logger.debug('edits:', JSON.stringify(edits, null, 2))
    this.changing = true
    await doc.applyEdits(edits, true, true)
    this.changing = false
    if (delta != 0) {
      for (let r of ranges) {
        let n = getBeforeCount(r, this.ranges, textRange)
        r.move(n * delta)
      }
    }
    this.doHighlights()
  }

  public applyComposedEdit(originalLines: string[], newLines: string[]): boolean {
    // check complex edit
    let diffs = fastDiff(originalLines[0], newLines[0])
    let first = this.ranges[0]
    // let ranges = this.ranges.filter(o => o.line == first.line)
    let s = first.position.character
    let firstLine = first.position.line
    let len = first.text.length
    let diff = diffs[0]
    if (s > 0 && (diff[0] != fastDiff.EQUAL || !diff[1].startsWith(originalLines[0].slice(0, s)))) {
      this.cancel()
      return false
    }
    let used = 0
    let invalid = false
    let changes: DiffItem[] = []
    for (let i = 0; i < diffs.length; i++) {
      let [kind, text] = diffs[i]
      if (i == 0 && s > 0) {
        text = text.slice(s)
      }
      if (kind == fastDiff.EQUAL) {
        used += text.length
        if (used > len) break
      } else if (kind == fastDiff.DELETE) {
        let offset = used
        used += text.length
        if (used > len) {
          invalid = true
          break
        }
        changes.push({ offset, remove: text })
      } else {
        let prev = diffs[i - 1]
        if (prev && prev[0] == fastDiff.DELETE) {
          changes[changes.length - 1].add = text
        } else {
          changes.push({ offset: used, add: text })
        }
      }
    }
    if (invalid || !changes.length) {
      this.cancel()
      return false
    }
    let doc = TextDocument.create('file:///1', '', 0, originalLines.join('\n'))
    let change: TextChange | SurrondChange
    if (changes.length == 1) {
      change = {
        offset: changes[0].offset,
        remove: changes[0].remove ? changes[0].remove.length : 0,
        insert: changes[0].add ?? ''
      }
    } else if (surrondChanges(changes, len)) {
      change = {
        prepend: [changes[0].remove ? changes[0].remove.length : 0, changes[0].add ?? ''],
        append: [changes[1].remove ? changes[1].remove.length : 0, changes[1].add ?? '']
      }
    } else {
      let text = first.text
      let oldText = ''
      let newText = ''
      let offset = changes[0].offset
      for (let c of changes) {
        if (c.offset > offset + oldText.length) {
          let s = text.slice(offset + oldText.length, c.offset)
          oldText += s
          newText += s
        }
        if (c.add) {
          newText += c.add
        }
        if (c.remove) {
          oldText += c.remove
        }
      }
      change = {
        offset,
        remove: oldText.length,
        insert: newText
      }
    }
    let edits: TextEdit[] = this.ranges.map(o => {
      let line = o.position.line - firstLine
      let { start, end } = o.range
      let range = Range.create(line, start.character, line, end.character)
      o.applyChange(change)
      return TextEdit.replace(range, o.text)
    })
    let content = TextDocument.applyEdits(doc, edits)
    if (content !== newLines.join('\n')) {
      this.cancel()
      return false
    }

    let delta = getDelta(change)
    if (delta != 0) {
      for (let r of this.ranges) {
        let n = getBeforeCount(r, this.ranges)
        r.move(n * delta)
      }
    }
    this.doHighlights()
    return true
  }
}

export function surrondChanges(changes: DiffItem[], len: number): boolean {
  if (changes.length != 2 || changes[0].offset != 0) return false
  let end = changes[1].offset + (changes[1].remove ? changes[1].remove.length : 0)
  if (end !== len) return false
  return true
}
