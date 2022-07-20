'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Range } from 'vscode-languageserver-protocol'
import Document from '../model/document'
import { comparePosition } from '../util/position'
import window from '../window'
import workspace from '../workspace'
import CursorSession from './session'
import { getVisualRanges, splitRange } from './util'
const logger = require('../util/logger')('cursors')

export default class Cursors {
  private sessionsMap: Map<number, CursorSession> = new Map()
  private disposables: Disposable[] = []
  constructor(private nvim: Neovim) {
    workspace.onDidCloseTextDocument(e => {
      let session = this.getSession(e.bufnr)
      if (!session) return
      this.sessionsMap.delete(e.bufnr)
      session.cancel()
    }, null, this.disposables)
  }

  public cancel(uri: number | string): void {
    let doc = workspace.getDocument(uri)
    if (!doc) return
    let session = this.getSession(doc.bufnr)
    if (session) session.cancel()
  }

  public getSession(bufnr: number): CursorSession | undefined {
    return this.sessionsMap.get(bufnr)
  }

  public async isActivated(): Promise<boolean> {
    let bufnr = await this.nvim.call('bufnr', ['%']) as number
    return this.sessionsMap.get(bufnr) != null
  }

  public async select(bufnr: number, kind: string, mode: string): Promise<void> {
    let doc = workspace.getAttachedDocument(bufnr)
    let { nvim } = this
    let session = this.createSession(doc)
    let pos = await window.getCursorPosition()
    let range: Range
    if (kind == 'operator') {
      await nvim.command(`normal! ${mode == 'line' ? `'[` : '`['}`)
      let start = await window.getCursorPosition()
      await nvim.command(`normal! ${mode == 'line' ? `']` : '`]'}`)
      let end = await window.getCursorPosition()
      await window.moveTo(pos)
      let relative = comparePosition(start, end)
      // do nothing for empty range
      if (relative == 0) return
      if (relative >= 0) [start, end] = [end, start]
      // include end character
      let line = doc.getline(end.line)
      if (end.character < line.length) {
        end.character = end.character + 1
      }
      let ranges = splitRange(doc, Range.create(start, end))
      session.addRanges(ranges)
    } else if (kind == 'word') {
      range = doc.getWordRangeAtPosition(pos)
      if (!range) {
        let line = doc.getline(pos.line)
        if (pos.character == line.length) {
          range = Range.create(pos.line, Math.max(0, line.length - 1), pos.line, line.length)
        } else {
          range = Range.create(pos.line, pos.character, pos.line, pos.character + 1)
        }
      }
      session.addRange(range)
      await nvim.command(`silent! call repeat#set("\\<Plug>(coc-cursors-${kind})", -1)`)
    } else if (kind == 'position') {
      // make sure range contains character for highlight
      let line = doc.getline(pos.line)
      if (pos.character >= line.length) {
        range = Range.create(pos.line, line.length - 1, pos.line, line.length)
      } else {
        range = Range.create(pos.line, pos.character, pos.line, pos.character + 1)
      }
      session.addRange(range)
      await nvim.command(`silent! call repeat#set("\\<Plug>(coc-cursors-${kind})", -1)`)
    } else if (kind == 'range') {
      await nvim.call('eval', 'feedkeys("\\<esc>", "in")')
      let range = await window.getSelectedRange(mode)
      if (!range) return
      let ranges = mode == '\x16' ? getVisualRanges(doc, range) : splitRange(doc, range)
      for (let r of ranges) {
        session.addRange(r)
      }
    } else {
      throw new Error(`select kind "${kind}" not supported`)
    }
  }

  public createSession(doc: Document): CursorSession {
    let { bufnr } = doc
    let session = this.getSession(bufnr)
    if (session) return session
    session = new CursorSession(this.nvim, doc)
    this.sessionsMap.set(bufnr, session)
    session.onDidCancel(() => {
      session.dispose()
      this.sessionsMap.delete(bufnr)
    })
    return session
  }

  // Add ranges to current document
  public async addRanges(ranges: Range[]): Promise<boolean> {
    let { nvim } = this
    let bufnr = await nvim.call('bufnr', ['%']) as number
    let doc = workspace.getAttachedDocument(bufnr)
    let session = this.createSession(doc)
    return session.addRanges(ranges)
  }

  public reset(): void {
    for (let session of this.sessionsMap.values()) {
      session.cancel()
    }
    this.sessionsMap.clear()
  }
}
