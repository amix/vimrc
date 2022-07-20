'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationToken, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { LinesTextDocument } from '../model/textdocument'
import { emptyRange, getEnd, positionInRange, rangeInRange } from '../util/position'
import { getChangedPosition } from '../util/textedit'
import { prepareMatchCode, preparePythonCodes, UltiSnippetContext } from './eval'
import * as Snippets from "./parser"
import { VariableResolver } from './parser'
const logger = require('../util/logger')('snippets-snipet')

export interface CocSnippetPlaceholder {
  index: number | undefined
  marker: Snippets.Placeholder | Snippets.Variable
  value: string
  primary: boolean
  transform: boolean
  // range in current buffer
  range: Range
  // snippet text before
  before: string
  // snippet text after
  after: string
}

export class CocSnippet {
  private _placeholders: CocSnippetPlaceholder[]
  private _text: string | undefined
  public tmSnippet: Snippets.TextmateSnippet

  constructor(private snippetString: string,
    private position: Position,
    private nvim: Neovim,
    private resolver?: VariableResolver,
  ) {
  }

  public async init(ultisnip?: UltiSnippetContext, isResolve = false): Promise<void> {
    const matchCode = ultisnip ? prepareMatchCode(ultisnip) : undefined
    const parser = new Snippets.SnippetParser(!!ultisnip, matchCode)
    const snippet = parser.parse(this.snippetString, true)
    this.tmSnippet = snippet
    await this.resolve(ultisnip)
    this.synchronize()
    if (!isResolve) {
      this.nvim.call('coc#compat#del_var', ['coc_selected_text'], true)
      this.nvim.call('coc#compat#del_var', ['coc_last_placeholder'], true)
    }
  }

  private async resolve(ultisnip?: UltiSnippetContext): Promise<void> {
    let { snippet } = this.tmSnippet
    let { resolver, nvim } = this
    if (resolver) {
      await snippet.resolveVariables(resolver)
    }
    if (ultisnip && ultisnip.noPython !== true) {
      let pyCodes: string[] = []
      if (snippet.hasPython) pyCodes = preparePythonCodes(ultisnip)
      await snippet.evalCodeBlocks(nvim, pyCodes)
    }
  }

  public getRanges(placeholder: CocSnippetPlaceholder): Range[] {
    let marker = placeholder.marker
    if (placeholder.value.length == 0) return []
    let placeholders = this._placeholders.filter(o => o.index == placeholder.index)
    let ranges = placeholders.map(o => o.range)
    let parents = this.tmSnippet.enclosingPlaceholders(marker)
    let markers: Snippets.Marker[]
    let p = marker.parent
    if (marker instanceof Snippets.Placeholder) {
      let index = marker.index
      markers = this.tmSnippet.placeholders.filter(o => o.index == index && o.parent == p)
    } else {
      let name = marker.name
      markers = this.tmSnippet.variables.filter(o => o.name == name && o.parent == p)
    }
    parents.forEach(p => {
      let arr = this._placeholders.filter(o => o.index == p.index && o.marker !== p)
      if (!arr.length) return
      for (let m of markers) {
        let before = this.tmSnippet.getTextBefore(m, p)
        arr.forEach(item => {
          if (item.transform) {
            ranges.push(item.range)
          } else {
            let s = item.range.start
            ranges.push(Range.create(getEnd(s, before), getEnd(s, before + m.toString())))
          }
        })
      }
    })
    return ranges.filter(r => !emptyRange(r))
  }

  public getSortedPlaceholders(curr?: CocSnippetPlaceholder | undefined): CocSnippetPlaceholder[] {
    let res = curr ? [curr] : []
    let arr = this._placeholders.filter(o => o !== curr && !o.transform)
    arr.sort((a, b) => {
      if (a.primary !== b.primary) return a.primary ? -1 : 1
      if (a.index == 0 || b.index == 0) return a.index == 0 ? 1 : -1
      return a.index - b.index
    })
    res.push(...arr)
    return res
  }

  public get hasPython(): boolean {
    return this.tmSnippet.pyBlocks.length > 0
  }

  public resetStartPosition(pos: Position): void {
    this.position = pos
    this.synchronize()
  }

  public get start(): Position {
    return Object.assign({}, this.position)
  }

  public get range(): Range {
    return Range.create(this.position, getEnd(this.position, this._text))
  }

  public get text(): string {
    return this._text
  }

  public get finalCount(): number {
    return this._placeholders.filter(o => o.index == 0).length
  }

  public get placeholders(): ReadonlyArray<Snippets.Marker> {
    return this._placeholders.map(o => o.marker)
  }

  public get firstPlaceholder(): CocSnippetPlaceholder | undefined {
    let index = 0
    for (let p of this._placeholders) {
      if (p.index == 0 || p.transform) continue
      if (index == 0 || p.index < index) {
        index = p.index
      }
    }
    return this.getPlaceholder(index)
  }

  public getPlaceholderByMarker(marker: Snippets.Marker): CocSnippetPlaceholder {
    return this._placeholders.find(o => o.marker === marker)
  }

  public getPlaceholder(index: number): CocSnippetPlaceholder {
    let filtered = this._placeholders.filter(o => o.index == index && !o.transform)
    let find = filtered.find(o => o.primary) || filtered[0]
    return find ?? filtered[0]
  }

  public getPrevPlaceholder(index: number): CocSnippetPlaceholder | undefined {
    if (index <= 1) return undefined
    let placeholders = this._placeholders.filter(o => o.index < index && o.index != 0 && !o.transform)
    let find: CocSnippetPlaceholder
    while (index > 1) {
      index = index - 1
      let arr = placeholders.filter(o => o.index == index)
      if (arr.length) {
        find = arr.find(o => o.primary) || arr[0]
        break
      }
    }
    return find
  }

  public getNextPlaceholder(index: number): CocSnippetPlaceholder | undefined {
    let placeholders = this._placeholders.filter(o => !o.transform)
    let find: CocSnippetPlaceholder
    let indexes = placeholders.map(o => o.index)
    let max = Math.max.apply(null, indexes)
    for (let i = index + 1; i <= max + 1; i++) {
      let idx = i == max + 1 ? 0 : i
      let arr = placeholders.filter(o => o.index == idx)
      if (arr.length) {
        find = arr.find(o => o.primary) || arr[0]
        break
      }
    }
    return find
  }

  public getPlaceholderByRange(range: Range): CocSnippetPlaceholder {
    return this._placeholders.find(o => rangeInRange(range, o.range))
  }

  public async insertSnippet(placeholder: CocSnippetPlaceholder, snippet: string, parts: [string, string], ultisnip?: UltiSnippetContext): Promise<Snippets.Placeholder | Snippets.Variable> {
    if (ultisnip) {
      let { start, end } = placeholder.range
      this.nvim.setVar('coc_last_placeholder', {
        current_text: placeholder.value,
        start: { line: start.line, col: start.character, character: start.character },
        end: { line: end.line, col: end.character, character: end.character }
      }, true)
    }
    let select = this.tmSnippet.insertSnippet(snippet, placeholder.marker, parts, ultisnip)
    await this.resolve(ultisnip)
    this.synchronize()
    return select
  }

  /**
   * Check newText for placeholder.
   */
  public getNewText(placeholder: CocSnippetPlaceholder, inserted: string): string | undefined {
    let { before, after } = placeholder
    if (!inserted.startsWith(before)) return undefined
    if (inserted.length < before.length + after.length) return undefined
    if (!inserted.endsWith(after)) return undefined
    if (!after.length) return inserted.slice(before.length)
    return inserted.slice(before.length, - after.length)
  }

  public async updatePlaceholder(placeholder: CocSnippetPlaceholder, cursor: Position, newText: string, token: CancellationToken): Promise<{ text: string; delta: Position } | undefined> {
    let start = this.position
    let { marker, before } = placeholder
    let cloned = this.tmSnippet.clone()
    token.onCancellationRequested(() => {
      this.tmSnippet = cloned
      this.synchronize()
    })
    // range before placeholder
    let r = Range.create(start, getEnd(start, before))
    await this.tmSnippet.update(this.nvim, marker, newText)
    if (token.isCancellationRequested) return undefined
    this.synchronize()
    let p = this._placeholders.find(o => o.marker == marker)
    let after = p ? p.before : before
    return { text: this._text, delta: getChangedPosition(cursor, TextEdit.replace(r, after)) }
  }

  public removeText(offset: number, length: number): boolean {
    let succeed = this.tmSnippet.deleteText(offset, length)
    if (succeed) this.synchronize()
    return succeed
  }

  private synchronize(): void {
    const snippet = this.tmSnippet
    const { line, character } = this.position
    const document = TextDocument.create('untitled:/1', 'snippet', 0, snippet.toString())
    let { placeholders, variables, maxIndexNumber } = snippet
    const variableIndexMap: Map<string, number> = new Map()
    let variableIndex = maxIndexNumber + 1

    this._placeholders = [...placeholders, ...variables].map(p => {
      const offset = snippet.offset(p)
      const position = document.positionAt(offset)
      const start: Position = {
        line: line + position.line,
        character: position.line == 0 ? character + position.character : position.character
      }
      let index: number
      if (p instanceof Snippets.Variable) {
        let key = p.name
        if (variableIndexMap.has(key)) {
          index = variableIndexMap.get(key)
        } else {
          variableIndexMap.set(key, variableIndex)
          index = variableIndex
          variableIndex = variableIndex + 1
        }
      } else {
        index = p.index
      }
      const value = p.toString()
      const end = getEnd(position, value)
      let res: CocSnippetPlaceholder = {
        index,
        value,
        marker: p,
        transform: !!p.transform,
        range: Range.create(start, getEnd(start, value)),
        before: document.getText(Range.create(Position.create(0, 0), position)),
        after: document.getText(Range.create(end, Position.create(document.lineCount, 0))),
        primary: p instanceof Snippets.Placeholder && p.primary === true
      }
      return res
    })
    this._text = this.tmSnippet.toString()
  }
}

/**
 * Current line text before marker
 */
export function getContentBefore(marker: Snippets.Marker): string {
  let res = ''
  const calc = (m: Snippets.Marker): void => {
    let p = m.parent
    if (!p) return
    let s = ''
    for (let b of p.children) {
      if (b === m) break
      s = s + b.toString()
    }
    if (s.indexOf('\n') !== -1) {
      let arr = s.split(/\n/)
      res = arr[arr.length - 1] + res
      return
    }
    res = s + res
    calc(p)
  }
  calc(marker)
  return res
}

/*
 * Avoid change unnecessary range of text.
 */
export function reduceTextEdit(edit: TextEdit, oldText: string): TextEdit {
  let { range, newText } = edit
  let ol = oldText.length
  let nl = newText.length
  if (ol === 0 || nl === 0) return edit
  let { start, end } = range
  let bo = 0
  for (let i = 1; i <= Math.min(nl, ol); i++) {
    if (newText[i - 1] === oldText[i - 1]) {
      bo = i
    } else {
      break
    }
  }
  let eo = 0
  let t = Math.min(nl - bo, ol - bo)
  if (t > 0) {
    for (let i = 1; i <= t; i++) {
      if (newText[nl - i] === oldText[ol - i]) {
        eo = i
      } else {
        break
      }
    }
  }
  let text = eo == 0 ? newText.slice(bo) : newText.slice(bo, -eo)
  if (bo > 0) start = getEnd(start, newText.slice(0, bo))
  if (eo > 0) end = getEnd(range.start, oldText.slice(0, -eo))
  return TextEdit.replace(Range.create(start, end), text)
}

/*
 * Check if cursor inside
 */
export function checkCursor(start: Position, cursor: Position, newText: string): boolean {
  let r = Range.create(start, getEnd(start, newText))
  return positionInRange(cursor, r) == 0
}

/*
 * Check if textDocument have same text before position.
 */
export function checkContentBefore(position: Position, oldTextDocument: LinesTextDocument, textDocument: LinesTextDocument): boolean {
  let lines = textDocument.lines
  if (lines.length < position.line) return false
  let checked = true
  for (let i = position.line; i >= 0; i--) {
    let newLine = textDocument.lines[i] ?? ''
    if (i === position.line) {
      let before = oldTextDocument.lines[i].slice(0, position.character)
      if (!newLine.startsWith(before)) {
        checked = false
        break
      }
    } else if (newLine !== oldTextDocument.lines[i]) {
      checked = false
      break
    }
  }
  return checked
}

/**
 * Get new end position by old end position and new TextDocument
 */
export function getEndPosition(position: Position, oldTextDocument: LinesTextDocument, textDocument: LinesTextDocument): Position | undefined {
  let total = oldTextDocument.lines.length
  if (textDocument.lines.length < total - position.line) return undefined
  let end: Position
  let cl = textDocument.lines.length - total
  for (let i = position.line; i < total; i++) {
    let newLine = textDocument.lines[i + cl]
    if (i == position.line) {
      let text = oldTextDocument.lines[i].slice(position.character)
      if (text.length && !newLine.endsWith(text)) break
      end = Position.create(i + cl, newLine.length - text.length)
    } else if (newLine !== oldTextDocument.lines[i]) {
      end = undefined
      break
    }
  }
  return end
}

/*
 * r in range
 */
export function getParts(text: string, range: Range, r: Range): [string, string] {
  let before: string[] = []
  let after: string[] = []
  let lines = text.split('\n')
  let d = r.start.line - range.start.line
  for (let i = 0; i <= d; i++) {
    let s = lines[i] ?? ''
    if (i == d) {
      before.push(i == 0 ? s.substring(0, r.start.character - range.start.character) : s.substring(0, r.start.character))
    } else {
      before.push(s)
    }
  }
  d = range.end.line - r.end.line
  for (let i = 0; i <= d; i++) {
    let s = lines[r.end.line - range.start.line + i] ?? ''
    if (i == 0) {
      if (d == 0) {
        after.push(range.end.character == r.end.character ? '' : s.slice(r.end.character - range.end.character))
      } else {
        after.push(s.substring(r.end.character))
      }
    } else {
      after.push(s)
    }
  }
  return [before.join('\n'), after.join('\n')]
}

export function normalizeSnippetString(snippet: string, indent: string, opts: { tabSize: number, insertSpaces: boolean }): string {
  let lines = snippet.split(/\r?\n/)
  let ind = opts.insertSpaces ? ' '.repeat(opts.tabSize) : '\t'
  let tabSize = opts.tabSize || 2
  lines = lines.map((line, idx) => {
    let space = line.match(/^\s*/)[0]
    let pre = space
    let isTab = space.startsWith('\t')
    if (isTab && opts.insertSpaces) {
      pre = ind.repeat(space.length)
    } else if (!isTab && !opts.insertSpaces) {
      pre = ind.repeat(space.length / tabSize)
    }
    return (idx == 0 || line.length == 0 ? '' : indent) + pre + line.slice(space.length)
  })
  return lines.join('\n')
}

export function shouldFormat(snippet: string): boolean {
  if (/^\s/.test(snippet)) return true
  if (snippet.indexOf('\n') !== -1) return true
  return false
}
