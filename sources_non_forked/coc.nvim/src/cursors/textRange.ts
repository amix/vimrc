'use strict'
import { Position, Range, TextEdit } from 'vscode-languageserver-types'
import { getEnd } from '../util/position'
import { getChangedPosition } from '../util/textedit'
import { isSurrondChange, SurrondChange, TextChange } from './util'
const logger = require('../util/logger')('cursors-range')

export default class TextRange {
  private start: Position
  private end: Position
  private _text: string

  constructor(line: number, character: number, text: string) {
    this.start = Position.create(line, character)
    this._text = text
    this.end = getEnd(this.start, this._text)
  }

  public get position(): Position {
    return this.start
  }

  public get line(): number {
    return this.start.line
  }

  public get text(): string {
    return this._text
  }

  public get range(): Range {
    return Range.create(this.start, this.end)
  }

  public get textEdit(): TextEdit {
    return {
      range: this.range,
      newText: this.text
    }
  }

  public applyChange(change: SurrondChange | TextChange): void {
    if (isSurrondChange(change)) {
      this.applySurrondChange(change)
    } else {
      this.applyTextChange(change)
    }
  }

  public applySurrondChange(change: SurrondChange): void {
    let { prepend, append } = change
    let len = this._text.length
    let text = this._text.substring(prepend[0], len - append[0])
    this._text = `${prepend[1]}${text}${append[1]}`
  }

  public applyTextChange(change: TextChange): void {
    let { text } = this
    let { offset, remove, fromEnd, insert } = change
    if (fromEnd) offset = -offset
    let pre = text.slice(0, fromEnd && offset == 0 ? text.length : offset)
    let after = text.slice(pre.length)
    if (remove) after = after.slice(remove)
    this._text = `${pre}${insert || ''}${after}`
  }

  /**
   * Adjust range
   */
  public move(delta: number): void {
    if (delta != 0) {
      let { line, character } = this.start
      this.start = Position.create(line, character + delta)
    }
    this.end = getEnd(this.start, this._text)
  }

  public adjustFromEdit(edit: TextEdit): number {
    let changed = getChangedPosition(this.start, edit)
    if (changed.line || changed.character) {
      let { line, character } = this.start
      this.start = Position.create(line + changed.line, character + changed.character)
      this.end = getEnd(this.start, this._text)
    }
    return changed.character
  }

  public isBefore(range: TextRange): boolean {
    let { position } = range
    let { line, character } = this.start
    return position.line == line && position.character > character
  }
}
