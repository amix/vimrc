'use strict'
import { Range } from 'vscode-languageserver-protocol'
import Document from '../model/document'
import { equals } from '../util/object'
import { getWellformedRange } from '../util/textedit'
import type TextRange from './textRange'

export interface TextChange {
  offset: number
  remove: number
  insert: string
  fromEnd?: boolean
}

export interface SurrondChange {
  /**
   * delete count & insert text
   */
  prepend: [number, string]
  /**
   * delete count & insert text
   */
  append: [number, string]
}

/**
 * Split to single line ranges
 */
export function splitRange(doc: Document, range: Range): Range[] {
  let splited: Range[] = []
  for (let i = range.start.line; i <= range.end.line; i++) {
    let curr = doc.getline(i) || ''
    let sc = i == range.start.line ? range.start.character : 0
    let ec = i == range.end.line ? range.end.character : curr.length
    if (sc == ec) continue
    splited.push(Range.create(i, sc, i, ec))
  }
  return splited
}

/**
 * Get ranges of visual block
 */
export function getVisualRanges(doc: Document, range: Range): Range[] {
  let { start, end } = getWellformedRange(range)
  let sc = start.character < end.character ? start.character : end.character
  let ec = start.character < end.character ? end.character : start.character
  let ranges: Range[] = []
  for (let i = start.line; i <= end.line; i++) {
    let line = doc.getline(i)
    ranges.push(Range.create(i, sc, i, Math.min(line.length, ec)))
  }
  return ranges
}

export function isSurrondChange(change: TextChange | SurrondChange): change is SurrondChange {
  return Array.isArray(change['prepend']) && Array.isArray(change['append'])
}

export function isTextChange(change: TextChange | SurrondChange): change is TextChange {
  return typeof change['offset'] === 'number' && typeof change['remove'] === 'number'
}

export function getDelta(change: TextChange | SurrondChange): number {
  if (isSurrondChange(change)) {
    return change.append[1].length + change.prepend[1].length - change.append[0] - change.prepend[0]
  }
  return change.insert.length - change.remove
}

export function getChange(r: TextRange, range: Range, newText: string): TextChange | SurrondChange {
  let text = r.text
  if (equals(r.range, range)) {
    // surrond
    let idx = text.indexOf(newText)
    if (idx !== -1) {
      let prepend: [number, string] = [idx, '']
      let append: [number, string] = [text.length - newText.length - idx, '']
      return { prepend, append }
    }
    idx = newText.indexOf(text)
    if (idx !== -1) {
      let prepend: [number, string] = [0, newText.slice(0, idx)]
      let append: [number, string] = [0, newText.slice(- (newText.length - text.length - idx))]
      return { prepend, append }
    }
  }
  if (equals(r.range.end, range.end)) {
    // end change
    let remove = range.end.character - range.start.character
    return { offset: remove, remove, insert: newText, fromEnd: true }
  }
  let remove = range.end.character - range.start.character
  let offset = range.start.character - r.range.start.character
  return { offset, remove, insert: newText }
}

export function getBeforeCount(textRange: TextRange, ranges: TextRange[], exclude?: TextRange): number {
  let n = 0
  for (let idx = 0; idx < ranges.length; idx++) {
    const r = ranges[idx]
    if (r.position.line < textRange.position.line || r === exclude) continue
    if (r.isBefore(textRange)) {
      n++
      continue
    }
    break
  }
  return n
}
