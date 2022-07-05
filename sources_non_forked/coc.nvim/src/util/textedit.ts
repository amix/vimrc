'use strict'
import { AnnotatedTextEdit, ChangeAnnotation, Position, Range, TextDocumentEdit, TextEdit, WorkspaceEdit } from 'vscode-languageserver-protocol'
import { LinesTextDocument } from '../model/textdocument'
import { DocumentChange } from '../types'
import { comparePosition, emptyRange, samePosition, toValidRange } from './position'
import { byteLength, contentToLines } from './string'

export type TextChangeItem = [string[], number, number, number, number]

export function singleLineEdit(edit: TextEdit): boolean {
  let { range, newText } = edit
  return range.start.line == range.end.line && newText.indexOf('\n') == -1
}

export function lineCountChange(edit: TextEdit): number {
  let { newText } = edit
  let range = getWellformedRange(edit.range)
  let n = range.end.line - range.start.line
  return newText.split(/\r?\n/).length - n - 1
}

export function getWellformedRange(range: Range): Range {
  const start = range.start
  const end = range.end
  if (start.line > end.line || (start.line === end.line && start.character > end.character)) {
    return { start: end, end: start }
  }
  return range
}

export function getWellformedEdit(textEdit: TextEdit) {
  const range = getWellformedRange(textEdit.range)
  if (range !== textEdit.range) {
    return { newText: textEdit.newText, range }
  }
  return textEdit
}

export function mergeSort<T>(data: T[], compare: (a: T, b: T) => number): T[] {
  if (data.length <= 1) {
    // sorted
    return data
  }
  const p = (data.length / 2) | 0
  const left = data.slice(0, p)
  const right = data.slice(p)
  mergeSort(left, compare)
  mergeSort(right, compare)
  let leftIdx = 0
  let rightIdx = 0
  let i = 0
  while (leftIdx < left.length && rightIdx < right.length) {
    let ret = compare(left[leftIdx], right[rightIdx])
    if (ret <= 0) {
      // smaller_equal -> take left to preserve order
      data[i++] = left[leftIdx++]
    } else {
      // greater -> take right
      data[i++] = right[rightIdx++]
    }
  }
  while (leftIdx < left.length) {
    data[i++] = left[leftIdx++]
  }
  while (rightIdx < right.length) {
    data[i++] = right[rightIdx++]
  }
  return data
}

export function emptyTextEdit(edit: TextEdit): boolean {
  return emptyRange(edit.range) && edit.newText.length === 0
}

export function emptyWorkspaceEdit(edit: WorkspaceEdit): boolean {
  let { changes, documentChanges } = edit
  if (documentChanges && documentChanges.length) return false
  if (changes && Object.keys(changes).length) return false
  return true
}

export function getConfirmAnnotations(changes: ReadonlyArray<DocumentChange>, changeAnnotations: { [id: string]: ChangeAnnotation }): ReadonlyArray<string> {
  let keys: string[] = []
  for (let change of changes) {
    let key = getAnnotationKey(change)
    if (key && !keys.includes(key) && changeAnnotations[key]?.needsConfirmation) keys.push(key)
  }
  return keys
}

export function getAnnotationKey(change: DocumentChange): string | undefined {
  let key: string
  if (TextDocumentEdit.is(change)) {
    if (AnnotatedTextEdit.is(change.edits[0])) {
      key = change.edits[0].annotationId
    }
  } else {
    key = change.annotationId
  }
  return key
}

export function toDocumentChanges(edit: WorkspaceEdit): DocumentChange[] {
  if (edit.documentChanges) return edit.documentChanges
  let changes: DocumentChange[] = []
  if (edit.changes) {
    for (let [uri, edits] of Object.entries(edit.changes)) {
      changes.push({ textDocument: { uri, version: null }, edits })
    }
  }
  return changes
}

/**
 * Filter unnessary edits and fix edits.
 */
export function filterSortEdits(textDocument: LinesTextDocument, edits: TextEdit[]): TextEdit[] {
  let res: TextEdit[] = []
  let end = textDocument.end
  let checkEnd = end.line > 0 && end.character == 0
  let prevDelete: Position | undefined
  for (let i = 0; i < edits.length; i++) {
    let edit = edits[i]
    let { newText } = edit
    let range = toValidRange(edit.range)
    if (prevDelete) {
      // merge possible delete, insert edits.
      if (samePosition(prevDelete, range.start) && emptyRange(range) && newText.length > 0) {
        let last = res[res.length - 1]
        last.newText = newText
        prevDelete = undefined
        continue
      }
      prevDelete = undefined
    }
    if (newText.includes('\r')) newText = newText.replace(/\r\n/g, '\n')
    let d = comparePosition(range.end, end)
    if (d > 0) range.end = { line: end.line, character: end.character }
    if (textDocument.getText(range) !== newText) {
      // Adjust textEdit to make it acceptable by nvim_buf_set_text
      if (d === 0 && checkEnd && !emptyRange(range) && newText.endsWith('\n')) {
        newText = newText.slice(0, -1)
        let text = textDocument.lines[end.line - 1]
        range.end = Position.create(end.line - 1, text.length)
      } else if (newText.length == 0) {
        prevDelete = range.start
      }
      res.push({ range, newText })
    }
  }
  return mergeSort(res, (a, b) => {
    let diff = a.range.start.line - b.range.start.line
    if (diff === 0) {
      return a.range.start.character - b.range.start.character
    }
    return diff
  })
}

/**
 * Apply valid & sorted edits
 */
export function applyEdits(document: LinesTextDocument, edits: TextEdit[]): string[] | undefined {
  if (edits.length == 1) {
    let { start, end } = edits[0].range
    let { lines } = document
    let sl = lines[start.line] ?? ''
    let el = lines[end.line] ?? ''
    let content = sl.substring(0, start.character) + edits[0].newText + el.substring(end.character)
    if (end.line >= lines.length && document.eol) {
      if (content == '') return [...lines.slice(0, start.line)]
      if (content.endsWith('\n')) content = content.slice(0, -1)
      return [...lines.slice(0, start.line), ...content.split('\n')]
    }
    return [...lines.slice(0, start.line), ...content.split('\n'), ...lines.slice(end.line + 1)]
  }
  let text = document.getText()
  let lastModifiedOffset = 0
  const spans = []
  for (const e of edits) {
    let startOffset = document.offsetAt(e.range.start)
    if (startOffset < lastModifiedOffset) {
      throw new Error('Overlapping edit')
    }
    else if (startOffset > lastModifiedOffset) {
      spans.push(text.substring(lastModifiedOffset, startOffset))
    }
    if (e.newText.length) {
      spans.push(e.newText)
    }
    lastModifiedOffset = document.offsetAt(e.range.end)
  }
  spans.push(text.substring(lastModifiedOffset))
  let result = spans.join('')
  if (result === text) return undefined
  return contentToLines(result, document.eol)
}

export function toTextChanges(lines: ReadonlyArray<string>, edits: TextEdit[]): TextChangeItem[] {
  return edits.map(o => {
    let { start, end } = o.range
    let sl = lines[start.line] ?? ''
    let sc = byteLength(sl.slice(0, start.character))
    let el = end.line == start.line ? sl : lines[end.line] ?? ''
    let ec = byteLength(el.slice(0, end.character))
    let { newText } = o
    return [newText.length > 0 ? newText.split('\n') : [], start.line, sc, end.line, ec]
  })
}

export function getChangedPosition(start: Position, edit: TextEdit): { line: number; character: number } {
  let { range, newText } = edit
  if (comparePosition(range.end, start) <= 0) {
    let lines = newText.split('\n')
    let lineCount = lines.length - (range.end.line - range.start.line) - 1
    let character = start.character
    if (range.end.line == start.line) {
      let last = lines[lines.length - 1].length
      if (lines.length > 1) {
        character = last + character - range.end.character
      } else {
        character = range.start.character + last + character - range.end.character
      }
    }
    return { line: lineCount, character: character - start.character }
  }
  return { line: 0, character: 0 }
}

export function getPosition(start: Position, edit: TextEdit): Position {
  let { line, character } = start
  let { range, newText } = edit
  let { end } = range
  let lines = newText.split('\n')
  let lineCount = lines.length - (end.line - range.start.line) - 1
  let c = range.end.line - start.line
  if (c > 0) return { line, character }
  if (c < 0) return { line: line + lineCount, character }
  if (lines.length > 1) {
    let last = lines[lines.length - 1].length
    return { line: line + lineCount, character: last + character - end.character }
  }
  let d = range.start.character - range.end.character
  return { line: line + lineCount, character: d + newText.length + character }
}

/**
 * Get new position from sorted edits
 */
export function getPositionFromEdits(start: Position, edits: TextEdit[]): Position {
  let position = Position.create(start.line, start.character)
  let before = false
  for (let i = edits.length - 1; i >= 0; i--) {
    let edit = edits[i]
    if (before) {
      position.line += lineCountChange(edit)
      continue
    }
    let d = comparePosition(edit.range.end, position)
    if (d > 0) continue
    if (edit.range.end.line == position.line) {
      position = getPosition(position, edit)
    } else {
      before = true
      position.line += lineCountChange(edit)
    }
  }
  return position
}

export function getChangedLineCount(start: Position, edits: TextEdit[]): number {
  let total = 0
  for (let edit of edits) {
    let r = getWellformedRange(edit.range)
    if (comparePosition(r.end, start) <= 0) {
      total += lineCountChange(edit)
    }
  }
  return total
}

/**
 * Merge sorted edits to single textedit
 */
export function mergeTextEdits(edits: TextEdit[], oldLines: ReadonlyArray<string>, newLines: ReadonlyArray<string>): TextEdit {
  let start = edits[0].range.start
  let end = edits[edits.length - 1].range.end
  let lr = oldLines.length - end.line
  let cr = (oldLines[end.line] ?? '').length - end.character
  let line = newLines.length - lr
  let character = (newLines[line] ?? '').length - cr
  let newText = getText(start, Position.create(line, character), newLines)
  return TextEdit.replace(Range.create(start, end), newText)
}

function getText(start: Position, end: Position, lines: ReadonlyArray<string>): string {
  if (start.line === end.line) {
    return (lines[start.line] ?? '').slice(start.character, end.character)
  }
  let spans: string[] = []
  for (let i = start.line; i <= end.line; i++) {
    let s = lines[i] ?? ''
    if (i === start.line) {
      spans.push(s.slice(start.character))
    } else if (i === end.line) {
      spans.push(s.slice(0, end.character))
    } else {
      spans.push(s)
    }
  }
  return spans.join('\n')
}
