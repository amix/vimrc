'use strict'
import { Position, Range } from 'vscode-languageserver-protocol'

export function rangeInRange(r: Range, range: Range): boolean {
  return positionInRange(r.start, range) === 0 && positionInRange(r.end, range) === 0
}

export function samePosition(one: Position, two: Position): boolean {
  return one.line === two.line && one.character === two.character
}

/**
 * Convert to well formed range
 */
export function toValidRange(range: Range, max?: Position): Range {
  let { start, end } = range
  if (start.line > end.line || (start.line === end.line && start.character > end.character)) {
    let m = start
    start = end
    end = m
  }
  start = Position.create(Math.max(0, start.line), Math.max(0, start.character))
  end = Position.create(Math.max(0, end.line), Math.max(0, end.character))
  return { start, end }
}

export function rangeAdjacent(r: Range, range: Range): boolean {
  if (comparePosition(r.end, range.start) == 0) {
    return true
  }
  if (comparePosition(range.end, r.start) == 0) {
    return true
  }
  return false
}

/**
 * Check if two ranges have overlap character.
 */
export function rangeOverlap(r: Range, range: Range): boolean {
  let { start, end } = r
  if (comparePosition(end, range.start) <= 0) {
    return false
  }
  if (comparePosition(start, range.end) >= 0) {
    return false
  }
  return true
}

/**
 * Check if two ranges have overlap or nested
 */
export function rangeIntersect(r: Range, range: Range): boolean {
  if (positionInRange(r.start, range) == 0) {
    return true
  }
  if (positionInRange(r.end, range) == 0) {
    return true
  }
  if (rangeInRange(range, r)) {
    return true
  }
  return false
}

/**
 * Adjust from start position
 */
export function adjustRangePosition(range: Range, position: Position): Range {
  let { line, character } = position
  let { start, end } = range
  let endCharacter = end.line == start.line ? end.character + character : end.character
  return Range.create(start.line + line, character + start.character, end.line + line, endCharacter)
}

export function lineInRange(line: number, range: Range): boolean {
  let { start, end } = range
  return line >= start.line && line <= end.line
}

export function emptyRange(range: Range): boolean {
  let { start, end } = range
  return start.line == end.line && start.character == end.character
}

export function positionInRange(position: Position, range: Range): number {
  let { start, end } = range
  if (comparePosition(position, start) < 0) return -1
  if (comparePosition(position, end) > 0) return 1
  return 0
}

export function comparePosition(position: Position, other: Position): number {
  if (position.line > other.line) return 1
  if (other.line == position.line && position.character > other.character) return 1
  if (other.line == position.line && position.character == other.character) return 0
  return -1
}

export function isSingleLine(range: Range): boolean {
  return range.start.line == range.end.line
}

/*
 * Get end position by content
 */
export function getEnd(start: Position, content: string): Position {
  const lines = content.split(/\r?\n/)
  const len = lines.length
  const lastLine = lines[len - 1]
  const end = len == 1 ? start.character + content.length : lastLine.length
  return Position.create(start.line + len - 1, end)
}
