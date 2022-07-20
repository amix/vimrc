'use strict'
import { Range } from 'vscode-languageserver-protocol'

export function rangeParts(text: string, range: Range): [string, string] {
  let { start, end } = range
  let lines = text.split(/\r?\n/)
  let before = ''
  let after = ''
  let len = lines.length
  // get start and end parts
  for (let i = 0; i < len; i++) {
    let curr = lines[i]
    if (i < start.line) {
      before += curr + '\n'
      continue
    }
    if (i > end.line) {
      after += curr + (i == len - 1 ? '' : '\n')
      continue
    }
    if (i == start.line) {
      before += curr.slice(0, start.character)
    }
    if (i == end.line) {
      after += curr.slice(end.character) + (i == len - 1 ? '' : '\n')
    }
  }
  return [before, after]
}

// lowerCase 1, upperCase 2
export function getCase(code: number): number {
  if (code >= 97 && code <= 122) return 1
  if (code >= 65 && code <= 90) return 2
  return 0
}

export function getNextWord(codes: ReadonlyArray<number>, index: number): [number, number] | undefined {
  let preCase = index == 0 ? 0 : getCase(codes[index - 1])
  for (let i = index; i < codes.length; i++) {
    let curr = getCase(codes[i])
    if (curr > 0 && curr != preCase) {
      return [i, codes[i]]
    }
    preCase = curr
  }
  return undefined
}

export function getCharIndexes(input: string, character: string): number[] {
  let res: number[] = []
  for (let i = 0; i < input.length; i++) {
    if (input[i] == character) res.push(i)
  }
  return res
}

// nvim use utf8
export function byteLength(str: string): number {
  return Buffer.byteLength(str)
}

export function upperFirst(str: string): string {
  return str?.length > 0 ? str[0].toUpperCase() + str.slice(1) : ''
}

export function byteIndex(content: string, index: number): number {
  let s = content.slice(0, index)
  return Buffer.byteLength(s)
}

export function indexOf(str: string, ch: string, count = 1): number {
  let curr = 0
  for (let i = 0; i < str.length; i++) {
    if (str[i] == ch) {
      curr = curr + 1
      if (curr == count) {
        return i
      }
    }
  }
  return -1
}

export function characterIndex(content: string, byteIndex: number): number {
  let buf = Buffer.from(content, 'utf8')
  return buf.slice(0, byteIndex).toString('utf8').length
}

export function byteSlice(content: string, start: number, end?: number): string {
  let buf = Buffer.from(content, 'utf8')
  return buf.slice(start, end).toString('utf8')
}

export function isWord(character: string): boolean {
  let code = character.charCodeAt(0)
  if (code > 128) return false
  if (code == 95) return true
  if (code >= 48 && code <= 57) return true
  if (isAlphabet(code)) return true
  return false
}

export function isAlphabet(code: number): boolean {
  if (code >= 65 && code <= 90) return true
  if (code >= 97 && code <= 122) return true
  return false
}

function doEqualsIgnoreCase(a: string, b: string, stopAt = a.length): boolean {
  if (typeof a !== 'string' || typeof b !== 'string') {
    return false
  }
  for (let i = 0; i < stopAt; i++) {
    const codeA = a.charCodeAt(i)
    const codeB = b.charCodeAt(i)
    if (codeA === codeB) {
      continue
    }
    // a-z A-Z
    if (isAlphabet(codeA) && isAlphabet(codeB)) {
      const diff = Math.abs(codeA - codeB)
      if (diff !== 0 && diff !== 32) {
        return false
      }
    }
    // Any other charcode
    else {
      if (String.fromCharCode(codeA).toLowerCase() !== String.fromCharCode(codeB).toLowerCase()) {
        return false
      }
    }
  }
  return true
}

export function equalsIgnoreCase(a: string, b: string): boolean {
  const len1 = a ? a.length : 0
  const len2 = b ? b.length : 0
  if (len1 !== len2) return false
  return doEqualsIgnoreCase(a, b)
}

export function contentToLines(content: string, eol: boolean): string[] {
  if (eol && content.endsWith('\n')) {
    return content.slice(0, -1).split('\n')
  }
  return content.split('\n')
}
