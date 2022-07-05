'use strict'
import { sep as pathSeparator } from 'path'
import { getCharCodes, fuzzyMatch, fuzzyChar, caseMatch } from './fuzzy'

export interface MatchResult {
  score: number
  matches?: number[] // character indexes
}

// first is start or path start +1, fuzzy +0.5
// next is followed of path start +1, fuzzy +0.5
// filename startsWith +1, fuzzy +0.5

export function getMatchResult(text: string, query: string, filename = ''): MatchResult {
  if (!text) return { score: 0 }
  if (!query) return { score: 1 }
  let matches: number[] = []
  let codes = getCharCodes(query)
  let filenameIdx = filename ? text.indexOf(filename) : -1
  let matchBase = filenameIdx != -1 && fuzzyMatch(codes, filename)
  let score = 0
  let c = query[0]
  let idx = 0
  let first = text[0]
  // base => start => pathSeparator => fuzzy
  if (matchBase) {
    if (filename.startsWith(c)) {
      score = score + 2
      idx = filenameIdx + 1
      matches.push(filenameIdx)
    } else if (filename[0].toLowerCase() == c) {
      score = score + 1.5
      idx = filenameIdx + 1
      matches.push(filenameIdx)
    } else {
      for (let i = 1; i < filename.length; i++) {
        if (fuzzyChar(c, filename[i])) {
          score = score + 1
          idx = filenameIdx + i + 1
          matches.push(filenameIdx + i)
          break
        }
      }
    }
  } else if (first.toLowerCase() === c.toLowerCase()) {
    score = score + (first == c ? 1 : 0.5)
    matches.push(0)
    idx = 1
  } else {
    for (let i = 1; i < text.length; i++) {
      let pre = text[i - 1]
      if (pre == pathSeparator && text[i] == c) {
        score = score + 1
        matches.push(i)
        idx = i + 1
        break
      }
    }
    if (idx == 0) {
      for (let i = 0; i < text.length; i++) {
        if (fuzzyChar(c.toLowerCase(), text[i])) {
          score = score + (c === text[i] ? 0.5 : 0.3)
          matches.push(i)
          idx = i + 1
          break
        }
      }
    }
  }
  if (idx == 0) return { score: 0 }
  if (codes.length == 1) return { score, matches }
  return nextResult(codes.slice(1), text, idx, { score, matches })
}

/**
 *
 * @public
 * @param {number[]} codes - remain codes
 * @param {string} text - total text
 * @param {number} idx - start index of text
 * @param {MatchResult} curr - current result
 * @returns {MatchResult | null}
 */
function nextResult(codes: number[], text: string, idx: number, curr: MatchResult): MatchResult | null {
  let { score, matches } = curr
  let results: MatchResult[] = []
  let c = codes[0]
  let remain = codes.slice(1)
  let result: MatchResult

  function getRemainResult(index: number): void {
    if (!result) return
    if (remain.length == 0) {
      results.push(result)
    } else if (result) {
      let res = nextResult(remain, text, index, result)
      if (res) results.push(res)
    }
  }
  let followed = idx < text.length ? text[idx].charCodeAt(0) : null
  if (!followed) return null
  if (followed == c) {
    result = { score: score + 1, matches: matches.concat([idx]) }
    getRemainResult(idx + 1)
  } else if (caseMatch(c, followed, true)) {
    result = { score: score + 0.5, matches: matches.concat([idx]) }
    getRemainResult(idx + 1)
  }
  if (idx + 1 < text.length) {
    // follow path
    for (let i = idx + 1; i < text.length; i++) {
      let ch = text[i].charCodeAt(0)
      if (text[i - 1] == pathSeparator && caseMatch(c, ch, true)) {
        let add = c == ch ? 1 : 0.5
        result = { score: score + add, matches: matches.concat([i]) }
        getRemainResult(i + 1)
        break
      }
    }
    // next fuzzy
    for (let i = idx + 1; i < text.length; i++) {
      let ch = text[i].charCodeAt(0)
      if (caseMatch(c, ch, true)) {
        let add = c == ch ? 0.5 : 0.2
        result = { score: score + add, matches: matches.concat([i]) }
        getRemainResult(i + 1)
        break
      }
    }
  }
  return results.length ? bestResult(results) : null
}

function bestResult(results: MatchResult[]): MatchResult {
  let res = results[0]
  for (let i = 1; i < results.length; i++) {
    if (results[i].score > res.score) {
      res = results[i]
    }
  }
  return res
}
