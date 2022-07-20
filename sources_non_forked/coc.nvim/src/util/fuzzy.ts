'use strict'

export function getCharCodes(str: string): number[] {
  let res = []
  for (let i = 0, l = str.length; i < l; i++) {
    res.push(str.charCodeAt(i))
  }
  return res
}

export function wordChar(ch: number): boolean {
  return (ch >= 97 && ch <= 122) || (ch >= 65 && ch <= 90)
}

export function caseMatch(input: number, code: number, ignorecase = false): boolean {
  if (input == code) return true
  if (input >= 97 && input <= 122 && code + 32 === input) return true
  if (ignorecase && input <= 90 && input + 32 === code) return true
  return false
}

export function fuzzyChar(a: string, b: string): boolean {
  let ca = a.charCodeAt(0)
  let cb = b.charCodeAt(0)
  if (ca === cb) return true
  if (ca >= 97 && ca <= 122 && cb + 32 === ca) return true
  return false
}

// upper case must match, lower case ignore case
export function fuzzyMatch(needle: number[], text: string): boolean {
  let totalCount = needle.length
  if (needle.length > text.length) return false
  let i = 0
  for (let j = 0; j < text.length; j++) {
    if (i === totalCount) break
    let code = text.charCodeAt(j)
    let m = needle[i]
    if (code === m) {
      i = i + 1
      continue
    }
    // upper case match lower case
    if ((m >= 97 && m <= 122) && code + 32 === m) {
      i = i + 1
      continue
    }
  }
  return i === totalCount
}
