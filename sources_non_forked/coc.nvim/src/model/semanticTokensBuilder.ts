'use strict'
import { Range, SemanticTokens, SemanticTokensLegend } from "vscode-languageserver-protocol"

function isStringArray(value: any): value is string[] {
  return Array.isArray(value) && (value as any[]).every(elem => typeof elem === 'string')
}

function isStrArrayOrUndefined(arg: any): arg is string[] | undefined {
  return ((typeof arg === 'undefined') || isStringArray(arg))
}

/**
 * A semantic tokens builder can help with creating a `SemanticTokens` instance
 * which contains delta encoded semantic tokens.
 */
export class SemanticTokensBuilder {
  private _prevLine: number
  private _prevChar: number
  private _dataIsSortedAndDeltaEncoded: boolean
  private _data: number[]
  private _dataLen: number
  private _tokenTypeStrToInt: Map<string, number>
  private _tokenModifierStrToInt: Map<string, number>
  private _hasLegend: boolean

  constructor(legend?: SemanticTokensLegend) {
    this._prevLine = 0
    this._prevChar = 0
    this._dataIsSortedAndDeltaEncoded = true
    this._data = []
    this._dataLen = 0
    this._tokenTypeStrToInt = new Map<string, number>()
    this._tokenModifierStrToInt = new Map<string, number>()
    this._hasLegend = false
    if (legend) {
      this._hasLegend = true
      for (let i = 0, len = legend.tokenTypes.length; i < len; i++) {
        this._tokenTypeStrToInt.set(legend.tokenTypes[i], i)
      }
      for (let i = 0, len = legend.tokenModifiers.length; i < len; i++) {
        this._tokenModifierStrToInt.set(legend.tokenModifiers[i], i)
      }
    }
  }

  /**
   * Add another token.
   *
   * @param line The token start line number (absolute value).
   * @param char The token start character (absolute value).
   * @param length The token length in characters.
   * @param tokenType The encoded token type.
   * @param tokenModifiers The encoded token modifiers.
   */
  public push(line: number, char: number, length: number, tokenType: number, tokenModifiers?: number): void
  /**
   * Add another token. Use only when providing a legend.
   *
   * @param range The range of the token. Must be single-line.
   * @param tokenType The token type.
   * @param tokenModifiers The token modifiers.
   */
  public push(range: Range, tokenType: string, tokenModifiers?: string[]): void
  public push(arg0: any, arg1: any, arg2: any, arg3?: any, arg4?: any): void {
    if (typeof arg0 === 'number' && typeof arg1 === 'number' && typeof arg2 === 'number' && typeof arg3 === 'number' && (typeof arg4 === 'number' || typeof arg4 === 'undefined')) {
      if (typeof arg4 === 'undefined') {
        arg4 = 0
      }
      // 1st overload
      return this._pushEncoded(arg0, arg1, arg2, arg3, arg4)
    }
    if (Range.is(arg0) && typeof arg1 === 'string' && isStrArrayOrUndefined(arg2)) {
      // 2nd overload
      return this._push(arg0, arg1, arg2)
    }
    throw new Error('Illegal argument')
  }

  private _push(range: Range, tokenType: string, tokenModifiers?: string[]): void {
    if (!this._hasLegend) {
      throw new Error('Legend must be provided in constructor')
    }
    if (range.start.line !== range.end.line) {
      throw new Error('`range` cannot span multiple lines')
    }
    if (!this._tokenTypeStrToInt.has(tokenType)) {
      throw new Error('`tokenType` is not in the provided legend')
    }
    const line = range.start.line
    const char = range.start.character
    const length = range.end.character - range.start.character
    const nTokenType = this._tokenTypeStrToInt.get(tokenType)!
    let nTokenModifiers = 0
    if (tokenModifiers) {
      for (const tokenModifier of tokenModifiers) {
        if (!this._tokenModifierStrToInt.has(tokenModifier)) {
          throw new Error('`tokenModifier` is not in the provided legend')
        }
        const nTokenModifier = this._tokenModifierStrToInt.get(tokenModifier)!
        nTokenModifiers |= (1 << nTokenModifier) >>> 0
      }
    }
    this._pushEncoded(line, char, length, nTokenType, nTokenModifiers)
  }

  private _pushEncoded(line: number, char: number, length: number, tokenType: number, tokenModifiers: number): void {
    if (this._dataIsSortedAndDeltaEncoded && (line < this._prevLine || (line === this._prevLine && char < this._prevChar))) {
      // push calls were ordered and are no longer ordered
      this._dataIsSortedAndDeltaEncoded = false

      // Remove delta encoding from data
      const tokenCount = (this._data.length / 5) | 0
      let prevLine = 0
      let prevChar = 0
      for (let i = 0; i < tokenCount; i++) {
        let line = this._data[5 * i]
        let char = this._data[5 * i + 1]

        if (line === 0) {
          // on the same line as previous token
          line = prevLine
          char += prevChar
        } else {
          // on a different line than previous token
          line += prevLine
        }

        this._data[5 * i] = line
        this._data[5 * i + 1] = char

        prevLine = line
        prevChar = char
      }
    }

    let pushLine = line
    let pushChar = char
    if (this._dataIsSortedAndDeltaEncoded && this._dataLen > 0) {
      pushLine -= this._prevLine
      if (pushLine === 0) {
        pushChar -= this._prevChar
      }
    }

    this._data[this._dataLen++] = pushLine
    this._data[this._dataLen++] = pushChar
    this._data[this._dataLen++] = length
    this._data[this._dataLen++] = tokenType
    this._data[this._dataLen++] = tokenModifiers

    this._prevLine = line
    this._prevChar = char
  }

  private static _sortAndDeltaEncode(data: number[]): number[] {
    let pos: number[] = []
    const tokenCount = (data.length / 5) | 0
    for (let i = 0; i < tokenCount; i++) {
      pos[i] = i
    }
    pos.sort((a, b) => {
      const aLine = data[5 * a]
      const bLine = data[5 * b]
      if (aLine === bLine) {
        const aChar = data[5 * a + 1]
        const bChar = data[5 * b + 1]
        return aChar - bChar
      }
      return aLine - bLine
    })
    const result = new Array<number>(data.length)
    let prevLine = 0
    let prevChar = 0
    for (let i = 0; i < tokenCount; i++) {
      const srcOffset = 5 * pos[i]
      const line = data[srcOffset + 0]
      const char = data[srcOffset + 1]
      const length = data[srcOffset + 2]
      const tokenType = data[srcOffset + 3]
      const tokenModifiers = data[srcOffset + 4]

      const pushLine = line - prevLine
      const pushChar = (pushLine === 0 ? char - prevChar : char)

      const dstOffset = 5 * i
      result[dstOffset + 0] = pushLine
      result[dstOffset + 1] = pushChar
      result[dstOffset + 2] = length
      result[dstOffset + 3] = tokenType
      result[dstOffset + 4] = tokenModifiers

      prevLine = line
      prevChar = char
    }

    return result
  }

  /**
   * Finish and create a `SemanticTokens` instance.
   */
  public build(resultId?: string): SemanticTokens {
    if (!this._dataIsSortedAndDeltaEncoded) {
      return { data: SemanticTokensBuilder._sortAndDeltaEncode(this._data), resultId }
    }
    return { data: this._data, resultId }
  }
}
