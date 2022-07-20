'use strict'
/* ---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

import { Neovim } from '@chemzqm/neovim'
import unidecode from 'unidecode'
import { groupBy } from '../util/array'
import { CharCode } from '../util/charCode'
import { getCharIndexes } from '../util/string'
import { convertRegex, evalCode, EvalKind, executePythonCode, getVariablesCode, prepareMatchCode, UltiSnippetContext } from './eval'
const logger = require('../util/logger')('snippets-parser')

const knownRegexOptions = ['d', 'g', 'i', 'm', 's', 'u', 'y']
export const enum TokenType {
  Dollar,
  Colon,
  Comma,
  CurlyOpen,
  CurlyClose,
  Backslash,
  Forwardslash,
  Pipe,
  Int,
  VariableName,
  Format,
  Plus,
  Dash,
  QuestionMark,
  EOF,
  OpenParen,
  CloseParen,
  BackTick,
  ExclamationMark,
}

export interface Token {
  type: TokenType
  pos: number
  len: number
}

export class Scanner {

  private static _table: { [ch: number]: TokenType } = {
    [CharCode.DollarSign]: TokenType.Dollar,
    [CharCode.Colon]: TokenType.Colon,
    [CharCode.Comma]: TokenType.Comma,
    [CharCode.OpenCurlyBrace]: TokenType.CurlyOpen,
    [CharCode.CloseCurlyBrace]: TokenType.CurlyClose,
    [CharCode.Backslash]: TokenType.Backslash,
    [CharCode.Slash]: TokenType.Forwardslash,
    [CharCode.Pipe]: TokenType.Pipe,
    [CharCode.Plus]: TokenType.Plus,
    [CharCode.Dash]: TokenType.Dash,
    [CharCode.QuestionMark]: TokenType.QuestionMark,
    [CharCode.OpenParen]: TokenType.OpenParen,
    [CharCode.CloseParen]: TokenType.CloseParen,
    [CharCode.BackTick]: TokenType.BackTick,
    [CharCode.ExclamationMark]: TokenType.ExclamationMark,
  }

  public static isDigitCharacter(ch: number): boolean {
    return ch >= CharCode.Digit0 && ch <= CharCode.Digit9
  }

  public static isVariableCharacter(ch: number): boolean {
    return ch === CharCode.Underline
      || (ch >= CharCode.a && ch <= CharCode.z)
      || (ch >= CharCode.A && ch <= CharCode.Z)
  }

  public value: string
  public pos: number

  constructor() {
    this.text('')
  }

  public text(value: string): void {
    this.value = value
    this.pos = 0
  }

  public tokenText(token: Token): string {
    return this.value.substr(token.pos, token.len)
  }

  public next(): Token {

    if (this.pos >= this.value.length) {
      return { type: TokenType.EOF, pos: this.pos, len: 0 }
    }

    let pos = this.pos
    let len = 0
    let ch = this.value.charCodeAt(pos)
    let type: TokenType

    // static types
    type = Scanner._table[ch]
    if (typeof type === 'number') {
      this.pos += 1
      return { type, pos, len: 1 }
    }

    // number
    if (Scanner.isDigitCharacter(ch)) {
      type = TokenType.Int
      do {
        len += 1
        ch = this.value.charCodeAt(pos + len)
      } while (Scanner.isDigitCharacter(ch))

      this.pos += len
      return { type, pos, len }
    }

    // variable name
    if (Scanner.isVariableCharacter(ch)) {
      type = TokenType.VariableName
      do {
        ch = this.value.charCodeAt(pos + (++len))
      } while (Scanner.isVariableCharacter(ch) || Scanner.isDigitCharacter(ch))

      this.pos += len
      return { type, pos, len }
    }

    // format
    type = TokenType.Format
    do {
      len += 1
      ch = this.value.charCodeAt(pos + len)
    } while (
      !isNaN(ch)
      && typeof Scanner._table[ch] === 'undefined' // not static token
      && !Scanner.isDigitCharacter(ch) // not number
      && !Scanner.isVariableCharacter(ch) // not variable
    )

    this.pos += len
    return { type, pos, len }
  }
}

export abstract class Marker {

  public readonly _markerBrand: any

  public parent: Marker
  protected _children: Marker[] = []

  public appendChild(child: Marker): this {
    if (child instanceof Text && this._children[this._children.length - 1] instanceof Text) {
      // this and previous child are text -> merge them
      (this._children[this._children.length - 1] as Text).value += child.value
    } else {
      // normal adoption of child
      child.parent = this
      this._children.push(child)
    }
    return this
  }

  public setOnlyChild(child: Marker): void {
    child.parent = this
    this._children = [child]
  }

  public replaceChildren(children: Marker[]): void {
    for (const child of children) {
      child.parent = this
    }
    this._children = children
  }

  public get children(): Marker[] {
    return this._children
  }

  public get snippet(): TextmateSnippet | undefined {
    // eslint-disable-next-line @typescript-eslint/no-this-alias
    let candidate: Marker = this
    // eslint-disable-next-line no-constant-condition
    while (true) {
      if (!candidate) {
        return undefined
      }
      if (candidate instanceof TextmateSnippet) {
        return candidate
      }
      candidate = candidate.parent
    }
  }

  public toString(): string {
    return this.children.reduce((prev, cur) => prev + cur.toString(), '')
  }

  public abstract toTextmateString(): string

  public len(): number {
    return 0
  }

  public abstract clone(): Marker
}

export class Text extends Marker {

  public static escape(value: string): string {
    return value.replace(/\$|}|\\/g, '\\$&')
  }

  constructor(public value: string) {
    super()
  }
  public toString(): string {
    return this.value
  }

  public toTextmateString(): string {
    return Text.escape(this.value)
  }
  public len(): number {
    return this.value.length
  }
  public clone(): Text {
    return new Text(this.value)
  }
}

export class CodeBlock extends Marker {

  private _value = ''
  private _related: number[] = []

  constructor(public code: string, public readonly kind: EvalKind, value?: string) {
    super()
    if (kind === 'python') {
      let { _related } = this
      let arr
      let re = /\bt\[(\d+)\]/g
      // eslint-disable-next-line no-constant-condition
      while (true) {
        arr = re.exec(code)
        if (arr == null) break
        let n = parseInt(arr[1], 10)
        if (!_related.includes(n)) _related.push(n)
      }
    }
    if (value !== undefined) this._value = value
  }

  public get related(): number[] {
    return this._related
  }

  public update(map: Map<number, number>): void {
    if (this.kind !== 'python') return
    let related: Set<number> = new Set()
    this.code = this.code.replace(/\bt\[(\d+)\]/g, (_, p1) => {
      let idx = Number(p1)
      let id = map.has(idx) ? map.get(idx) : idx
      related.add(id)
      return `t[${id}]`
    })
    this._related = Array.from(related)
  }

  public get index(): number | undefined {
    if (this.parent instanceof Placeholder) {
      return this.parent.index
    }
    return undefined
  }

  public async resolve(nvim: Neovim): Promise<void> {
    if (!this.code.length) return
    let res = await evalCode(nvim, this.kind, this.code, this._value)
    if (res != null) this._value = res
  }

  public len(): number {
    return this._value.length
  }

  public toString(): string {
    return this._value
  }

  public get value(): string {
    return this._value
  }

  public toTextmateString(): string {
    let t = ''
    if (this.kind == 'python') {
      t = '!p '
    } else if (this.kind == 'shell') {
      t = ''
    } else if (this.kind == 'vim') {
      t = '!v '
    }
    return '`' + t + (this.code) + '`'
  }

  public clone(): CodeBlock {
    return new CodeBlock(this.code, this.kind, this.value)
  }
}

export abstract class TransformableMarker extends Marker {
  public transform: Transform
}

export class Placeholder extends TransformableMarker {
  public primary = false

  constructor(public index: number) {
    super()
  }

  public get isFinalTabstop(): boolean {
    return this.index === 0
  }

  public get choice(): Choice | undefined {
    return this._children.length === 1 && this._children[0] instanceof Choice
      ? this._children[0] as Choice
      : undefined
  }

  public toTextmateString(): string {
    let transformString = ''
    if (this.transform) {
      transformString = this.transform.toTextmateString()
    }
    if (this.children.length === 0 && !this.transform) {
      return `$${this.index}`
    } else if (this.children.length === 0) {
      return `\${${this.index}${transformString}}`
    } else if (this.choice) {
      return `\${${this.index}|${this.choice.toTextmateString()}|${transformString}}`
    } else {
      return `\${${this.index}:${this.children.map(child => child.toTextmateString()).join('')}${transformString}}`
    }
  }

  public clone(): Placeholder {
    let ret = new Placeholder(this.index)
    if (this.transform) {
      ret.transform = this.transform.clone()
    }
    ret._children = this.children.map(child => child.clone())
    return ret
  }
}

export class Choice extends Marker {

  public readonly options: Text[] = []

  public appendChild(marker: Marker): this {
    if (marker instanceof Text) {
      marker.parent = this
      this.options.push(marker)
    }
    return this
  }

  public toString(): string {
    return this.options[0].value
  }

  public toTextmateString(): string {
    return this.options
      .map(option => option.value.replace(/\||,/g, '\\$&'))
      .join(',')
  }

  public len(): number {
    return this.options[0].len()
  }

  public clone(): Choice {
    let ret = new Choice()
    for (let opt of this.options) {
      ret.appendChild(opt as any)
    }
    return ret
  }
}

export class Transform extends Marker {

  public regexp: RegExp
  public ascii = false
  public ultisnip = false

  public resolve(value: string): string {
    let didMatch = false
    let ret = value.replace(this.regexp, (...args) => {
      didMatch = true
      return this._replace(args.slice(0, -2))
    })
    // when the regex didn't match and when the transform has
    // else branches, then run those
    if (!didMatch && this._children.some(child => child instanceof FormatString && Boolean(child.elseValue))) {
      ret = this._replace([])
    }
    return ret
  }

  private _replace(groups: string[]): string {
    let ret = ''
    let backslashIndexes: number[] = []
    for (const marker of this._children) {
      if (marker instanceof FormatString) {
        let val = marker.resolve(groups[marker.index] || '')
        if (this.ultisnip && val.indexOf('\\') !== -1) {
          let s = ret.length
          backslashIndexes.push(...getCharIndexes(val, '\\').map(i => i + s))
        }
        ret += val
      } else if (marker instanceof ConditionString) {
        ret += marker.resolve(groups[marker.index])
      } else {
        ret += marker.toString()
      }
    }
    if (this.ascii) ret = unidecode(ret)
    return this.ultisnip ? transformEscapes(ret, backslashIndexes) : ret
  }

  public toString(): string {
    return ''
  }

  public toTextmateString(): string {
    return `/${this.regexp.source}/${this.children.map(c => c.toTextmateString())}/${(this.regexp.ignoreCase ? 'i' : '') + (this.regexp.global ? 'g' : '')}`
  }

  public clone(): Transform {
    let ret = new Transform()
    ret.regexp = new RegExp(this.regexp.source, '' + (this.regexp.ignoreCase ? 'i' : '') + (this.regexp.global ? 'g' : ''))
    ret._children = this.children.map(child => child.clone())
    return ret
  }

}

export class ConditionString extends Marker {
  constructor(
    public readonly index: number,
    public readonly ifValue: string,
    public readonly elseValue: string,
  ) {
    super()
  }

  public resolve(value: string): string {
    if (value) return this.ifValue
    return this.elseValue
  }

  public toTextmateString(): string {
    return '(?' + this.index + ':' + this.ifValue + (this.elseValue ? ':' + this.elseValue : '') + ')'
  }

  public clone(): ConditionString {
    return new ConditionString(this.index, this.ifValue, this.elseValue)
  }
}

export class FormatString extends Marker {

  constructor(
    public readonly index: number,
    public readonly shorthandName?: string,
    public readonly ifValue?: string,
    public readonly elseValue?: string,
  ) {
    super()
  }

  public resolve(value: string): string {
    if (this.shorthandName === 'upcase') {
      return !value ? '' : value.toLocaleUpperCase()
    } else if (this.shorthandName === 'downcase') {
      return !value ? '' : value.toLocaleLowerCase()
    } else if (this.shorthandName === 'capitalize') {
      return !value ? '' : (value[0].toLocaleUpperCase() + value.substr(1))
    } else if (this.shorthandName === 'pascalcase') {
      return !value ? '' : this._toPascalCase(value)
    } else if (Boolean(value) && typeof this.ifValue === 'string') {
      return this.ifValue
    } else if (!value && typeof this.elseValue === 'string') {
      return this.elseValue
    } else {
      return value || ''
    }
  }

  private _toPascalCase(value: string): string {
    const match = value.match(/[a-z]+/gi)
    if (!match) {
      return value
    }
    return match.map(word => word.charAt(0).toUpperCase()
      + word.substr(1).toLowerCase())
      .join('')
  }

  public toTextmateString(): string {
    let value = '${'
    value += this.index
    if (this.shorthandName) {
      value += `:/${this.shorthandName}`

    } else if (this.ifValue && this.elseValue) {
      value += `:?${this.ifValue}:${this.elseValue}`
    } else if (this.ifValue) {
      value += `:+${this.ifValue}`
    } else if (this.elseValue) {
      value += `:-${this.elseValue}`
    }
    value += '}'
    return value
  }

  public clone(): FormatString {
    let ret = new FormatString(this.index, this.shorthandName, this.ifValue, this.elseValue)
    return ret
  }
}

export class Variable extends TransformableMarker {
  private _resolved = false

  constructor(public name: string, resolved?: boolean) {
    super()
    if (typeof resolved === 'boolean') {
      this._resolved = resolved
    }
  }

  public get resolved(): boolean {
    return this._resolved
  }

  public async resolve(resolver: VariableResolver): Promise<boolean> {
    let value = await resolver.resolve(this)
    this._resolved = true
    if (value && value.includes('\n')) {
      // get indent from previous texts
      let indent = ''
      this.snippet.walk(m => {
        if (m == this) {
          return false
        }
        if (m instanceof Text) {
          let lines = m.toString().split(/\r?\n/)
          indent = lines[lines.length - 1].match(/^\s*/)[0]
        }
        return true
      })
      let lines = value.split('\n')
      let indents = lines.filter(s => s.length > 0).map(s => s.match(/^\s*/)[0])
      let minIndent = indents.length == 0 ? '' :
        indents.reduce((p, c) => p.length < c.length ? p : c)
      let newLines = lines.map((s, i) => i == 0 || s.length == 0 || !s.startsWith(minIndent) ? s :
        indent + s.slice(minIndent.length))
      value = newLines.join('\n')
    }
    if (this.transform) {
      value = this.transform.resolve(value || '')
    }
    if (value !== undefined) {
      this._children = [new Text(value) as any]
      return true
    }
    return false
  }

  public toTextmateString(): string {
    let transformString = ''
    if (this.transform) {
      transformString = this.transform.toTextmateString()
    }
    if (this.children.length === 0) {
      return `\${${this.name}${transformString}}`
    } else {
      return `\${${this.name}:${this.children.map(child => child.toTextmateString()).join('')}${transformString}}`
    }
  }

  public clone(): Variable {
    const ret = new Variable(this.name, this.resolved)
    if (this.transform) {
      ret.transform = this.transform.clone()
    }
    ret._children = this.children.map(child => child.clone())
    return ret
  }
}

export interface VariableResolver {
  resolve(variable: Variable): Promise<string | undefined>
}

export interface PlaceholderInfo {
  placeholders: Placeholder[]
  variables: Variable[]
  pyBlocks: CodeBlock[]
  otherBlocks: CodeBlock[]
}

function walk(marker: Marker[], visitor: (marker: Marker) => boolean): void {
  const stack = [...marker]
  while (stack.length > 0) {
    const marker = stack.shift()
    const recurse = visitor(marker)
    if (!recurse) {
      break
    }
    stack.unshift(...marker.children)
  }
}

export class TextmateSnippet extends Marker {

  public readonly ultisnip: boolean
  private _placeholders?: PlaceholderInfo
  private _values?: { [index: number]: string }
  constructor(ultisnip?: boolean) {
    super()
    this.ultisnip = ultisnip === true
  }

  public get hasPython(): boolean {
    if (!this.ultisnip) return false
    return this.pyBlocks.length > 0
  }

  public get hasCodeBlock(): boolean {
    if (!this.ultisnip) return false
    let { pyBlocks, otherBlocks } = this
    return pyBlocks.length > 0 || otherBlocks.length > 0
  }

  /**
   * Values for each placeholder index
   */
  public get values(): { [index: number]: string } {
    if (this._values) return this._values
    let values: { [index: number]: string } = {}
    let maxIndexNumber = 0
    this.placeholders.forEach(c => {
      maxIndexNumber = Math.max(c.index, maxIndexNumber)
      if (c.transform != null) return
      if (c.primary || values[c.index] === undefined) values[c.index] = c.toString()
    })
    for (let i = 0; i <= maxIndexNumber; i++) {
      if (values[i] === undefined) values[i] = ''
    }
    this._values = values
    return values
  }

  public get orderedPyIndexBlocks(): CodeBlock[] {
    let res: CodeBlock[] = []
    let filtered = this.pyBlocks.filter(o => typeof o.index === 'number')
    if (filtered.length == 0) return res
    let allIndexes = filtered.map(o => o.index)
    let usedIndexes: number[] = []
    const checkBlock = (b: CodeBlock): boolean => {
      let { related } = b
      if (related.length == 0
        || related.every(idx => !allIndexes.includes(idx) || usedIndexes.includes(idx))) {
        usedIndexes.push(b.index)
        res.push(b)
        return true
      }
      return false
    }
    while (filtered.length > 0) {
      let c = false
      for (let b of filtered) {
        if (checkBlock(b)) {
          c = true
        }
      }
      if (!c) {
        // recuisive dependencies detected
        break
      }
      filtered = filtered.filter(o => !usedIndexes.includes(o.index))
    }
    return res
  }

  public async evalCodeBlocks(nvim: Neovim, prepareCodes: string[]): Promise<void> {
    let { pyBlocks, otherBlocks } = this
    // update none python blocks
    await Promise.all(otherBlocks.map(block => {
      let pre = block.value
      return block.resolve(nvim).then(() => {
        if (block.parent instanceof Placeholder && pre !== block.value) {
          // update placeholder with same index
          this.onPlaceholderUpdate(block.parent)
        }
      })
    }))
    if (pyBlocks.length) {
      // run all python code by sequence
      const variableCode = getVariablesCode(this.values)
      await executePythonCode(nvim, [...prepareCodes, variableCode])
      for (let block of pyBlocks) {
        let pre = block.value
        await block.resolve(nvim)
        if (pre === block.value) continue
        if (block.parent instanceof Placeholder) {
          // update placeholder with same index
          this.onPlaceholderUpdate(block.parent)
          await executePythonCode(nvim, [getVariablesCode(this.values)])
        }
      }
      for (let block of this.orderedPyIndexBlocks) {
        await this.updatePyIndexBlock(nvim, block)
      }
      // update normal python block with related.
      let filtered = pyBlocks.filter(o => o.index === undefined && o.related.length > 0)
      for (let block of filtered) {
        await block.resolve(nvim)
      }
    }
  }

  /**
   * Update python blocks after user change Placeholder with index
   */
  public async updatePythonCodes(nvim: Neovim, marker: Marker): Promise<void> {
    let index: number | undefined
    if (marker instanceof Placeholder) {
      index = marker.index
    } else {
      while (marker.parent) {
        if (marker instanceof Placeholder) {
          index = marker.index
          break
        }
        marker = marker.parent
      }
    }
    if (index === undefined) return
    // update related placeholders
    let blocks = this.getDependentPyIndexBlocks(index)
    await executePythonCode(nvim, [getVariablesCode(this.values)])
    for (let block of blocks) {
      await this.updatePyIndexBlock(nvim, block)
    }
    // update normal py codes.
    let filtered = this.pyBlocks.filter(o => o.index === undefined && o.related.length > 0)
    for (let block of filtered) {
      await block.resolve(nvim)
    }
  }

  private getDependentPyIndexBlocks(index: number): CodeBlock[] {
    const res: CodeBlock[] = []
    const taken: number[] = []
    let filtered = this.pyBlocks.filter(o => typeof o.index === 'number')
    const search = (idx: number) => {
      let blocks = filtered.filter(o => !taken.includes(o.index) && o.related.includes(idx))
      if (blocks.length > 0) {
        res.push(...blocks)
        blocks.forEach(b => {
          search(b.index)
        })
      }
    }
    search(index)
    return res
  }

  /**
   * Update single index block
   */
  private async updatePyIndexBlock(nvim: Neovim, block: CodeBlock): Promise<void> {
    let pre = block.value
    await block.resolve(nvim)
    if (pre === block.value) return
    if (block.parent instanceof Placeholder) {
      this.onPlaceholderUpdate(block.parent)
    }
    await executePythonCode(nvim, [getVariablesCode(this.values)])
  }

  public get placeholderInfo(): PlaceholderInfo {
    if (!this._placeholders) {
      const variables = []
      const pyBlocks: CodeBlock[] = []
      const otherBlocks: CodeBlock[] = []
      // fill in placeholders
      let placeholders: Placeholder[] = []
      this.walk(candidate => {
        if (candidate instanceof Placeholder) {
          placeholders.push(candidate)
        } else if (candidate instanceof Variable) {
          let first = candidate.name.charCodeAt(0)
          // not jumpover for uppercase variable.
          if (first < 65 || first > 90) {
            variables.push(candidate)
          }
        } else if (candidate instanceof CodeBlock) {
          if (candidate.kind === 'python') {
            pyBlocks.push(candidate)
          } else {
            otherBlocks.push(candidate)
          }
        }
        return true
      })
      this._placeholders = { placeholders, pyBlocks, otherBlocks, variables }
    }
    return this._placeholders
  }

  public get variables(): Variable[] {
    return this.placeholderInfo.variables
  }

  public get placeholders(): Placeholder[] {
    return this.placeholderInfo.placeholders
  }

  public get pyBlocks(): CodeBlock[] {
    return this.placeholderInfo.pyBlocks
  }

  public get otherBlocks(): CodeBlock[] {
    return this.placeholderInfo.otherBlocks
  }

  public get maxIndexNumber(): number {
    let { placeholders } = this
    return placeholders.reduce((curr, p) => Math.max(curr, p.index), 0)
  }

  public get first(): Placeholder | Variable {
    let { placeholders, variables } = this
    let [normals, finals] = groupBy(placeholders.filter(p => !p.transform), v => v.index !== 0)
    if (normals.length) {
      let minIndex = Math.min.apply(null, normals.map(o => o.index))
      let arr = normals.filter(v => v.index == minIndex)
      return arr.find(p => p.primary) ?? arr[0]
    }
    if (variables.length) return variables[0]
    return finals.find(o => o.primary) ?? finals[0]
  }

  public insertSnippet(snippet: string, marker: Placeholder | Variable, parts: [string, string], ultisnip?: UltiSnippetContext): Placeholder | Variable {
    let index = marker instanceof Placeholder ? marker.index : this.maxIndexNumber + 1
    let [before, after] = parts
    let matchCode = ultisnip ? prepareMatchCode(ultisnip) : undefined
    let nested = new SnippetParser(!!ultisnip, matchCode).parse(snippet, true)
    let maxIndexAdded = nested.maxIndexNumber + 1
    let changed: Map<number, number> = new Map()
    for (let p of nested.placeholders) {
      let idx = p.index
      if (p.isFinalTabstop) {
        p.index = maxIndexAdded + index
      } else {
        p.index = p.index + index
      }
      changed.set(idx, p.index)
    }
    if (ultisnip) {
      nested.pyBlocks.forEach(b => {
        b.update(changed)
      })
    }
    let map: Map<number, number> = new Map()
    this.walk(m => {
      if (m instanceof Placeholder && m.index > index) {
        let idx = m.index
        m.index = m.index + maxIndexAdded
        map.set(idx, m.index)
      }
      return true
    })
    if (this.hasPython) {
      this.walk(m => {
        if (m instanceof CodeBlock) {
          m.update(map)
        }
        return true
      })
    }
    const select = nested.first
    let children = nested.children.slice()
    if (before) children.unshift(new Text(before))
    if (after) children.push(new Text(after))
    this.replace(marker, children)
    return select
  }

  public async update(nvim: Neovim, marker: Placeholder | Variable, value: string): Promise<void> {
    this.resetMarker(marker, value)
    if (this.hasPython) {
      await this.updatePythonCodes(nvim, marker)
    }
  }

  public deleteText(offset: number, length: number): boolean {
    let pos = 0
    let marker: Text | undefined
    let end = offset + length
    let start = 0
    this.walk(candidate => {
      let len = candidate.len()
      if (candidate instanceof Text && offset >= pos && pos + len >= end) {
        marker = candidate
        start = offset - pos
        return false
      }
      pos += len
      return true
    })
    if (!marker) return false
    let parent = marker.parent
    let text = marker.value
    let value = text.slice(0, start) + text.slice(start + length)
    let children = parent.children.slice()
    let idx = children.indexOf(marker)
    children.splice(idx, 1, new Text(value))
    parent.replaceChildren(children)
    return true
  }

  public resetMarker(marker: Placeholder | Variable, val: string): void {
    let markers: (Placeholder | Variable)[]
    if (marker instanceof Placeholder) {
      markers = this.placeholders.filter(o => o.index == marker.index)
    } else {
      markers = this.variables.filter(o => o.name == marker.name)
    }
    for (let p of markers) {
      let newText = p.transform ? p.transform.resolve(val) : val
      p.setOnlyChild(new Text(newText || ''))
    }
    this.synchronizeParents(markers)
    this.reset()
  }

  /**
   * Reflact changes for related markers.
   */
  public onPlaceholderUpdate(marker: Placeholder | Variable): void {
    let val = marker.toString()
    let markers: Placeholder[] | Variable[]
    if (marker instanceof Placeholder) {
      this.values[marker.index] = val
      markers = this.placeholders.filter(o => o.index == marker.index)
    } else {
      markers = this.variables.filter(o => o.name == marker.name)
    }
    for (let p of markers) {
      if (p === marker) continue
      let newText = p.transform ? p.transform.resolve(val) : val
      p.setOnlyChild(new Text(newText || ''))
    }
    this.synchronizeParents(markers)
  }

  public synchronizeParents(markers: Marker[]): void {
    let arr: Placeholder[] = []
    markers.forEach(m => {
      let p = m.parent
      if (p instanceof Placeholder && !arr.includes(p)) {
        arr.push(p)
      }
    })
    arr.forEach(p => {
      this.onPlaceholderUpdate(p)
    })
  }

  public offset(marker: Marker): number {
    let pos = 0
    let found = false
    this.walk(candidate => {
      if (candidate === marker) {
        found = true
        return false
      }
      pos += candidate.len()
      return true
    })

    if (!found) {
      return -1
    }
    return pos
  }

  public fullLen(marker: Marker): number {
    let ret = 0
    walk([marker], marker => {
      ret += marker.len()
      return true
    })
    return ret
  }

  public getTextBefore(marker: Marker, parent: Placeholder): string {
    let res = ''
    const calc = (m: Marker): void => {
      let p = m.parent
      if (!p) return
      let s = ''
      for (let b of p.children) {
        if (b === m) break
        s = s + b.toString()
      }
      res = s + res
      if (p == parent) return
      calc(p)
    }
    calc(marker)
    return res
  }

  public enclosingPlaceholders(placeholder: Placeholder | Variable): Placeholder[] {
    let ret: Placeholder[] = []
    let { parent } = placeholder
    while (parent) {
      if (parent instanceof Placeholder) {
        ret.push(parent)
      }
      parent = parent.parent
    }
    return ret
  }

  public async resolveVariables(resolver: VariableResolver): Promise<void> {
    let items: Variable[] = []
    this.walk(candidate => {
      if (candidate instanceof Variable && !candidate.resolved) {
        items.push(candidate)
      }
      return true
    })
    if (items.length) {
      await Promise.all(items.map(o => o.resolve(resolver)))
      this.synchronizeParents(items)
    }
  }

  public appendChild(child: Marker): this {
    this.reset()
    return super.appendChild(child)
  }

  public replace(marker: Marker, children: Marker[]): void {
    marker.replaceChildren(children)
    if (marker instanceof Placeholder || marker instanceof Variable) {
      this.onPlaceholderUpdate(marker)
    }
    this.reset()
  }

  /**
   * Used on replace happens.
   */
  public reset(): void {
    this._placeholders = undefined
    this._values = undefined
  }

  public toTextmateString(): string {
    return this.children.reduce((prev, cur) => prev + cur.toTextmateString(), '')
  }

  public clone(): TextmateSnippet {
    let ret = new TextmateSnippet(this.ultisnip)
    ret._children = this.children.map(child => child.clone())
    return ret
  }

  public walk(visitor: (marker: Marker) => boolean): void {
    walk(this.children, visitor)
  }
}

export class SnippetParser {
  constructor(
    private ultisnip?: boolean,
    private matchCode?: string
  ) {
  }

  public static escape(value: string): string {
    return value.replace(/\$|}|\\/g, '\\$&')
  }

  public static isPlainText(value: string): boolean {
    let s = new SnippetParser().parse(value.replace(/\$0$/, ''), false)
    return s.children.length == 1 && s.children[0] instanceof Text
  }

  private _scanner = new Scanner()
  private _token: Token

  public text(value: string): string {
    return this.parse(value, false).toString()
  }

  public parse(value: string, insertFinalTabstop?: boolean): TextmateSnippet {

    this._scanner.text(value)
    this._token = this._scanner.next()

    const snippet = new TextmateSnippet(this.ultisnip)
    while (this._parse(snippet)) {
      // nothing
    }

    // fill in values for placeholders. the first placeholder of an index
    // that has a value defines the value for all placeholders with that index
    const defaultValues = new Map<number, string>()
    const incompletePlaceholders: Placeholder[] = []
    let complexPlaceholders: Placeholder[] = []
    let hasFinal = false
    snippet.walk(marker => {
      if (marker instanceof Placeholder) {
        if (marker.index == 0) hasFinal = true
        if (marker.children.some(o => o instanceof Placeholder)) {
          complexPlaceholders.push(marker)
        } else if (!defaultValues.has(marker.index) && marker.children.length > 0) {
          marker.primary = true
          defaultValues.set(marker.index, marker.toString())
        } else {
          incompletePlaceholders.push(marker)
        }
      }
      return true
    })

    const complexIndexes = complexPlaceholders.map(p => p.index)
    for (const placeholder of incompletePlaceholders) {
      // avoid transform and replace since no value exists.
      if (defaultValues.has(placeholder.index)) {
        let val = defaultValues.get(placeholder.index)
        let text = new Text(placeholder.transform ? placeholder.transform.resolve(val) : val)
        placeholder.setOnlyChild(text)
      } else if (!complexIndexes.includes(placeholder.index)) {
        if (placeholder.transform) {
          let text = new Text(placeholder.transform.resolve(''))
          placeholder.setOnlyChild(text)
        } else {
          placeholder.primary = true
          defaultValues.set(placeholder.index, '')
        }
      }
    }
    const resolveComplex = () => {
      let resolved: Set<number> = new Set()
      for (let p of complexPlaceholders) {
        if (p.children.every(o => !(o instanceof Placeholder) || defaultValues.has(o.index))) {
          let val = p.toString()
          defaultValues.set(p.index, val)
          for (let placeholder of incompletePlaceholders.filter(o => o.index == p.index)) {
            let text = new Text(placeholder.transform ? placeholder.transform.resolve(val) : val)
            placeholder.setOnlyChild(text)
          }
          resolved.add(p.index)
        }
      }
      complexPlaceholders = complexPlaceholders.filter(p => !resolved.has(p.index))
      if (complexPlaceholders.length == 0 || !resolved.size) return
      resolveComplex()
    }
    resolveComplex()

    if (!hasFinal && insertFinalTabstop) {
      // the snippet uses placeholders but has no
      // final tabstop defined -> insert at the end
      snippet.appendChild(new Placeholder(0))
    }

    return snippet
  }

  private _accept(type?: TokenType): boolean
  private _accept(type: TokenType | undefined, value: true): string
  private _accept(type: TokenType, value?: boolean): boolean | string {
    if (type === undefined || this._token.type === type) {
      let ret = !value ? true : this._scanner.tokenText(this._token)
      this._token = this._scanner.next()
      return ret
    }
    return false
  }

  private _backTo(token: Token): false {
    this._scanner.pos = token.pos + token.len
    this._token = token
    return false
  }

  private _until(type: TokenType, checkBackSlash = false): false | string {
    if (this._token.type === TokenType.EOF) {
      return false
    }
    let start = this._token
    let pre: Token
    while (this._token.type !== type || (checkBackSlash && pre?.type === TokenType.Backslash)) {
      if (checkBackSlash) pre = this._token
      this._token = this._scanner.next()
      if (this._token.type === TokenType.EOF) {
        return false
      }
    }
    let value = this._scanner.value.substring(start.pos, this._token.pos)
    this._token = this._scanner.next()
    return value
  }

  private _parse(marker: Marker): boolean {
    return this._parseEscaped(marker)
      || this._parseCodeBlock(marker)
      || this._parseTabstopOrVariableName(marker)
      || this._parseComplexPlaceholder(marker)
      || this._parseComplexVariable(marker)
      || this._parseAnything(marker)
  }

  // \$, \\, \} -> just text
  private _parseEscaped(marker: Marker): boolean {
    let value: string
    // eslint-disable-next-line no-cond-assign
    if (value = this._accept(TokenType.Backslash, true)) {
      // saw a backslash, append escaped token or that backslash
      value = this._accept(TokenType.Dollar, true)
        || this._accept(TokenType.CurlyClose, true)
        || this._accept(TokenType.Backslash, true)
        || (this.ultisnip && this._accept(TokenType.CurlyOpen, true))
        || (this.ultisnip && this._accept(TokenType.BackTick, true))
        || value

      marker.appendChild(new Text(value))
      return true
    }
    return false
  }

  // $foo -> variable, $1 -> tabstop
  private _parseTabstopOrVariableName(parent: Marker): boolean {
    let value: string
    const token = this._token
    const match = this._accept(TokenType.Dollar)
      && (value = this._accept(TokenType.VariableName, true) || this._accept(TokenType.Int, true))

    if (!match) {
      return this._backTo(token)
    }

    parent.appendChild(/^\d+$/.test(value)
      ? new Placeholder(Number(value))
      : new Variable(value)
    )
    return true
  }

  // ${1:<children>}, ${1} -> placeholder
  private _parseComplexPlaceholder(parent: Marker): boolean {
    let index: string
    const token = this._token
    const match = this._accept(TokenType.Dollar)
      && this._accept(TokenType.CurlyOpen)
      && (index = this._accept(TokenType.Int, true))

    if (!match) {
      return this._backTo(token)
    }

    const placeholder = new Placeholder(Number(index))

    if (this._accept(TokenType.Colon)) {
      // ${1:<children>}
      // eslint-disable-next-line no-constant-condition
      while (true) {

        // ...} -> done
        if (this._accept(TokenType.CurlyClose)) {
          parent.appendChild(placeholder)
          return true
        }

        if (this._parse(placeholder)) {
          continue
        }

        // fallback
        parent.appendChild(new Text('${' + index + ':'))
        placeholder.children.forEach(parent.appendChild, parent)
        return true
      }
    } else if (placeholder.index > 0 && this._accept(TokenType.Pipe)) {
      // ${1|one,two,three|}
      const choice = new Choice()

      // eslint-disable-next-line no-constant-condition
      while (true) {
        if (this._parseChoiceElement(choice)) {

          if (this._accept(TokenType.Comma)) {
            // opt, -> more
            continue
          }

          if (this._accept(TokenType.Pipe)) {
            placeholder.appendChild(choice)
            if (this._accept(TokenType.CurlyClose)) {
              // ..|} -> done
              parent.appendChild(placeholder)
              return true
            }
          }
        }

        this._backTo(token)
        return false
      }

    } else if (this._accept(TokenType.Forwardslash)) {
      // ${1/<regex>/<format>/<options>}
      if (this._parseTransform(placeholder)) {
        parent.appendChild(placeholder)
        return true
      }

      this._backTo(token)
      return false

    } else if (this._accept(TokenType.CurlyClose)) {
      // ${1}
      parent.appendChild(placeholder)
      return true

    } else {
      // ${1 <- missing curly or colon
      return this._backTo(token)
    }
  }

  private _parseChoiceElement(parent: Choice): boolean {
    const token = this._token
    const values: string[] = []

    // eslint-disable-next-line no-constant-condition
    while (true) {
      if (this._token.type === TokenType.Comma || this._token.type === TokenType.Pipe) {
        break
      }
      let value: string
      // eslint-disable-next-line no-cond-assign
      if (value = this._accept(TokenType.Backslash, true)) {
        // \, \|, or \\
        value = this._accept(TokenType.Comma, true)
          || this._accept(TokenType.Pipe, true)
          || this._accept(TokenType.Backslash, true)
          || value
      } else {
        value = this._accept(undefined, true)
      }
      if (!value) {
        // EOF
        this._backTo(token)
        return false
      }
      values.push(value)
    }

    if (values.length === 0) {
      this._backTo(token)
      return false
    }

    parent.appendChild(new Text(values.join('')))
    return true
  }

  // ${foo:<children>}, ${foo} -> variable
  private _parseComplexVariable(parent: Marker): boolean {
    let name: string
    const token = this._token
    const match = this._accept(TokenType.Dollar)
      && this._accept(TokenType.CurlyOpen)
      && (name = this._accept(TokenType.VariableName, true))

    if (!match) {
      return this._backTo(token)
    }

    const variable = new Variable(name)
    if (this._accept(TokenType.Colon)) {
      // ${foo:<children>}
      // eslint-disable-next-line no-constant-condition
      while (true) {

        // ...} -> done
        if (this._accept(TokenType.CurlyClose)) {
          parent.appendChild(variable)
          return true
        }

        if (this._parse(variable)) {
          continue
        }

        // fallback
        parent.appendChild(new Text('${' + name + ':'))
        variable.children.forEach(parent.appendChild, parent)
        return true
      }

    } else if (this._accept(TokenType.Forwardslash)) {
      // ${foo/<regex>/<format>/<options>}
      if (this._parseTransform(variable)) {
        parent.appendChild(variable)
        return true
      }

      this._backTo(token)
      return false

    } else if (this._accept(TokenType.CurlyClose)) {
      // ${foo}
      parent.appendChild(variable)
      return true

    } else {
      // ${foo <- missing curly or colon
      return this._backTo(token)
    }
  }

  private _parseTransform(parent: TransformableMarker): boolean {
    // ...<regex>/<format>/<options>}

    let transform = new Transform()
    transform.ultisnip = this.ultisnip === true
    let regexValue = ''
    let regexOptions = ''

    // (1) /regex
    // eslint-disable-next-line no-constant-condition
    while (true) {
      if (this._accept(TokenType.Forwardslash)) {
        break
      }

      let escaped: string
      // eslint-disable-next-line no-cond-assign
      if (escaped = this._accept(TokenType.Backslash, true)) {
        escaped = this._accept(TokenType.Forwardslash, true) || escaped
        regexValue += escaped
        continue
      }

      if (this._token.type !== TokenType.EOF) {
        regexValue += this._accept(undefined, true)
        continue
      }
      return false
    }

    // (2) /format
    // eslint-disable-next-line no-constant-condition
    while (true) {
      if (this._accept(TokenType.Forwardslash)) {
        break
      }

      let escaped: string
      // eslint-disable-next-line no-cond-assign
      if (escaped = this._accept(TokenType.Backslash, true)) {
        escaped = this._accept(TokenType.Forwardslash, true) || escaped
        transform.appendChild(new Text(escaped))
        continue
      }
      if (this._parseFormatString(transform) || this._parseConditionString(transform) || this._parseAnything(transform)) {
        continue
      }
      return false
    }

    let ascii = false
    // (3) /option
    // eslint-disable-next-line no-constant-condition
    while (true) {
      if (this._accept(TokenType.CurlyClose)) {
        break
      }
      if (this._token.type !== TokenType.EOF) {
        let c = this._accept(undefined, true)
        if (c == 'a') {
          ascii = true
        } else {
          if (!knownRegexOptions.includes(c)) {
            logger.error(`Unknown regex option: ${c}`)
          }
          regexOptions += c
        }
        continue
      }
      return false
    }

    try {
      if (ascii) transform.ascii = true
      if (this.ultisnip) regexValue = convertRegex(regexValue)
      transform.regexp = new RegExp(regexValue, regexOptions)
    } catch (e) {
      return false
    }

    parent.transform = transform
    return true
  }

  private _parseConditionString(parent: Transform): boolean {
    if (!this.ultisnip) return false
    const token = this._token
    // (?1:foo:bar)
    if (!this._accept(TokenType.OpenParen)) {
      return false
    }
    if (!this._accept(TokenType.QuestionMark)) {
      this._backTo(token)
      return false
    }
    let index = this._accept(TokenType.Int, true)
    if (!index) {
      this._backTo(token)
      return false
    }
    if (!this._accept(TokenType.Colon)) {
      this._backTo(token)
      return false
    }
    let text = this._until(TokenType.CloseParen, true)
    if (text) {
      let i = 0
      while (i < text.length) {
        let t = text[i]
        if (t == ':' && text[i - 1] != '\\') {
          break
        }
        i++
      }
      let ifValue = text.slice(0, i)
      let elseValue = text.slice(i + 1)
      parent.appendChild(new ConditionString(Number(index), ifValue, elseValue))
      return true
    }
    this._backTo(token)
    return false
  }

  private _parseFormatString(parent: Transform): boolean {

    const token = this._token
    if (!this._accept(TokenType.Dollar)) {
      return false
    }

    let complex = false
    if (this._accept(TokenType.CurlyOpen)) {
      complex = true
    }

    let index = this._accept(TokenType.Int, true)

    if (!index) {
      this._backTo(token)
      return false

    } else if (!complex) {
      // $1
      parent.appendChild(new FormatString(Number(index)))
      return true

    } else if (this._accept(TokenType.CurlyClose)) {
      // ${1}
      parent.appendChild(new FormatString(Number(index)))
      return true

    } else if (!this._accept(TokenType.Colon)) {
      this._backTo(token)
      return false
    }
    if (this.ultisnip) {
      this._backTo(token)
      return false
    }

    if (this._accept(TokenType.Forwardslash)) {
      // ${1:/upcase}
      let shorthand = this._accept(TokenType.VariableName, true)
      if (!shorthand || !this._accept(TokenType.CurlyClose)) {
        this._backTo(token)
        return false
      } else {
        parent.appendChild(new FormatString(Number(index), shorthand))
        return true
      }

    } else if (this._accept(TokenType.Plus)) {
      // ${1:+<if>}
      let ifValue = this._until(TokenType.CurlyClose)
      if (ifValue) {
        parent.appendChild(new FormatString(Number(index), undefined, ifValue, undefined))
        return true
      }

    } else if (this._accept(TokenType.Dash)) {
      // ${2:-<else>}
      let elseValue = this._until(TokenType.CurlyClose)
      if (elseValue) {
        parent.appendChild(new FormatString(Number(index), undefined, undefined, elseValue))
        return true
      }

    } else if (this._accept(TokenType.QuestionMark)) {
      // ${2:?<if>:<else>}
      let ifValue = this._until(TokenType.Colon)
      if (ifValue) {
        let elseValue = this._until(TokenType.CurlyClose)
        if (elseValue) {
          parent.appendChild(new FormatString(Number(index), undefined, ifValue, elseValue))
          return true
        }
      }

    } else {
      let elseValue = this._until(TokenType.CurlyClose)
      if (elseValue) {
        parent.appendChild(new FormatString(Number(index), undefined, undefined, elseValue))
        return true
      }
    }

    this._backTo(token)
    return false
  }

  private _parseCodeBlock(parent: Marker): boolean {
    if (!this.ultisnip) return false
    const token = this._token
    if (!this._accept(TokenType.BackTick)) {
      return false
    }
    let text = this._until(TokenType.BackTick, true)
    // `shell code` `!v` `!p`
    if (text) {
      if (!text.startsWith('!')) {
        let marker = new CodeBlock(text.trim(), 'shell')
        parent.appendChild(marker)
        return true
      }
      if (text.startsWith('!v')) {
        let marker = new CodeBlock(text.slice(2).trim(), 'vim')
        parent.appendChild(marker)
        return true
      }
      if (text.startsWith('!p')) {
        let code = text.slice(2)
        let pre = this.matchCode ? this.matchCode + '\n' : ''
        if (code.indexOf('\n') == -1) {
          let marker = new CodeBlock(pre + code.trim(), 'python')
          parent.appendChild(marker)
        } else {
          let codes = code.split(/\r?\n/)
          codes = codes.filter(s => !/^\s*$/.test(s))
          // format multi line code
          let ind = codes[0] ? codes[0].match(/^\s*/)[0] : ''
          if (ind.length && codes.every(s => s.startsWith(ind))) {
            codes = codes.map(s => s.slice(ind.length))
          }
          if (ind == ' ' && codes[0].startsWith(ind)) codes[0] = codes[0].slice(1)
          let marker = new CodeBlock(pre + codes.join('\n'), 'python')
          parent.appendChild(marker)
        }
        return true
      }
    }
    this._backTo(token)
    return false
  }

  private _parseAnything(marker: Marker): boolean {
    if (this._token.type !== TokenType.EOF) {
      let text = this._scanner.tokenText(this._token)
      marker.appendChild(new Text(text))
      this._accept(undefined)
      return true
    }
    return false
  }
}

const escapedCharacters = [':', '(', ')', '{', '}']
export function transformEscapes(input: string, backslashIndexes = []): string {
  let res = ''
  let len = input.length
  let i = 0
  let toUpper = false
  let toLower = false
  while (i < len) {
    let ch = input[i]
    if (ch.charCodeAt(0) === CharCode.Backslash && !backslashIndexes.includes(i)) {
      let next = input[i + 1]
      if (escapedCharacters.includes(next)) {
        i++
        continue
      }
      if (next == 'u' || next == 'l') {
        // Uppercase/Lowercase next letter
        let follow = input[i + 2]
        if (follow) res = res + (next == 'u' ? follow.toUpperCase() : follow.toLowerCase())
        i = i + 3
        continue
      }
      if (next == 'U' || next == 'L') {
        // Uppercase/Lowercase to \E
        if (next == 'U') {
          toUpper = true
        } else {
          toLower = true
        }
        i = i + 2
        continue
      }
      if (next == 'E') {
        toUpper = false
        toLower = false
        i = i + 2
        continue
      }
      if (next == 'n') {
        res += '\n'
        i = i + 2
        continue
      }
      if (next == 't') {
        res += '\t'
        i = i + 2
        continue
      }
    }
    if (toUpper) {
      ch = ch.toUpperCase()
    } else if (toLower) {
      ch = ch.toLowerCase()
    }
    res += ch
    i++
  }
  return res
}
