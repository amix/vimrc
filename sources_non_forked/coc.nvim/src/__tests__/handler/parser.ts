import { DocumentSymbol, Range, SymbolKind } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'

/**
 * A syntax parser that parse `class` and `method` only.
 */
export default class Parser {
  private _curr = 0
  private _symbols: DocumentSymbol[] = []
  private currSymbol: DocumentSymbol | undefined
  private len: number
  private textDocument: TextDocument
  constructor(private _content: string, private showDetail = false) {
    this.len = _content.length
    this.textDocument = TextDocument.create('test:///a', 'txt', 1, _content)
  }

  public parse(): DocumentSymbol[] {
    while (this._curr <= this.len - 1) {
      this.parseToken()
    }
    return this._symbols
  }

  /**
   * Parse a symbol, reset currSymbol & _curr
   */
  private parseToken(): void {
    this.skipSpaces()
    if (this.currSymbol) {
      let endOffset = this.textDocument.offsetAt(this.currSymbol.range.end)
      if (this._curr > endOffset) {
        this.currSymbol = undefined
      }
    }
    let remain = this.getLineRemain()
    let ms = remain.match(/^(class)\s(\w+)\s\{\s*/)
    if (ms) {
      // find class
      let start = this._curr + 6
      let end = start + ms[2].length
      let selectionRange = Range.create(this.textDocument.positionAt(start), this.textDocument.positionAt(end))
      let endPosition = this.findMatchedIndex(this._curr + ms[0].length)
      let range = Range.create(this.textDocument.positionAt(this._curr), this.textDocument.positionAt(endPosition))
      let symbolInfo: DocumentSymbol = {
        range,
        selectionRange,
        kind: SymbolKind.Class,
        name: ms[2],
        children: []
      }
      if (this.currSymbol && this.currSymbol.children) {
        this.currSymbol.children.push(symbolInfo)
      } else {
        this._symbols.push(symbolInfo)
      }
      this.currSymbol = symbolInfo
    } else if (this.currSymbol && this.currSymbol.kind == SymbolKind.Class) {
      let ms = remain.match(/(\w+)\((.*)\)\s*\{/)
      if (ms) {
        // find method
        let start = this._curr
        let end = start + ms[1].length
        let selectionRange = Range.create(this.textDocument.positionAt(start), this.textDocument.positionAt(end))
        let endPosition = this.findMatchedIndex(this._curr + ms[0].length)
        let range = Range.create(this.textDocument.positionAt(this._curr), this.textDocument.positionAt(endPosition))
        let symbolInfo: DocumentSymbol = {
          range,
          selectionRange,
          kind: SymbolKind.Method,
          detail: this.showDetail ? `(${ms[2]})` : undefined,
          name: ms[1]
        }
        if (this.currSymbol && this.currSymbol.children) {
          this.currSymbol.children.push(symbolInfo)
        } else {
          this._symbols.push(symbolInfo)
        }
      }
    }
    this._curr = this._curr + remain.length + 1
  }

  private findMatchedIndex(start: number): number {
    let level = 0
    for (let i = start; i < this.len; i++) {
      let ch = this._content[i]
      if (ch == '{') {
        level = level + 1
      }
      if (ch == '}') {
        if (level == 0) return i
        level = level - 1
      }
    }
    throw new Error(`Can't find matched }`)
  }

  private getLineRemain(): string {
    let chars = ''
    for (let i = this._curr; i < this.len; i++) {
      let ch = this._content[i]
      if (ch == '\n') break
      chars = chars + ch
    }
    return chars
  }

  private skipSpaces(): void {
    for (let i = this._curr; i < this.len; i++) {
      let ch = this._content[i]
      if (!ch || /\S/.test(ch)) {
        this._curr = i
        break
      }
    }
  }
}
