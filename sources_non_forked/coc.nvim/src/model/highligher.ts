'use strict'
import { Buffer } from '@chemzqm/neovim'
import { parseAnsiHighlights } from '../util/ansiparse'
import { byteLength } from '../util/string'
import { HighlightItem } from '../types'

export interface TextItem {
  text: string
  hlGroup?: string
}

/**
 * Build highlights, with lines and highlights
 */
export default class Highlighter {
  private lines: string[] = []
  private _highlights: HighlightItem[] = []

  public addLine(line: string, hlGroup?: string): void {
    if (line.includes('\n')) {
      for (let content of line.split(/\r?\n/)) {
        this.addLine(content, hlGroup)
      }
      return
    }
    if (hlGroup) {
      this._highlights.push({
        lnum: this.lines.length,
        colStart: line.match(/^\s*/)[0].length,
        colEnd: byteLength(line),
        hlGroup
      })
    } // '\x1b'
    if (line.includes('\x1b')) {
      let res = parseAnsiHighlights(line)
      for (let hl of res.highlights) {
        let { span, hlGroup } = hl
        if (span[0] != span[1]) {
          this._highlights.push({
            lnum: this.lines.length,
            colStart: span[0],
            colEnd: span[1],
            hlGroup
          })
        }
      }
      this.lines.push(res.line)
    } else {
      this.lines.push(line)
    }
  }

  public addLines(lines): void {
    this.lines.push(...lines)
  }

  /**
   * Add texts to new Lines
   */
  public addTexts(items: TextItem[]): void {
    let len = this.lines.length
    let text = ''
    for (let item of items) {
      let colStart = byteLength(text)
      if (item.hlGroup) {
        this._highlights.push({
          lnum: len,
          colStart,
          colEnd: colStart + byteLength(item.text),
          hlGroup: item.hlGroup
        })
      }
      text += item.text
    }
    this.lines.push(text)
  }

  public addText(text: string, hlGroup?: string): void {
    let { lines } = this
    let pre = lines[lines.length - 1] || ''
    if (text.includes('\n')) {
      let parts = text.split('\n')
      this.addText(parts[0], hlGroup)
      for (let line of parts.slice(1)) {
        this.addLine(line, hlGroup)
      }
      return
    }
    if (hlGroup) {
      let colStart = byteLength(pre)
      this._highlights.push({
        lnum: lines.length ? lines.length - 1 : 0,
        colStart,
        colEnd: colStart + byteLength(text),
        hlGroup
      })
    }
    if (lines.length) {
      lines[lines.length - 1] = `${pre}${text}`
    } else {
      lines.push(text)
    }
  }

  public get length(): number {
    return this.lines.length
  }

  public getline(line: number): string {
    return this.lines[line] || ''
  }

  public get highlights(): ReadonlyArray<HighlightItem> {
    return this._highlights
  }

  public get content(): string {
    return this.lines.join('\n')
  }

  // default to replace
  public render(buffer: Buffer, start = 0, end = -1): void {
    // eslint-disable-next-line @typescript-eslint/no-floating-promises
    buffer.setLines(this.lines, { start, end, strictIndexing: false }, true)
    for (let item of this._highlights) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      buffer.addHighlight({
        hlGroup: item.hlGroup,
        colStart: item.colStart,
        colEnd: item.colEnd == null ? -1 : item.colEnd,
        line: start + item.lnum,
        srcId: -1
      })
    }
  }
}
