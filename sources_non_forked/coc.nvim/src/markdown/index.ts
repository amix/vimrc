'use strict'
import { marked } from 'marked'
import Renderer from './renderer'
import { parseAnsiHighlights } from '../util/ansiparse'
import { byteLength } from '../util/string'
import stripAnsi from 'strip-ansi'
import { HighlightItem, Documentation } from '../types'
export const diagnosticFiletypes = ['Error', 'Warning', 'Info', 'Hint']
const logger = require('../util/logger')('markdown-index')

export interface MarkdownParseOptions {
  excludeImages?: boolean
}

export interface CodeBlock {
  /**
   * Must have filetype or hlgroup
   */
  filetype?: string
  hlGroup?: string
  startLine: number // 0 based
  endLine: number
}

export interface DocumentInfo {
  lines: string[]
  highlights: HighlightItem[]
  codes: CodeBlock[]
}

export function parseDocuments(docs: Documentation[], opts: MarkdownParseOptions = {}): DocumentInfo {
  let lines: string[] = []
  let highlights: HighlightItem[] = []
  let codes: CodeBlock[] = []
  let idx = 0
  for (let doc of docs) {
    let currline = lines.length
    let { content, filetype } = doc
    let hls = doc.highlights
    if (filetype == 'markdown') {
      let info = parseMarkdown(content, opts)
      codes.push(...info.codes.map(o => {
        o.startLine = o.startLine + currline
        o.endLine = o.endLine + currline
        return o
      }))
      highlights.push(...info.highlights.map(o => {
        o.lnum = o.lnum + currline
        return o
      }))
      lines.push(...info.lines)
    } else {
      let parts = content.trim().split(/\r?\n/)
      if (diagnosticFiletypes.includes(doc.filetype)) {
        codes.push({ hlGroup: `Coc${filetype}Float`, startLine: currline, endLine: currline + parts.length })
      } else {
        codes.push({ filetype: doc.filetype, startLine: currline, endLine: currline + parts.length })
      }
      lines.push(...parts)
    }
    if (Array.isArray(hls)) {
      highlights.push(...hls.map(o => {
        return Object.assign({}, o, { lnum: o.lnum + currline })
      }))
    }
    if (Array.isArray(doc.active)) {
      let arr = getHighlightItems(content, currline, doc.active)
      if (arr.length) highlights.push(...arr)
    }
    if (idx != docs.length - 1) {
      lines.push('─') // separate line
    }
    idx = idx + 1
  }
  return { lines, highlights, codes }
}

/**
 * Get 'CocUnderline' highlights from offset range
 */
export function getHighlightItems(content: string, currline: number, active: [number, number]): HighlightItem[] {
  let res: HighlightItem[] = []
  let [start, end] = active
  let lines = content.split(/\r?\n/)
  let used = 0
  let inRange = false
  for (let i = 0; i < lines.length; i++) {
    let line = lines[i]
    if (!inRange) {
      if (used + line.length > start) {
        inRange = true
        let colStart = byteLength(line.slice(0, start - used))
        if (used + line.length > end) {
          let colEnd = byteLength(line.slice(0, end - used))
          inRange = false
          res.push({ colStart, colEnd, lnum: i + currline, hlGroup: 'CocUnderline' })
          break
        } else {
          let colEnd = byteLength(line)
          res.push({ colStart, colEnd, lnum: i + currline, hlGroup: 'CocUnderline' })
        }
      }
    } else {
      if (used + line.length > end) {
        let colEnd = byteLength(line.slice(0, end - used))
        res.push({ colStart: 0, colEnd, lnum: i + currline, hlGroup: 'CocUnderline' })
        inRange = false
        break
      } else {
        let colEnd = byteLength(line)
        res.push({ colStart: 0, colEnd, lnum: i + currline, hlGroup: 'CocUnderline' })
      }
    }
    used = used + line.length + 1
  }
  return res
}

/**
 * Parse markdown for lines, highlights & codes
 */
export function parseMarkdown(content: string, opts: MarkdownParseOptions): DocumentInfo {
  marked.setOptions({
    renderer: new Renderer(),
    gfm: true,
    breaks: true
  })
  let lines: string[] = []
  let highlights: HighlightItem[] = []
  let codes: CodeBlock[] = []
  let currline = 0
  let inCodeBlock = false
  let filetype: string
  let startLnum = 0
  let parsed = marked(content)
  let links = Renderer.getLinks()
  if (links.length) {
    parsed = parsed + '\n\n' + links.join('\n')
  }
  parsed = parsed.replace(/\s*$/, '')
  let parsedLines = parsed.split(/\n/)
  for (let i = 0; i < parsedLines.length; i++) {
    let line = parsedLines[i]
    if (!line.length) {
      let pre = lines[lines.length - 1]
      if (pre) {
        lines.push(line)
        currline++
      }
      continue
    }
    if (opts.excludeImages && line.indexOf('![') !== -1) {
      line = line.replace(/\s*!\[.*?\]\(.*?\)/g, '')
      if (!stripAnsi(line).trim().length) continue
    }
    if (/\s*```\s*([A-Za-z0-9_,]+)?$/.test(line)) {
      if (!inCodeBlock) {
        let pre = parsedLines[i - 1]
        if (pre && /^\s*```\s*/.test(pre)) {
          lines.push('')
          currline++
        }
        inCodeBlock = true
        filetype = line.replace(/^\s*```\s*/, '')
        if (filetype == 'js') filetype = 'javascript'
        if (filetype == 'ts') filetype = 'typescript'
        if (filetype == 'bash') filetype = 'sh'
        startLnum = currline
      } else {
        inCodeBlock = false
        codes.push({
          filetype,
          startLine: startLnum,
          endLine: currline
        })
      }
      continue
    }
    if (inCodeBlock) {
      // no parse
      lines.push(line)
      currline++
      continue
    }
    let res = parseAnsiHighlights(line, true)
    if (res.highlights) {
      for (let hi of res.highlights) {
        let { hlGroup, span } = hi
        highlights.push({
          hlGroup,
          lnum: currline,
          colStart: span[0],
          colEnd: span[1]
        })
      }
    }
    lines.push(res.line)
    currline++
  }
  return { lines, highlights, codes }
}
