'use strict'
/**
 * Renderer for convert markdown to terminal string
 */
import Table from 'cli-table'
import * as styles from './styles'
const logger = require('../util/logger')('markdown-renderer')
let TABLE_CELL_SPLIT = '^*||*^'
let TABLE_ROW_WRAP = '*|*|*|*'
let TABLE_ROW_WRAP_REGEXP = new RegExp(escapeRegExp(TABLE_ROW_WRAP), 'g')
let COLON_REPLACER = '*#COLON|*'
let COLON_REPLACER_REGEXP = new RegExp(escapeRegExp(COLON_REPLACER), 'g')

// HARD_RETURN holds a character sequence used to indicate text has a
// hard (no-reflowing) line break.  Previously \r and \r\n were turned
// into \n in marked's lexer- preprocessing step. So \r is safe to use
// to indicate a hard (non-reflowed) return.
let HARD_RETURN = '\r'

function identity(str: string): string {
  return str
}

function cleanUpHtml(input: string): string {
  return styles.gray(input.replace(/(<([^>]+)>)/ig, ''))
}

let defaultOptions = {
  code: identity,
  blockquote: identity,
  html: cleanUpHtml,
  heading: styles.magenta,
  firstHeading: styles.magenta,
  hr: identity,
  listitem: identity,
  list,
  table: identity,
  paragraph: identity,
  strong: styles.bold,
  em: styles.italic,
  codespan: styles.yellow,
  del: styles.strikethrough,
  link: styles.underline,
  href: styles.underline,
  text: identity,
  unescape: true,
  emoji: false,
  width: 80,
  showSectionPrefix: true,
  tab: 2,
  tableOptions: {}
}

function fixHardReturn(text, reflow) {
  return reflow ? text.replace(HARD_RETURN, /\n/g) : text
}

function indentLines(indent, text) {
  return text.replace(/(^|\n)(.+)/g, '$1' + indent + '$2')
}

function identify(indent, text) {
  if (!text) return text
  return indent + text.split('\n').join('\n' + indent)
}

let BULLET_POINT_REGEX = '\\*'
let NUMBERED_POINT_REGEX = '\\d+\\.'
let POINT_REGEX = '(?:' + [BULLET_POINT_REGEX, NUMBERED_POINT_REGEX].join('|') + ')'

// Prevents nested lists from joining their parent list's last line
function fixNestedLists(body, indent) {
  let regex = new RegExp(
    '' +
    '(\\S(?: |  )?)' + // Last char of current point, plus one or two spaces
    // to allow trailing spaces
    '((?:' +
    indent +
    ')+)' + // Indentation of sub point
    '(' +
    POINT_REGEX +
    '(?:.*)+)$',
    'gm'
  ) // Body of subpoint
  return body.replace(regex, '$1\n' + indent + '$2$3')
}

let isPointedLine = function(line, indent) {
  return line.match('^(?:' + indent + ')*' + POINT_REGEX)
}

function toSpaces(str) {
  return ' '.repeat(str.length)
}

let BULLET_POINT = '* '
function bulletPointLine(indent, line) {
  return isPointedLine(line, indent) ? line : toSpaces(BULLET_POINT) + line
}

function bulletPointLines(lines, indent) {
  let transform = bulletPointLine.bind(null, indent)
  return lines
    .split('\n')
    .filter(identity)
    .map(transform)
    .join('\n')
}

let numberedPoint = function(n) {
  return n + '. '
}
function numberedLine(indent, line, num) {
  return isPointedLine(line, indent)
    ? {
      num: num + 1,
      line: line.replace(BULLET_POINT, numberedPoint(num + 1))
    }
    : {
      num,
      line: toSpaces(numberedPoint(num)) + line
    }
}

function numberedLines(lines, indent) {
  let transform = numberedLine.bind(null, indent)
  let num = 0
  return lines
    .split('\n')
    .filter(identity)
    .map(line => {
      const numbered = transform(line, num)
      num = numbered.num

      return numbered.line
    })
    .join('\n')
}

function list(body, ordered, indent) {
  body = body.trim()
  body = ordered ? numberedLines(body, indent) : bulletPointLines(body, indent)
  return body
}

function section(text) {
  return text + '\n\n'
}

function undoColon(str) {
  return str.replace(COLON_REPLACER_REGEXP, ':')
}

function generateTableRow(text, escape = null) {
  if (!text) return []
  escape = escape || identity
  let lines = escape(text).split('\n')

  let data = []
  lines.forEach(function(line) {
    if (!line) return
    let parsed = line
      .replace(TABLE_ROW_WRAP_REGEXP, '')
      .split(TABLE_CELL_SPLIT)

    data.push(parsed.splice(0, parsed.length - 1))
  })
  return data
}

function escapeRegExp(str) {
  // eslint-disable-next-line no-useless-escape
  return str.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, '\\$&')
}

function unescapeEntities(html) {
  return html
    .replace(/&amp;/g, '&')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
}

const links: Map<string, string> = new Map()

export interface RendererOptions {
  sanitize?: boolean
}

class Renderer {
  private o: any
  private tab: any
  private tableSettings: any
  // private emoji: any
  private unescape: any
  private transform: any
  constructor(public options: RendererOptions = {}, public highlightOptions: any = {}) {
    this.o = Object.assign({}, defaultOptions, options)
    this.tab = '  '
    this.tableSettings = this.o.tableOptions
    // this.emoji = identity
    this.unescape = this.o.unescape ? unescapeEntities : identity
    this.highlightOptions = highlightOptions || {}
    this.transform = this.compose(undoColon, this.unescape)
  }

  public text(t: string): string {
    return this.o.text(t)
  }

  public code(code: string, lang: string, _escaped: boolean): string {
    return '``` ' + lang + '\n' + code + '\n```\n'
  }

  public blockquote(quote: string): string {
    return section(this.o.blockquote(identify(this.tab, quote.trim())))
  }

  public html(html: string): string {
    return this.o.html(html)
  }

  public heading(text: string, level: number, _raw: any): string {
    text = this.transform(text)
    let prefix = this.o.showSectionPrefix
      ? new Array(level + 1).join('#') + ' '
      : ''
    text = prefix + text
    return section(
      level === 1 ? this.o.firstHeading(text) : this.o.heading(text)
    )
  }

  public hr(): string {
    // NOTE: the '─' character is conveniently translated into a window-wide
    // horizontal rule by coc.nvim/autoload/coc/float.vim. Using this character
    // causes the horizontal rule to appear like a proper hr separator. In case
    // the user isn't benefiting from a floating window, we provide three
    // characters so that the hr doesn't deviate too significantly from
    // Markdown's normal '-'.
    return `───\n`
  }

  public list(body, ordered): string {
    body = this.o.list(body, ordered, this.tab)
    return section(fixNestedLists(indentLines(this.tab, body), this.tab))
  }

  public listitem(text: string): string {
    let transform = this.compose(this.o.listitem, this.transform)
    let isNested = text.indexOf('\n') !== -1
    if (isNested) text = text.trim()
    // Use BULLET_POINT as a marker for ordered or unordered list item
    return '\n' + BULLET_POINT + transform(text)
  }

  public checkbox(checked): string {
    return '[' + (checked ? 'X' : ' ') + '] '
  }

  public paragraph(text: string): string {
    let transform = this.compose(this.o.paragraph, this.transform)
    text = transform(text)
    return section(text)
  }

  public table(header, body): string {
    let table = new Table(
      Object.assign(
        {},
        {
          head: generateTableRow(header)[0]
        },
        this.tableSettings
      )
    )
    generateTableRow(body, this.transform).forEach(function(row) {
      table.push(row)
    })
    return section(this.o.table(table.toString()))
  }

  public tablerow(content: string): string {
    return TABLE_ROW_WRAP + content + TABLE_ROW_WRAP + '\n'
  }

  public tablecell(content, _flags): string {
    return content + TABLE_CELL_SPLIT
  }

  public strong(text: string): string {
    return this.o.strong(text)
  }

  public em(text: string): string {
    text = fixHardReturn(text, this.o.reflowText)
    return this.o.em(text)
  }

  public codespan(text: string): string {
    text = fixHardReturn(text, this.o.reflowText)
    return this.o.codespan(text.replace(/:/g, COLON_REPLACER))
  }

  public br(): string {
    return '\n'
  }

  public del(text: string): string {
    return this.o.del(text)
  }

  public link(href, title, text): string {
    let prot: string
    try {
      prot = decodeURIComponent(unescape(href))
        .replace(/[^\w:]/g, '')
        .toLowerCase()
    } catch (e) {
      return ''
    }
    if (prot.startsWith('javascript:')) {
      return ''
    }
    if (text && href && text != href) {
      links.set(text, href)
    }
    if (text && text != href) return styles.blue(text)
    let out = this.o.href(href)
    return this.o.link(out)
  }

  public image(href, title, text): string {
    let out = '![' + text
    if (title) out += ' – ' + title
    return out + '](' + href + ')'
  }

  public compose(...funcs: Function[]): any {
    return (...args: any[]) => {
      for (let i = funcs.length; i-- > 0;) {
        args = [funcs[i].apply(this, args)]
      }
      return args[0]
    }
  }

  public static getLinks(): string[] {
    let res = []
    for (let [text, href] of links.entries()) {
      res.push(`${styles.blue(text)}: ${href}`)
    }
    links.clear()
    return res
  }
}

export default Renderer
