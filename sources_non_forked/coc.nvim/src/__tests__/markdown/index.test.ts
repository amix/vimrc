import { getHighlightItems, parseMarkdown, parseDocuments } from '../../markdown/index'
import { Documentation } from '../../types'

describe('getHighlightItems', () => {
  it('should get highlights in single line', async () => {
    let res = getHighlightItems('this line has highlights', 0, [10, 15])
    expect(res).toEqual([{
      colStart: 10,
      colEnd: 15,
      lnum: 0,
      hlGroup: 'CocUnderline'
    }])
  })

  it('should get highlights when active end extended', async () => {
    let res = getHighlightItems('this line', 0, [5, 30])
    expect(res).toEqual([{
      colStart: 5,
      colEnd: 9,
      lnum: 0,
      hlGroup: 'CocUnderline'
    }])
  })

  it('should get highlights across line', async () => {
    let res = getHighlightItems('this line\nhas highlights', 0, [5, 15])
    expect(res).toEqual([{
      colStart: 5, colEnd: 9, lnum: 0, hlGroup: 'CocUnderline'
    }, {
      colStart: 0, colEnd: 5, lnum: 1, hlGroup: 'CocUnderline'
    }])
    res = getHighlightItems('a\nb\nc\nd', 0, [2, 5])
    expect(res).toEqual([
      { colStart: 0, colEnd: 1, lnum: 1, hlGroup: 'CocUnderline' },
      { colStart: 0, colEnd: 1, lnum: 2, hlGroup: 'CocUnderline' },
      { colStart: 0, colEnd: 0, lnum: 3, hlGroup: 'CocUnderline' }
    ])
  })
})

describe('parseMarkdown', () => {
  it('should parse code blocks', async () => {
    let content = `
\`\`\`js
var global = globalThis
\`\`\`
\`\`\`ts
let str:string
\`\`\`
\`\`\`bash
if
\`\`\`
`
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual([
      'var global = globalThis',
      '',
      'let str:string',
      '',
      'if'
    ])
    expect(res.codes).toEqual([
      { filetype: 'javascript', startLine: 0, endLine: 1 },
      { filetype: 'typescript', startLine: 2, endLine: 3 },
      { filetype: 'sh', startLine: 4, endLine: 5 },
    ])
  })

  it('should merge empty lines', async () => {
    let content = `
![img](http://img.io)
![img](http://img.io)
[link](http://example.com)
[link](javascript:void(0))
`
    let res = parseMarkdown(content, { excludeImages: true })
    expect(res.lines).toEqual([
      'link',
      '',
      'link: http://example.com'
    ])
  })

  it('should parse html code block', async () => {
    let content = `
example:
\`\`\`html
<div>code</div>
\`\`\`
    `
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['example:', '', '<div>code</div>'])
    expect(res.codes).toEqual([{ filetype: 'html', startLine: 2, endLine: 3 }])
  })

  it('should compose empty lines', async () => {
    let content = 'foo\n\n\nbar\n\n\n'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['foo', '', 'bar'])
  })

  it('should merge lines', async () => {
    let content = 'first\nsecond'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['first', 'second'])
  })

  it('should parse ansi highlights', async () => {
    let content = '__foo__\n[link](link)'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['foo', 'link'])
    expect(res.highlights).toEqual([
      { hlGroup: 'CocBold', lnum: 0, colStart: 0, colEnd: 3 },
      { hlGroup: 'CocUnderline', lnum: 1, colStart: 0, colEnd: 4 }
    ])
  })

  it('should exclude images by option', async () => {
    let content = 'head\n![img](img)\ncontent ![img](img) ![img](img)'
    let res = parseMarkdown(content, { excludeImages: false })
    expect(res.lines).toEqual(['head', '![img](img)', 'content ![img](img) ![img](img)'])
    content = 'head\n![img](img)\ncontent ![img](img) ![img](img)'
    res = parseMarkdown(content, { excludeImages: true })
    expect(res.lines).toEqual(['head', 'content'])
  })

  it('should render hr', async () => {
    let content = 'foo\n***\nbar'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['foo', '', '───', 'bar'])
  })

  it('should render deleted text', async () => {
    let content = '~foo~'
    let res = parseMarkdown(content, {})
    expect(res.highlights).toEqual([
      { hlGroup: 'CocStrikeThrough', lnum: 0, colStart: 0, colEnd: 3 }
    ])
  })

  it('should render br', async () => {
    let content = 'a  \nb'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['a', 'b'])
  })

  it('should render code span', async () => {
    let content = '`foo`'
    let res = parseMarkdown(content, {})
    expect(res.highlights).toEqual([
      { hlGroup: 'CocMarkdownCode', lnum: 0, colStart: 0, colEnd: 3 }
    ])
  })

  it('should render html', async () => {
    let content = '<div>foo</div>'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual(['foo'])
  })

  it('should render checkbox', async () => {
    let content = '- [x] first\n- [ ] second'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual([
      '  * [X] first', '  * [ ] second'
    ])
  })

  it('should render numbered list', async () => {
    let content = '1. one\n2. two\n3. three'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual([
      '  1. one', '  2. two', '  3. three'
    ])
  })

  it('should render nested list', async () => {
    let content = '- foo\n- bar\n    - one\n    - two'
    let res = parseMarkdown(content, {})
    expect(res.lines).toEqual([
      '  * foo', '  * bar', '    * one', '    * two'
    ])
  })
})

describe('parseDocuments', () => {
  it('should parse documents with diagnostic filetypes', async () => {
    let docs = [{
      filetype: 'Error',
      content: 'Error text'
    }, {
      filetype: 'Warning',
      content: 'Warning text'
    }]
    let res = parseDocuments(docs)
    expect(res.lines).toEqual([
      'Error text',
      '─',
      'Warning text'
    ])
    expect(res.codes).toEqual([
      { hlGroup: 'CocErrorFloat', startLine: 0, endLine: 1 },
      { hlGroup: 'CocWarningFloat', startLine: 2, endLine: 3 }
    ])
  })

  it('should parse markdown document with filetype document', async () => {
    let docs = [{
      filetype: 'typescript',
      content: 'const workspace'
    }, {
      filetype: 'markdown',
      content: '**header**'
    }]
    let res = parseDocuments(docs)
    expect(res.lines).toEqual([
      'const workspace',
      '─',
      'header'
    ])
    expect(res.highlights).toEqual([{
      hlGroup: 'CocBold',
      lnum: 2,
      colStart: 0,
      colEnd: 6
    }])
    expect(res.codes).toEqual([
      { filetype: 'typescript', startLine: 0, endLine: 1 }
    ])
  })

  it('should parse document with highlights', async () => {
    let docs: Documentation[] = [{
      filetype: 'txt',
      content: 'foo'
    }, {
      filetype: 'txt',
      content: 'foo bar',
      highlights: [{
        lnum: 0,
        colStart: 4,
        colEnd: 7,
        hlGroup: 'String'
      }]
    }]
    let res = parseDocuments(docs)
    let { highlights } = res
    expect(highlights).toEqual([{ lnum: 2, colStart: 4, colEnd: 7, hlGroup: 'String' }])
  })

  it('should parse documents with active highlights', async () => {
    let docs = [{
      filetype: 'javascript',
      content: 'func(foo, bar)',
      active: [5, 8]
    }, {
      filetype: 'javascript',
      content: 'func()',
      active: [15, 20]
    }]
    let res = parseDocuments(docs as any)
    expect(res.highlights).toEqual([{ colStart: 5, colEnd: 8, lnum: 0, hlGroup: 'CocUnderline' }
    ])
  })
})
