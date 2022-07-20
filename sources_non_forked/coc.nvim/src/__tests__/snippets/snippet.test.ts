import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { CancellationTokenSource } from 'vscode-languageserver-protocol'
import { Position, Range, TextEdit } from 'vscode-languageserver-types'
import { URI } from 'vscode-uri'
import { LinesTextDocument } from '../../model/textdocument'
import { addPythonTryCatch, convertRegex, executePythonCode, UltiSnippetContext } from '../../snippets/eval'
import { Placeholder, TextmateSnippet } from '../../snippets/parser'
import { checkContentBefore, CocSnippet, getContentBefore, getEndPosition, getParts, normalizeSnippetString, reduceTextEdit, shouldFormat } from '../../snippets/snippet'
import { parseComments, parseCommentstring, SnippetVariableResolver } from '../../snippets/variableResolve'
import { UltiSnippetOption } from '../../types'
import workspace from '../../workspace'
import helper from '../helper'

let nvim: Neovim
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  let pyfile = path.join(__dirname, '../ultisnips.py')
  await nvim.command(`execute 'pyxfile '.fnameescape('${pyfile}')`)
})

afterAll(async () => {
  await helper.shutdown()
})

async function createSnippet(snippet: string, opts?: UltiSnippetOption, range = Range.create(0, 0, 0, 0), line = '') {
  let resolver = new SnippetVariableResolver(nvim, workspace.workspaceFolderControl)
  let snip = new CocSnippet(snippet, Position.create(0, 0), nvim, resolver)
  let context: UltiSnippetContext
  if (opts) context = { range, line, ...opts, }
  await snip.init(context)
  return snip
}

function createTextDocument(text: string): LinesTextDocument {
  return new LinesTextDocument('file://a', 'txt', 1, text.split('\n'), 1, true)
}

describe('CocSnippet', () => {
  async function assertResult(snip: string, resolved: string) {
    let c = await createSnippet(snip, {})
    expect(c.text).toBe(resolved)
  }

  async function assertPyxValue(code: string, res: any) {
    let val = await nvim.call(`pyxeval`, code) as string
    if (typeof res === 'number' || typeof res === 'string' || typeof res === 'boolean') {
      expect(val).toBe(res)
    } else if (res instanceof RegExp) {
      expect(val).toMatch(res)
    } else {
      expect(val).toEqual(res)
    }
  }

  describe('resolveVariables()', () => {
    it('should resolve uppercase variables', async () => {
      let doc = await helper.createDocument()
      let fsPath = URI.parse(doc.uri).fsPath
      await assertResult('$TM_FILENAME', path.basename(fsPath))
      await assertResult('$TM_FILENAME_BASE', path.basename(fsPath, path.extname(fsPath)))
      await assertResult('$TM_DIRECTORY', path.dirname(fsPath))
      await assertResult('$TM_FILEPATH', fsPath)
      await nvim.call('setreg', ['""', 'foo'])
      await assertResult('$YANK', 'foo')
      await assertResult('$TM_LINE_INDEX', '0')
      await assertResult('$TM_LINE_NUMBER', '1')
      await nvim.setLine('foo')
      await assertResult('$TM_CURRENT_LINE', 'foo')
      await nvim.call('setreg', ['*', 'foo'])
      await assertResult('$CLIPBOARD', 'foo')
      let d = new Date()
      await assertResult('$CURRENT_YEAR', d.getFullYear().toString())
      await assertResult('$NOT_EXISTS', 'NOT_EXISTS')
    })

    it('should resolve new VSCode variables', async () => {
      let doc = await helper.createDocument()
      await doc.buffer.setOption('comments', 's1:/*,mb:*,ex:*/,://,b:#,:%,:XCOMM,n:>,fb:-')
      await doc.buffer.setOption('commentstring', '')
      let fsPath = URI.parse(doc.uri).fsPath
      let c = await createSnippet('$RANDOM')
      expect(c.text.length).toBe(6)
      c = await createSnippet('$RANDOM_HEX')
      expect(c.text.length).toBe(6)
      c = await createSnippet('$UUID')
      expect(c.text).toMatch('-')
      c = await createSnippet('$RELATIVE_FILEPATH')
      expect(c.text).toMatch(path.basename(fsPath))
      c = await createSnippet('$WORKSPACE_NAME')
      expect(c.text.length).toBeGreaterThan(0)
      c = await createSnippet('$WORKSPACE_FOLDER')
      expect(c.text.length).toBeGreaterThan(0)
      await assertResult('$LINE_COMMENT', '//')
      await assertResult('$BLOCK_COMMENT_START', '/*')
      await assertResult('$BLOCK_COMMENT_END', '*/')
      await doc.buffer.setOption('comments', '')
      await doc.buffer.setOption('commentstring', '// %s')
      await assertResult('$LINE_COMMENT', '//')
      await assertResult('$BLOCK_COMMENT_START', '')
      await assertResult('$BLOCK_COMMENT_END', '')
    })

    it('should resolve variables in placeholders', async () => {
      await nvim.setLine('foo')
      await assertResult('$1 ${1:$TM_CURRENT_LINE}', 'foo foo')
      await assertResult('$1 ${1:$TM_CURRENT_LINE bar}', 'foo bar foo bar')
      await assertResult('$2 ${2:|${1:$TM_CURRENT_LINE}|}', '|foo| |foo|')
      await assertResult('$1 $2 ${2:${1:|$TM_CURRENT_LINE|}}', '|foo| |foo| |foo|')
    })

    it('should resolve variables  with default value', async () => {
      await assertResult('$1 ${1:${VISUAL:foo}}', 'foo foo')
    })

    it('should resolve for lower case variables', async () => {
      await assertResult('${foo:abcdef} ${bar}', 'abcdef bar')
      await assertResult('${1:${foo:abcdef}} ${1/^\\w\\w(.*)/$1/}', 'abcdef cdef')
    })
  })

  describe('code block initialize', () => {
    it('should init shell code block', async () => {
      await assertResult('`echo "hello"` world', 'hello world')
    })

    it('should init vim block', async () => {
      await assertResult('`!v eval("1 + 1")` = 2', '2 = 2')
      await nvim.setLine('  ')
      await assertResult('${1:`!v indent(".")`} "$1"', '2 "2"')
    })

    it('should init code block in placeholders', async () => {
      await assertResult('f ${1:`echo "b"`}', 'f b')
      await assertResult('f ${1:`!v "b"`}', 'f b')
      await assertResult('f ${1:`!p snip.rv = "b"`}', 'f b')
    })

    it('should setup python globals', async () => {
      await helper.edit('t.js')
      await createSnippet('`!p snip.rv = fn`', {})
      await assertPyxValue('fn', 't.js')
      await assertPyxValue('path', /t\.js$/)
      await assertPyxValue('t', [''])
      await assertPyxValue('context', true)
      await createSnippet('`!p snip.rv = fn`', {
        regex: '[ab]',
        context: 'False'
      }, Range.create(0, 2, 0, 3), 'a b')
      await assertPyxValue('context', false)
      await assertPyxValue('match.group(0)', 'b')
    })

    it('should setup python match', async () => {
      let c = await createSnippet('\\\\frac{`!p snip.rv = match.group(1)`}{$1}$0', {
        regex: '((\\d+)|(\\d*)(\\\\)?([A-Za-z]+)((\\^|_)(\\{\\d+\\}|\\d))*)/',
        context: 'True'
      }, Range.create(0, 0, 0, 3), '20/')
      await assertPyxValue('context', true)
      await assertPyxValue('match.group(1)', '20')
      expect(c.text).toBe('\\frac{20}{}')
    })

    it('should work with methods of snip', async () => {
      await nvim.command('setl shiftwidth=4 ft=txt tabstop=4 expandtab')
      await createSnippet('`!p snip.rv = "a"`', {}, Range.create(0, 4, 0, 8), '    abcd')
      await executePythonCode(nvim, [
        'snip.shift(1)',
        // ultisnip indent only when there's '\n' in snip.rv
        'snip += ""',
        'newLine = snip.mkline("foo")'
      ])
      await assertPyxValue('newLine', '        foo')
      await executePythonCode(nvim, [
        'snip.unshift(1)',
        'newLine = snip.mkline("b")'
      ])
      await assertPyxValue('newLine', '    b')
      await executePythonCode(nvim, [
        'snip.shift(1)',
        'snip.reset_indent()',
        'newLine = snip.mkline("f")'
      ])
      await assertPyxValue('newLine', '    f')
      await executePythonCode(nvim, [
        'fff = snip.opt("&fff", "foo")',
        'ft = snip.opt("&ft", "ft")',
      ])
      await assertPyxValue('fff', 'foo')
      await assertPyxValue('ft', 'txt')
    })

    it('should init python code block', async () => {
      await assertResult('`!p snip.rv = "a"` = a', 'a = a')
      await assertResult('`!p snip.rv = t[1]` = ${1:a}', 'a = a')
      await assertResult('`!p snip.rv = t[1]` = ${1:`!v eval("\'a\'")`}', 'a = a')
      await assertResult('`!p snip.rv = t[1] + t[2]` = ${1:a} ${2:b}', 'ab = a b')
    })

    it('should init python placeholder', async () => {
      await assertResult('foo ${1/^\\|(.*)\\|$/$1/} ${1:|`!p snip.rv = "a"`|}', 'foo a |a|')
      await assertResult('foo $1 ${1:`!p snip.rv = "a"`}', 'foo a a')
      await assertResult('${1/^_(.*)/$1/} $1 aa ${1:`!p snip.rv = "_foo"`}', 'foo _foo aa _foo')
    })

    it('should init nested python placeholder', async () => {
      await assertResult('${1:foo`!p snip.rv = t[2]`} ${2:bar} $1', 'foobar bar foobar')
      await assertResult('${3:f${2:oo${1:b`!p snip.rv = "ar"`}}} `!p snip.rv = t[3]`', 'foobar foobar')
    })

    it('should recursive init python placeholder', async () => {
      await assertResult('${1:`!p snip.rv = t[2]`} ${2:`!p snip.rv = t[3]`} ${3:`!p snip.rv = t[4][0]`} ${4:bar}', 'b b b bar')
      await assertResult('${1:foo} ${2:`!p snip.rv = t[1][0]`} ${3:`!p snip.rv = ""`} ${4:`!p snip.rv = t[2]`}', 'foo f  f')
    })

    it('should update python block from placeholder', async () => {
      await assertResult('`!p snip.rv = t[1][0] if len(t[1]) > 0 else ""` ${1:`!p snip.rv = t[2]`} ${2:foo}', 'f foo foo')
    })

    it('should update nested placeholder values', async () => {
      let c = await createSnippet('${2:foo ${1:`!p snip.rv = "bar"`}} ${2/^\\w//} `!p snip.rv = t[2]`', {})
      expect(c.text).toBe('foo bar oo bar foo bar')
    })

  })

  describe('getContentBefore()', () => {
    it('should get text before marker', async () => {
      let c = await createSnippet('${1:foo} ${2:bar}', {})
      let markers = c.placeholders
      let p = markers[0].parent
      expect(p instanceof TextmateSnippet).toBe(true)
      expect(getContentBefore(p)).toBe('')
      expect(getContentBefore(markers[0])).toBe('')
      expect(getContentBefore(markers[1])).toBe('foo ')
    })

    it('should get text before nested marker', async () => {
      let c = await createSnippet('${1:foo} ${2:is nested with $4} $3 bar', {})
      let markers = c.placeholders as Placeholder[]
      let p = markers.find(o => o.index == 4)
      expect(getContentBefore(p)).toBe('foo is nested with ')
      p = markers.find(o => o.index == 0)
      expect(getContentBefore(p)).toBe('foo is nested with   bar')
    })

    it('should consider normal line break', async () => {
      let c = await createSnippet('${1:foo}\n${2:is nested with $4}', {})
      let markers = c.placeholders as Placeholder[]
      let p = markers.find(o => o.index == 4)
      expect(getContentBefore(p)).toBe('is nested with ')
    })

    it('should consider line break after update', async () => {
      let c = await createSnippet('${1:foo} ${2}', {})
      let p = c.getPlaceholder(1)
      await c.tmSnippet.update(nvim, p.marker, 'abc\ndef')
      let markers = c.placeholders as Placeholder[]
      let placeholder = markers.find(o => o.index == 2)
      expect(getContentBefore(placeholder)).toBe('def ')
    })
  })

  describe('getSortedPlaceholders()', () => {
    it('should get sorted placeholders', async () => {
      const assert = (snip: CocSnippet, index: number | undefined, indexes: number[]) => {
        let curr = index == null ? undefined : snip.getPlaceholder(index)
        let res = snip.getSortedPlaceholders(curr)
        expect(res.map(o => o.index)).toEqual(indexes)
      }
      let c = await createSnippet('${1:foo} ${2/^\\w//} ${2:bar} ', {})
      assert(c, undefined, [1, 2, 0])
      assert(c, 1, [1, 2, 0])
      assert(c, 2, [2, 1, 0])
    })
  })

  describe('getNewText()', () => {
    it('should getNewText for placeholder', async () => {
      let c = await createSnippet('before ${1:foo} after$2', {})
      let p = c.getPlaceholder(1)
      expect(c.getNewText(p, `fff`)).toBe(undefined)
      expect(c.getNewText(p, `before foo `)).toBe(undefined)
      expect(c.getNewText(p, `before foo afteralll`)).toBe(undefined)
      expect(c.getNewText(p, `before bar after`)).toBe('bar')
      p = c.getPlaceholder(2)
      expect(c.getNewText(p, `before foo afterbar`)).toBe('bar')
    })
  })

  describe('updatePlaceholder()', () => {
    async function assertUpdate(text: string, value: string, result: string, index = 1): Promise<CocSnippet> {
      let c = await createSnippet(text, {})
      let p = c.getPlaceholder(index)
      expect(p != null).toBe(true)
      await c.tmSnippet.update(nvim, p.marker, value)
      expect(c.tmSnippet.toString()).toBe(result)
      return c
    }

    it('should work with snip.c', async () => {
      let code = [
        '#ifndef ${1:`!p',
        'if not snip.c:',
        '  import random, string',
        "  name = re.sub(r'[^A-Za-z0-9]+','_', snip.fn).upper()",
        "  rand = ''.join(random.sample(string.ascii_letters+string.digits, 8))",
        "  snip.rv = ('%s_%s' % (name,rand)).upper()",
        "else:",
        "  snip.rv = snip.c + t[2]`}",
        '#define $1',
        '$2'
      ].join('\n')
      let c = await createSnippet(code, {})
      let first = c.text.split('\n')[0]
      let p = c.getPlaceholder(2)
      expect(p).toBeDefined()
      await c.tmSnippet.update(nvim, p.marker, 'foo')
      let t = c.tmSnippet.toString()
      expect(t.startsWith(first)).toBe(true)
      expect(t.split('\n').map(s => s.endsWith('foo'))).toEqual([true, true, true])
    })

    it('should calculate delta', async () => {
      // TODO
    })

    it('should update variable placeholders', async () => {
      await assertUpdate('${foo} ${foo}', 'bar', 'bar bar')
      await assertUpdate('${foo} ${foo:x}', 'bar', 'bar bar')
      await assertUpdate('${1:${foo:x}} $1', 'bar', 'bar bar')
    })

    it('should update placeholder with code blocks', async () => {
      await assertUpdate('${1:`echo "foo"`} $1', 'bar', 'bar bar')
      await assertUpdate('${2:${1:`echo "foo"`}} $2', 'bar', 'bar bar')
      await assertUpdate('${1:`!v "foo"`} $1', 'bar', 'bar bar')
      await assertUpdate('${1:`!p snip.rv = "foo"`} $1', 'bar', 'bar bar')
    })

    it('should update related python blocks', async () => {
      // multiple
      await assertUpdate('`!p snip.rv = t[1]` ${1:`!p snip.rv = "foo"`} `!p snip.rv = t[1]`', 'bar', 'bar bar bar')
      // parent
      await assertUpdate('`!p snip.rv = t[2]` ${2:foo ${1:`!p snip.rv = "foo"`}}', 'bar', 'foo bar foo bar')
      // related placeholders
      await assertUpdate('${2:foo `!p snip.rv = t[1]`} ${1:`!p snip.rv = "foo"`}', 'bar', 'foo bar bar')
    })

    it('should update python code blocks with normal placeholder values', async () => {
      await assertUpdate('`!p snip.rv = t[1]` $1 `!p snip.rv = t[1]`', 'bar', 'bar bar bar')
      await assertUpdate('`!p snip.rv = t[2]` ${2:foo $1}', 'bar', 'foo bar foo bar')
      await assertUpdate('${2:foo `!p snip.rv = t[1]`} $1', 'bar', 'foo bar bar')
    })

    it('should reset values for removed placeholders', async () => {
      // Keep remained placeholder this is same behavior of VSCode.
      let s = await assertUpdate('${2:bar${1:foo}} $2 $1', 'bar', 'bar bar foo', 2)
      let prev = s.getPrevPlaceholder(2)
      expect(prev).toBeDefined()
      expect(prev.value).toBe('foo')
      // python placeholder, reset to empty value
      await assertUpdate('${2:bar${1:foo}} $2 `!p snip.rv = t[1]`', 'bar', 'bar bar ', 2)
      // not reset since $1 still exists
      await assertUpdate('${2:bar${1:foo}} $2 $1 `!p snip.rv = t[1]`', 'bar', 'bar bar foo foo', 2)
    })
  })

  describe('getRanges()', () => {
    it('should get ranges of placeholder', async () => {
      let c = await createSnippet('${2:${1:x} $1}\n$2', {})
      let p = c.getPlaceholder(1)
      let arr = c.getRanges(p)
      expect(arr.length).toBe(4)
      expect(arr[0]).toEqual(Range.create(0, 0, 0, 1))
      expect(arr[1]).toEqual(Range.create(0, 2, 0, 3))
      expect(arr[2]).toEqual(Range.create(1, 0, 1, 1))
      expect(arr[3]).toEqual(Range.create(1, 2, 1, 3))
      expect(c.text).toBe('x x\nx x')
    })
  })

  describe('insertSnippet()', () => {
    it('should update indexes of python blocks', async () => {
      let c = await createSnippet('${1:a} ${2:b} ${3:`!p snip.rv=t[2]`}', {})
      let p = c.getPlaceholder(1)
      await c.insertSnippet(p, '${1:foo} ${2:bar}', ['', ''])
      expect(c.text).toBe('foo bar b b')
      p = c.getPlaceholder(5)
      expect(p.after).toBe(' b')
      let source = new CancellationTokenSource()
      let res = await c.updatePlaceholder(p, Position.create(0, 9), 'xyz', source.token)
      expect(res.text).toBe('foo bar xyz xyz')
    })

    it('should insert nested placeholder', async () => {
      let c = await createSnippet('${1:foo}\n$1', {})
      let p = c.getPlaceholder(1)
      let marker = await c.insertSnippet(p, '${1:x} $1', ['', '']) as Placeholder
      p = c.getPlaceholder(marker.index)
      let source = new CancellationTokenSource()
      let res = await c.updatePlaceholder(p, Position.create(0, 3), 'bar', source.token)
      expect(res.text).toBe('bar bar\nbar bar')
      expect(res.delta).toEqual(Position.create(0, 0))
    })

    it('should insert nested python snippet', async () => {
      let c = await createSnippet('${1:foo}\n`!p snip.rv = t[1]`', {})
      let p = c.getPlaceholder(1)
      let line = await nvim.line
      let marker = await c.insertSnippet(p, '${1:x} `!p snip.rv = t[1]`', ['', ''], { line, range: Range.create(0, 0, 0, 3) }) as Placeholder
      p = c.getPlaceholder(marker.index)
      expect(c.text).toBe('x x\nx x')
      let source = new CancellationTokenSource()
      let res = await c.updatePlaceholder(p, Position.create(0, 1), 'bar', source.token)
      expect(res.text).toBe('bar bar\nbar bar')
      await executePythonCode(nvim, [`snip = ContextSnippet()`])
      let val = await nvim.call('pyxeval', 'snip.last_placeholder.current_text')
      expect(val).toBe('foo')
    })

    it('should insert python snippet to normal snippet', async () => {
      let c = await createSnippet('${1:foo}\n$1', {})
      let p = c.getPlaceholder(1)
      expect(c.hasPython).toBe(false)
      let marker = await c.insertSnippet(p, '${1:x} `!p snip.rv = t[1]`', ['', ''], { line: '', range: Range.create(0, 0, 0, 3) }) as Placeholder
      p = c.getPlaceholder(marker.index)
      expect(c.text).toBe('x x\nx x')
      let source = new CancellationTokenSource()
      let res = await c.updatePlaceholder(p, Position.create(0, 1), 'bar', source.token)
      expect(res.text).toBe('bar bar\nbar bar')
      expect(c.hasPython).toBe(true)
    })

    it('should not change match for original placeholders', async () => {
      let c = await createSnippet('`!p snip.rv = match.group(1)` $1', {
        regex: '^(\\w+)'
      }, Range.create(0, 0, 0, 3), 'foo')
      let p = c.getPlaceholder(1)
      expect(c.hasPython).toBe(true)
      expect(c.text).toBe('foo ')
      await c.insertSnippet(p, '`!p snip.rv = match.group(1)`', ['', ''], {
        regex: '^(\\w+)',
        line: 'bar',
        range: Range.create(0, 0, 0, 3)
      })
      expect(c.text).toBe('foo bar')
    })
  })

  describe('utils', () => {
    function assertThrow(fn: () => void) {
      let err
      try {
        fn()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    }

    it('should check shouldFormat', () => {
      expect(shouldFormat(' f')).toBe(true)
      expect(shouldFormat('a\nb')).toBe(true)
      expect(shouldFormat('foo')).toBe(false)
    })

    it('should normalizeSnippetString', () => {
      expect(normalizeSnippetString('a\n\n\tb', '  ', {
        insertSpaces: true,
        tabSize: 2
      })).toBe('a\n\n    b')
      expect(normalizeSnippetString('a\n\n  b', '\t', {
        insertSpaces: false,
        tabSize: 2
      })).toBe('a\n\n\t\tb')
    })

    it('should throw for invalid regex', async () => {
      assertThrow(() => {
        convertRegex('\\z')
      })
      assertThrow(() => {
        convertRegex('(?s)')
      })
      assertThrow(() => {
        convertRegex('(?x)')
      })
      assertThrow(() => {
        convertRegex('a\nb')
      })
      assertThrow(() => {
        convertRegex('(<)?(\\w+@\\w+(?:\\.\\w+)+)(?(1)>|$)')
      })
      assertThrow(() => {
        convertRegex('(<)?(\\w+@\\w+(?:\\.\\w+)+)(?(1)>|)')
      })
    })

    it('should convert regex', async () => {
      // \\A
      expect(convertRegex('\\A')).toBe('^')
      expect(convertRegex('f(?#abc)b')).toBe('fb')
      expect(convertRegex('f(?P<abc>def)b')).toBe('f(?<abc>def)b')
      expect(convertRegex('f(?P=abc)b')).toBe('f\\k<abc>b')
    })

    it('should catch error with executePythonCode', async () => {
      let fn = async () => {
        await executePythonCode(nvim, ['INVALID_CODE'])
      }
      await expect(fn()).rejects.toThrow(Error)
    })

    it('should set error with addPythonTryCatch', async () => {
      let code = addPythonTryCatch('INVALID_CODE', true)
      await nvim.command(`pyx ${code}`)
      let msg = await nvim.getVar('errmsg')
      expect(msg).toBeDefined()
      expect(msg).toMatch('INVALID_CODE')
    })

    it('should parse comments', async () => {
      expect(parseCommentstring('a%sb')).toBeUndefined()
      expect(parseCommentstring('// %s')).toBe('//')
      expect(parseComments('')).toEqual({
        start: undefined,
        end: undefined,
        single: undefined
      })
      expect(parseComments('s:/*')).toEqual({
        start: '/*',
        end: undefined,
        single: undefined
      })
      expect(parseComments('e:*/')).toEqual({
        end: '*/',
        start: undefined,
        single: undefined
      })
      expect(parseComments(':#,:b')).toEqual({
        end: undefined,
        start: undefined,
        single: '#'
      })
    })

    it('should reduce TextEdit', () => {
      let e: TextEdit
      e = TextEdit.replace(Range.create(0, 0, 0, 3), 'foo')
      expect(reduceTextEdit(e, '')).toEqual(e)
      e = TextEdit.replace(Range.create(0, 0, 0, 3), 'foo\nbar')
      expect(reduceTextEdit(e, 'bar')).toEqual(
        TextEdit.replace(Range.create(0, 0, 0, 0), 'foo\n')
      )
      e = TextEdit.replace(Range.create(0, 0, 0, 3), 'foo\nbar')
      expect(reduceTextEdit(e, 'foo')).toEqual(
        TextEdit.replace(Range.create(0, 3, 0, 3), '\nbar')
      )
      e = TextEdit.replace(Range.create(0, 0, 0, 3), 'def')
      expect(reduceTextEdit(e, 'daf')).toEqual(
        TextEdit.replace(Range.create(0, 1, 0, 2), 'e')
      )
      e = TextEdit.replace(Range.create(2, 0, 3, 0), 'ascii ascii bar\n')
      expect(reduceTextEdit(e, 'xyz ascii bar\n')).toEqual(
        TextEdit.replace(Range.create(2, 0, 2, 3), 'ascii')
      )
    })

    it('should get new end position', () => {
      let assert = (pos: Position, oldText: string, newText: string, res: Position) => {
        expect(getEndPosition(pos, createTextDocument(oldText), createTextDocument(newText))).toEqual(res)
      }
      assert(Position.create(0, 0), 'foo', 'bar', undefined)
      assert(Position.create(0, 0), 'foo\nbar', 'bar', undefined)
      assert(Position.create(0, 0), 'foo\nbar', 'x\nfoo\nba', undefined)
      assert(Position.create(0, 0), 'foo\nbar', 'x\nfoo\nbar', Position.create(1, 0))
      assert(Position.create(0, 0), 'foo', 'foo', Position.create(0, 0))
    })

    it('should check content before position', () => {
      let assert = (pos: Position, oldText: string, newText: string, res: boolean) => {
        expect(checkContentBefore(pos, createTextDocument(oldText), createTextDocument(newText))).toBe(res)
      }
      assert(Position.create(1, 0), 'foo\nbar', 'foo', true)
      assert(Position.create(1, 1), 'foo\nbar', 'foo', false)
      assert(Position.create(2, 0), 'foo\nbar\n', 'foo', false)
      assert(Position.create(1, 1), 'foo\nbar', 'foo\nbd', true)
      assert(Position.create(1, 1), 'foo\nbar', 'foo\nab', false)
      assert(Position.create(1, 1), 'foo\nbar', 'aoo\nbb', false)
    })

    it('should getParts by range', async () => {
      expect(getParts('abcdef', Range.create(1, 5, 1, 11), Range.create(1, 6, 1, 10))).toEqual(['a', 'f'])
      expect(getParts('abc\nfoo\ndef', Range.create(0, 5, 2, 3), Range.create(1, 1, 1, 2))).toEqual(['abc\nf', 'o\ndef'])
      expect(getParts('abc\ndef', Range.create(0, 1, 2, 3), Range.create(0, 1, 2, 3))).toEqual(['', ''])
    })
  })
})
