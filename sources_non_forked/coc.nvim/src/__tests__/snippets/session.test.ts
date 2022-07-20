import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { Position, Range } from 'vscode-languageserver-protocol'
import { UltiSnippetContext } from '../../snippets/eval'
import { shouldCancel, SnippetSession } from '../../snippets/session'
import window from '../../window'
import workspace from '../../workspace'
import helper from '../helper'

let nvim: Neovim
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
})

async function createSession(enableHighlight = false, preferComplete = false): Promise<SnippetSession> {
  let doc = await workspace.document
  return new SnippetSession(nvim, doc, enableHighlight, preferComplete)
}

describe('SnippetSession', () => {
  const defaultRange = Range.create(0, 0, 0, 0)
  const defaultContext = {
    line: '',
    range: defaultRange
  }

  async function start(inserted: string, range = defaultRange, select = true, context?: UltiSnippetContext): Promise<boolean> {
    await nvim.input('i')
    let doc = await workspace.document
    let session = new SnippetSession(nvim, doc)
    return await session.start(inserted, range, select, context)
  }

  async function getCursorRange(): Promise<Range> {
    let pos = await window.getCursorPosition()
    return Range.create(pos, pos)
  }

  describe('start()', () => {

    it('should insert escaped text', async () => {
      let res = await start('\\`a\\` \\$ \\{\\}', Range.create(0, 0, 0, 0), false, defaultContext)
      expect(res).toBe(true)
      let line = await nvim.line
      expect(line).toBe('`a` $ {}')
    })

    it('should not start with plain snippet when jump to final placeholder', async () => {
      let res = await start('bar$0', defaultRange)
      expect(res).toBe(false)
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 3 })
    })

    it('should start with range replaced', async () => {
      await nvim.setLine('foo')
      let res = await start('bar$0', Range.create(0, 0, 0, 3), true)
      expect(res).toBe(false)
      let line = await nvim.line
      expect(line).toBe('bar')
    })

    it('should fix indent of next line when necessary', async () => {
      let buf = await nvim.buffer
      await nvim.setLine('  ab')
      await nvim.input('i')
      let session = await createSession()
      let res = await session.start('${1:x}\n', Range.create(0, 3, 0, 3))
      expect(res).toBe(true)
      let lines = await buf.lines
      expect(lines).toEqual(['  ax', '  b'])
    })

    it('should insert indent for snippet endsWith line break', async () => {
      let buf = await nvim.buffer
      await nvim.setLine('  bar')
      await nvim.command('startinsert')
      await nvim.call('cursor', [1, 3])
      let session = await createSession()
      let res = await session.start('${1:foo}\n', Range.create(0, 2, 0, 2))
      expect(res).toBe(true)
      let lines = await buf.lines
      expect(lines).toEqual(['  foo', '  bar'])
    })

    it('should start without select placeholder', async () => {
      let session = await createSession()
      let res = await session.start(' ${1:aa} ', defaultRange, false)
      expect(res).toBe(true)
      let { mode } = await nvim.mode
      expect(mode).toBe('n')
      await session.selectCurrentPlaceholder()
      await helper.waitFor('mode', [], 's')
    })

    it('should start with variable selected', async () => {
      let session = await createSession()
      let res = await session.start('${foo:bar}', defaultRange, false)
      expect(res).toBe(true)
      let line = await nvim.getLine()
      expect(line).toBe('bar')
      await session.selectCurrentPlaceholder()
      await helper.waitFor('mode', [], 's')
    })

    it('should select none transform placeholder', async () => {
      await start('${1/..*/ -> /}xy$1', defaultRange)
      let col = await nvim.call('col', '.')
      expect(col).toBe(3)
    })

    it('should indent multiple lines variable text', async () => {
      let buf = await nvim.buffer
      let text = 'abc\n  def'
      await nvim.setVar('coc_selected_text', text)
      await start('fun\n  ${0:${TM_SELECTED_TEXT:return}}\nend')
      let lines = await buf.lines
      expect(lines.length).toBe(4)
      expect(lines).toEqual([
        'fun', '  abc', '    def', 'end'
      ])
    })

    it('should resolve VISUAL', async () => {
      let text = 'abc'
      await nvim.setVar('coc_selected_text', text)
      await start('$VISUAL')
      let line = await nvim.line
      expect(line).toBe('abc')
    })

    it('should resolve default value of VISUAL', async () => {
      await nvim.setVar('coc_selected_text', '')
      await start('${VISUAL:foo}')
      let line = await nvim.line
      expect(line).toBe('foo')
    })
  })

  describe('nested snippet', () => {
    it('should start with nest snippet', async () => {
      let session = await createSession()
      let res = await session.start('${1:a} ${2:b}', defaultRange, false)
      let line = await nvim.getLine()
      expect(line).toBe('a b')
      expect(res).toBe(true)
      let { placeholder } = session
      expect(placeholder.index).toBe(1)
      let r = await getCursorRange()
      res = await session.start('${1:foo} ${2:bar}', r)
      expect(res).toBe(true)
      placeholder = session.placeholder
      expect(placeholder.index).toBe(2)
      line = await nvim.getLine()
      expect(line).toBe('foo bara b')
      expect(session.snippet.text).toBe('foo bara b')
      await session.nextPlaceholder()
      placeholder = session.placeholder
      expect(placeholder.index).toBe(3)
      expect(session.placeholder.value).toBe('bar')
      let col = await nvim.call('col', ['.'])
      expect(col).toBe(7)
      await session.nextPlaceholder()
      await session.nextPlaceholder()
      expect(session.placeholder.index).toBe(5)
      expect(session.placeholder.value).toBe('b')
    })

    it('should start nest snippet without select', async () => {
      await nvim.command('startinsert')
      let session = await createSession()
      let res = await session.start('${1:a} ${2:b}', defaultRange)
      let line = await nvim.call('getline', ['.'])
      let r = await getCursorRange()
      res = await session.start('${1:foo} ${2:bar}', r, false)
      expect(res).toBe(true)
      line = await nvim.line
      expect(line).toBe('afoo bar b')
    })
  })

  describe('synchronize()', () => {
    it('should synchronize content change', async () => {
      let pyfile = path.join(__dirname, '../ultisnips.py')
      await nvim.command(`execute 'pyxfile '.fnameescape('${pyfile}')`)
      let session = await createSession(true)
      await session.start('${1:foo}${2:`!p snip.rv = ""`} `!p snip.rv = t[1] + t[2]`', defaultRange, true, {
        line: '',
        range: defaultRange
      })
      await nvim.input('b')
      await helper.wait(20)
      await nvim.input('a')
      await helper.wait(30)
      await nvim.input('r')
      await helper.wait(40)
      await session.forceSynchronize()
      await helper.waitFor('getline', ['.'], 'bar bar')
    })

    it('should cancel when change after snippet', async () => {
      let session = await createSession()
      await nvim.setLine(' x')
      await nvim.input('i')
      await session.start('${1:foo }bar', defaultRange)
      await nvim.setLine('foo bar y')
      await session.forceSynchronize()
      expect(session.isActive).toBe(false)
    })

    it('should cancel when change before and in snippet', async () => {
      let session = await createSession()
      await nvim.setLine(' x')
      await nvim.input('i')
      await session.start('${1:foo }bar', defaultRange)
      await nvim.setLine('afoobar')
      await session.forceSynchronize()
      expect(session.isActive).toBe(false)
    })

    it('should reset position when change before snippet', async () => {
      let session = await createSession()
      await nvim.setLine('x')
      await nvim.input('a')
      let r = await getCursorRange()
      await session.start('${1:foo} bar', r)
      await nvim.setLine('yfoo bar')
      await session.forceSynchronize()
      expect(session.isActive).toBe(true)
      let start = session.snippet.start
      expect(start).toEqual(Position.create(0, 1))
    })

    it('should cancel when before and body changed', async () => {
      let session = await createSession()
      await nvim.setLine('x')
      await nvim.input('a')
      await session.start('${1:foo }bar', defaultRange)
      await nvim.setLine('yfoo  bar')
      await session.forceSynchronize()
      expect(session.isActive).toBe(false)
    })

    it('should cancel when unable to find placeholder', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('${1:foo} bar', defaultRange)
      await nvim.setLine('foodbar')
      await session.forceSynchronize()
      expect(session.isActive).toBe(false)
    })

    it('should cancel when unable to find removed Text', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('${1:foo} bar', defaultRange)
      await nvim.setLine('fobar')
      await session.forceSynchronize()
      expect(session.isActive).toBe(false)
    })

    it('should adjust with removed text', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('${1:foo} bar$0', defaultRange)
      await nvim.input('<esc>')
      await nvim.call('cursor', [1, 5])
      await nvim.input('i')
      await nvim.input('<backspace>')
      await session.forceSynchronize()
      expect(session.isActive).toBe(true)
      await session.nextPlaceholder()
      let col = await nvim.call('col', ['.'])
      expect(col).toBe(7)
    })

    it('should prefer range contains current cursor', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('$1 $2', defaultRange)
      await nvim.input('<esc>A')
      await nvim.input(' ')
      await session.forceSynchronize()
      expect(session.isActive).toBe(true)
      let p = session.placeholder
      expect(p.index).toBe(2)
    })

    it('should update cursor column after synchronize', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('${1} ${1:foo}', defaultRange)
      await nvim.input('b')
      await session.forceSynchronize()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual(Position.create(0, 3))
      await nvim.input('a')
      await session.forceSynchronize()
      pos = await window.getCursorPosition()
      expect(pos).toEqual(Position.create(0, 5))
      await nvim.input('<backspace>')
      await session.forceSynchronize()
      pos = await window.getCursorPosition()
      expect(pos).toEqual(Position.create(0, 3))
    })

    it('should update cursor line after synchronize', async () => {
      let buf = await nvim.buffer
      let session = await createSession()
      await nvim.input('i')
      await session.start('${1} ${1:foo}', defaultRange)
      await nvim.input('b')
      await session.forceSynchronize()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual(Position.create(0, 3))
      await nvim.input('<cr>')
      await session.forceSynchronize()
      expect(session.isActive).toBe(true)
      pos = await window.getCursorPosition()
      let lines = await buf.lines
      expect(lines).toEqual(['b', ' b', ''])
      expect(pos).toEqual(Position.create(2, 0))
    })
  })

  describe('deactivate()', () => {

    it('should deactivate on cursor outside', async () => {
      let buf = await nvim.buffer
      let session = await createSession()
      let res = await session.start('a${1:a}b', defaultRange)
      expect(res).toBe(true)
      await buf.append(['foo', 'bar'])
      await nvim.call('cursor', [2, 1])
      await session.checkPosition()
      expect(session.isActive).toBe(false)
    })

    it('should not throw when jump on deactivate session', async () => {
      let session = await createSession()
      session.deactivate()
      await session.start('${1:foo} $0', defaultRange)
      await session.selectPlaceholder(undefined, true)
      await session.forceSynchronize()
      await session.previousPlaceholder()
      await session.nextPlaceholder()
    })

    it('should cancel keymap on jump final placeholder', async () => {
      let session = await createSession()
      await nvim.input('i')
      await session.start('$0x${1:a}b$0', defaultRange)
      let line = await nvim.line
      expect(line).toBe('xab')
      let map = await nvim.call('maparg', ['<C-j>', 'i']) as string
      expect(map).toMatch('coc#snippet#jump')
      await session.nextPlaceholder()
      map = await nvim.call('maparg', ['<C-j>', 'i']) as string
      expect(map).toBe('')
    })
  })

  describe('nextPlaceholder()', () => {
    it('should not throw when session not activated', async () => {
      let session = await createSession()
      await session.start('${foo} ${bar}', defaultRange, false)
      session.deactivate()
      await session.nextPlaceholder()
      await session.previousPlaceholder()
    })

    it('should jump to variable placeholder', async () => {
      let session = await createSession()
      await session.start('${foo} ${bar}', defaultRange, false)
      await session.selectCurrentPlaceholder()
      await session.nextPlaceholder()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 6 })
    })

    it('should jump to variable placeholder after number placeholder', async () => {
      let session = await createSession()
      await session.start('${foo} ${1:bar}', defaultRange, false)
      await session.selectCurrentPlaceholder()
      await session.nextPlaceholder()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 2 })
    })

    it('should jump to first placeholder', async () => {
      let session = await createSession()
      await session.start('${foo} ${foo} ${2:bar}', defaultRange, false)
      await session.selectCurrentPlaceholder()
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 10 })
      await session.nextPlaceholder()
      pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 2 })
      await session.nextPlaceholder()
      pos = await window.getCursorPosition()
      expect(pos).toEqual({ line: 0, character: 11 })
    })

    it('should goto next placeholder', async () => {
      let session = await createSession()
      let res = await session.start('${1:a} ${2:b} c', defaultRange)
      expect(res).toBe(true)
      await session.nextPlaceholder()
      let { placeholder } = session
      expect(placeholder.index).toBe(2)
    })

    it('should jump to none transform placeholder', async () => {
      let session = await createSession()
      let res = await session.start('${1} ${2/^_(.*)/$2/}bar$2', defaultRange)
      expect(res).toBe(true)
      let line = await nvim.line
      expect(line).toBe(' bar')
      await session.nextPlaceholder()
      let col = await nvim.call('col', '.')
      expect(col).toBe(5)
    })
  })

  describe('previousPlaceholder()', () => {

    it('should goto previous placeholder', async () => {
      let session = await createSession()
      let res = await session.start('${1:foo} ${2:bar}', defaultRange)
      expect(res).toBe(true)
      await session.nextPlaceholder()
      expect(session.placeholder.index).toBe(2)
      await session.previousPlaceholder()
      expect(session.placeholder.index).toBe(1)
    })
  })

  describe('highlights()', () => {
    it('should add highlights', async () => {
      let ns = await nvim.call('coc#highlight#create_namespace', ['snippets'])
      let session = await createSession(true)
      await session.start('${2:bar ${1:foo}} $2', defaultRange)
      let buf = nvim.createBuffer(workspace.bufnr)
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBe(2)
      expect(markers[0][3].hl_group).toBe('CocSnippetVisual')
      expect(markers[1][3].hl_group).toBe('CocSnippetVisual')
      session.deactivate()
    })
  })

  describe('checkPosition()', () => {

    it('should cancel snippet if position out of range', async () => {
      let session = await createSession()
      await nvim.setLine('bar')
      await session.start('${1:foo}', defaultRange)
      await nvim.call('cursor', [1, 5])
      await session.checkPosition()
      expect(session.isActive).toBe(false)
    })

    it('should not cancel snippet if position in range', async () => {
      let session = await createSession()
      await session.start('${1:foo}', defaultRange)
      await nvim.call('cursor', [1, 3])
      await session.checkPosition()
      expect(session.isActive).toBe(true)
    })
  })

  describe('findPlaceholder()', () => {

    it('should find current placeholder if possible', async () => {
      let session = await createSession()
      await session.start('${1:abc}${2:def}', defaultRange)
      let placeholder = session.findPlaceholder(Range.create(0, 3, 0, 3))
      expect(placeholder.index).toBe(1)
    })

    it('should return null if placeholder not found', async () => {
      let session = await createSession()
      await session.start('${1:abc}xyz${2:def}', defaultRange)
      let placeholder = session.findPlaceholder(Range.create(0, 4, 0, 4))
      expect(placeholder).toBeNull()
    })
  })

  describe('selectPlaceholder()', () => {

    it('should select range placeholder', async () => {
      let session = await createSession()
      await session.start('${1:abc}', defaultRange)
      let mode = await nvim.mode
      expect(mode.mode).toBe('s')
      await nvim.input('<backspace>')
      let line = await nvim.line
      expect(line).toBe('')
    })

    it('should select empty placeholder', async () => {
      let session = await createSession()
      await session.start('a ${1} ${2}', defaultRange)
      let mode = await nvim.mode
      expect(mode.mode).toBe('i')
      let col = await nvim.call('col', '.')
      expect(col).toBe(3)
    })

    it('should select choice placeholder', async () => {
      await nvim.input('i')
      let session = await createSession()
      await session.start('${1|one,two,three|}', defaultRange)
      let line = await nvim.line
      expect(line).toBe('one')
      await helper.waitPopup()
      let val = await nvim.eval('g:coc#_context') as any
      expect(val.start).toBe(0)
      expect(val.candidates).toEqual(['one', 'two', 'three'])
    })
  })

  describe('shouldCancel()', () => {
    it('should check cancel', async () => {
      let doc = await helper.createDocument()
      expect(shouldCancel(doc, Position.create(0, 0))).toBe(false)
      await nvim.setLine('foo fo f')
      await doc.synchronize()
      await nvim.input('A')
      await nvim.input('<C-n>')
      await helper.waitPopup()
      expect(shouldCancel(doc, Position.create(0, 3))).toBe(true)
    })
  })
})
