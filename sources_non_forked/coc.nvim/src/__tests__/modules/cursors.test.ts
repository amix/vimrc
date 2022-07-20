import { Neovim } from '@chemzqm/neovim'
import { Position, Range, TextEdit } from 'vscode-languageserver-types'
import Cursors from '../../cursors'
import CursorsSession, { surrondChanges } from '../../cursors/session'
import TextRange from '../../cursors/textRange'
import { getChange, getDelta, isSurrondChange, isTextChange, SurrondChange, TextChange } from '../../cursors/util'
import workspace from '../../workspace'
import helper from '../helper'

let nvim: Neovim
let cursors: Cursors
let ns: number

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  ns = await nvim.createNamespace('coc-cursors')
  cursors = new Cursors(nvim)
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  nvim.pauseNotification()
  cursors.reset()
  await nvim.resumeNotification()
  await helper.reset()
})

async function rangeCount(): Promise<number> {
  let buf = await nvim.buffer
  let markers = await helper.getMarkers(buf.id, ns)
  return markers.length
}

describe('cursors', () => {
  describe('surrondChanges()', () => {
    it('should check surrond changes', async () => {
      expect(surrondChanges([], 0)).toBe(false)
      expect(surrondChanges([{ offset: 1, add: 'f' }, { offset: 3, add: 'f' }], 0)).toBe(false)
    })
  })

  describe('getDelta()', () => {
    it('should get delta count', async () => {
      expect(getDelta({ prepend: [1, 'foo'], append: [1, 'bar'] })).toBe(4)
      expect(getDelta({ offset: 0, remove: 2, insert: 'foo' })).toBe(1)
    })
  })

  describe('getChange()', () => {
    it('should get surrond change', async () => {
      const getText = (newText: string): string => {
        let r = new TextRange(0, 0, 'foo')
        let res = getChange(r, Range.create(0, 0, 0, 3), newText) as SurrondChange
        expect(isSurrondChange(res)).toBe(true)
        r.applySurrondChange(res)
        return r.text
      }
      expect(getText('"foo"')).toBe('"foo"')
      expect(getText('o')).toBe('o')
      expect(getText('')).toBe('')
    })

    it('should get end change', async () => {
      const getText = (character: number, newText: string) => {
        let start = Position.create(0, character)
        let r = new TextRange(0, 0, 'foo')
        let res = getChange(r, Range.create(start, r.range.end), newText) as TextChange
        expect(isTextChange(res)).toBe(true)
        r.applyTextChange(res)
        return r.text
      }
      expect(getText(3, 'bar')).toBe('foobar')
      expect(getText(1, '')).toBe('f')
      expect(getText(2, 'ba')).toBe('foba')
    })

    it('should get normal change', async () => {
      const getText = (start: number, end: number, newText: string) => {
        let r = new TextRange(0, 0, 'foo')
        let res = getChange(r, Range.create(0, start, 0, end), newText) as TextChange
        expect(isTextChange(res)).toBe(true)
        r.applyTextChange(res)
        return r.text
      }
      expect(getText(0, 0, 'a')).toBe('afoo')
      expect(getText(0, 1, '')).toBe('oo')
      expect(getText(0, 2, 'ba')).toBe('bao')
    })
  })

  describe('cancel()', () => {
    it('should cancel cursors session', async () => {
      cursors.cancel(999)
      let doc = await workspace.document
      cursors.cancel(doc.bufnr)
      await nvim.call('setline', [1, ['a', 'b']])
      await nvim.call('cursor', [1, 1])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'position', 'n')
      let activated = await cursors.isActivated()
      expect(activated).toBe(true)
      cursors.cancel(doc.bufnr)
      activated = await cursors.isActivated()
      expect(activated).toBe(false)
    })
  })

  describe('select()', () => {
    it('should throw with unsupported kind', async () => {
      let doc = await workspace.document
      let fn = async () => {
        await cursors.select(doc.bufnr, 'undefined', 'n')
      }
      await expect(fn()).rejects.toThrow(/not supported/)
    })

    it('should select by position', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['a', 'b']])
      await nvim.call('cursor', [1, 1])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'position', 'n')
      await helper.wait(30)
      let n = await rangeCount()
      expect(n).toBe(1)
      await nvim.setOption('virtualedit', 'onemore')
      await nvim.call('cursor', [2, 2])
      await cursors.select(doc.bufnr, 'position', 'n')
      n = await rangeCount()
      expect(n).toBe(2)
      await cursors.select(doc.bufnr, 'position', 'n')
      n = await rangeCount()
      expect(n).toBe(1)
    })

    it('should select by word', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo', 'bar']])
      await nvim.call('cursor', [1, 1])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'word', 'n')
      let n = await rangeCount()
      expect(n).toBe(1)
      await nvim.call('cursor', [2, 2])
      await cursors.select(doc.bufnr, 'word', 'n')
      n = await rangeCount()
      expect(n).toBe(2)
      await cursors.select(doc.bufnr, 'word', 'n')
      n = await rangeCount()
      expect(n).toBe(1)
    })

    it('should toggle select', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo', 'bar']])
      await nvim.call('cursor', [1, 1])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'word', 'n')
      let n = await rangeCount()
      expect(n).toBe(1)
      await cursors.select(doc.bufnr, 'word', 'n')
      n = await rangeCount()
      expect(n).toBe(0)
      let activated = await doc.buffer.getVar('coc_cursors_activated')
      expect(activated).toBe(0)
    })

    it('should select last character', async () => {
      let doc = await workspace.document
      await nvim.setOption('virtualedit', 'onemore')
      await nvim.call('setline', [1, ['}', '{']])
      await nvim.call('cursor', [1, 2])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'word', 'n')
      let n = await rangeCount()
      expect(n).toBe(1)
      await nvim.call('cursor', [2, 1])
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'word', 'n')
      n = await rangeCount()
      expect(n).toBe(2)
    })

    it('should select by visual range', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['"foo"', '"bar"']])
      await nvim.call('cursor', [1, 1])
      await nvim.command('normal! vE')
      await doc.synchronize()
      await cursors.select(doc.bufnr, 'range', 'v')
      let n = await rangeCount()
      expect(n).toBe(1)
      await nvim.call('cursor', [2, 1])
      await nvim.command('normal! vE')
      await cursors.select(doc.bufnr, 'range', 'v')
      n = await rangeCount()
      expect(n).toBe(2)
      await cursors.select(doc.bufnr, 'range', 'v')
      n = await rangeCount()
      expect(n).toBe(1)
    })

    it('should select visual blocks', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['let x = "foo"', 'let y = "bar"']])
      await doc.synchronize()
      await nvim.call('cursor', [1, 1])
      await nvim.input('<C-v>')
      await nvim.input('je')
      await cursors.select(doc.bufnr, 'range', '\x16')
      let n = await rangeCount()
      expect(n).toBe(2)
    })

    it('should select by operator', async () => {
      await nvim.command('nmap x  <Plug>(coc-cursors-operator)')
      await nvim.call('setline', [1, ['"short"', '"long"']])
      await nvim.call('cursor', [1, 2])
      await nvim.input('xa"')
      await helper.wait(30)
      await nvim.call('cursor', [2, 2])
      await nvim.input('xa"')
      await helper.wait(30)
      await nvim.command('nunmap x')
    })
  })

  describe('addRanges()', () => {
    it('should add ranges', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo foo foo', 'bar bar']])
      await doc.synchronize()
      let ranges = [
        Range.create(0, 0, 0, 3),
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
        Range.create(1, 0, 1, 3),
        Range.create(1, 4, 1, 7)
      ]
      await cursors.addRanges(ranges)
      let n = await rangeCount()
      expect(n).toBe(5)
    })
  })

  describe('validChange()', () => {
    it('should check valid change', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo', 'foo', '']])
      await doc.synchronize()
      let ranges = [
        Range.create(0, 0, 0, 3),
        Range.create(1, 0, 1, 3),
      ]
      await cursors.addRanges(ranges)
      let session = cursors.getSession(doc.bufnr)
      expect(session.validChange(Range.create(0, 0, 1, 0), '')).toBe(false)
      expect(session.validChange(Range.create(0, 0, 2, 0), '\n\n')).toBe(false)
      expect(session.validChange(Range.create(1, 0, 1, 3), 'bar')).toBe(false)
    })
  })

  describe('onChange()', () => {
    let session: CursorsSession

    function edit(sl: number, sc: number, el: number, ec: number, text: string): TextEdit {
      let r = Range.create(sl, sc, el, ec)
      return TextEdit.replace(r, text)
    }

    async function assertEdits(edits: TextEdit[], characters: number[], line?: string) {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo foo foo', '']])
      await doc.synchronize()
      let ranges = [
        Range.create(0, 0, 0, 3),
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
      ]
      await cursors.addRanges(ranges)
      session = cursors.getSession(doc.bufnr)
      let p = new Promise(resolve => {
        let disposable = session.onDidUpdate(() => {
          disposable.dispose()
          resolve(undefined)
        })
        void doc.applyEdits(edits)
      })
      await p
      if (line != null) {
        expect(doc.getline(0)).toBe(line)
      }
      let arr: number[] = []
      session.currentRanges.forEach(r => {
        arr.push(r.start.character, r.end.character)
      })
      expect(arr).toEqual(characters)
      session.cancel()
    }

    it('should adjust on text insert', async () => {
      await assertEdits([edit(0, 0, 0, 0, 'bar\n')], [0, 3, 4, 7, 8, 11])
      await assertEdits([edit(0, 0, 0, 0, 'b')], [0, 4, 5, 9, 10, 14], 'bfoo bfoo bfoo')
      await assertEdits([edit(0, 1, 0, 1, 'b')], [0, 4, 5, 9, 10, 14], 'fboo fboo fboo')
      await assertEdits([edit(0, 3, 0, 3, 'b')], [0, 4, 5, 9, 10, 14], 'foob foob foob')
      await assertEdits([edit(0, 3, 0, 4, '\n')], [0, 3, 0, 3, 4, 7], 'foo')
      await assertEdits([edit(1, 0, 1, 0, 'bar')], [0, 3, 4, 7, 8, 11])
      await nvim.call('setline', [1, ['foo foo foo', '']])
      await nvim.call('cursor', [1, 4])
      await assertEdits([edit(0, 8, 0, 8, 'b')], [0, 4, 5, 9, 10, 14], 'bfoo bfoo bfoo')
      let col = await nvim.call('col', ['.'])
      expect(col).toBe(5)
    })

    it('should adjust on text detete', async () => {
      await assertEdits([edit(0, 2, 0, 3, '')], [0, 2, 3, 5, 6, 8], 'fo fo fo')
      await assertEdits([edit(0, 3, 0, 4, '')], [0, 3, 3, 6, 7, 10], 'foofoo foo')
      await assertEdits([edit(0, 4, 0, 7, '')], [0, 0, 1, 1, 2, 2], '  ')
      await nvim.setLine('foo foo')
      await nvim.call('cursor', [1, 4])
      await assertEdits([edit(0, 3, 0, 7, '')], [0, 3, 4, 7], 'foo foo')
      await assertEdits([edit(0, 1, 0, 11, '')], [], 'f')
    })

    it('should adjust on text change', async () => {
      await assertEdits([edit(0, 0, 0, 0, '"'), edit(0, 3, 0, 3, '"')], [0, 5, 6, 11, 12, 17], '"foo" "foo" "foo"')
      await assertEdits([edit(0, 0, 0, 1, 'b')], [0, 3, 4, 7, 8, 11], 'boo boo boo')
      await assertEdits([edit(0, 0, 0, 3, 'ba')], [0, 2, 3, 5, 6, 8], 'ba ba ba')
      await nvim.call('setline', [1, ['', '']])
      await nvim.call('cursor', [2, 1])
      await assertEdits([edit(0, 4, 0, 5, 'ba')], [0, 4, 5, 9, 10, 14], 'baoo baoo baoo')
      let col = await nvim.call('col', ['.'])
      expect(col).toBe(1)
    })

    it('should adjust on undo & redo', async () => {
      let doc = await workspace.document
      let edits = [edit(0, 0, 0, 0, '"'), edit(0, 3, 0, 3, '"')]
      await nvim.call('setline', [1, ['foo foo foo', '']])
      await doc.synchronize()
      let ranges = [
        Range.create(0, 0, 0, 3),
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
      ]
      await cursors.addRanges(ranges)
      session = cursors.getSession(doc.bufnr)
      let p = new Promise(resolve => {
        let disposable = session.onDidUpdate(() => {
          disposable.dispose()
          resolve(undefined)
        })
        void doc.applyEdits(edits)
      })
      await p
      await nvim.command('undo')
      await helper.wait(50)
      expect(session.currentRanges).toEqual(ranges)
    })

    it('should highlight on empty content change', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['foo', '']])
      await doc.synchronize()
      let ranges = [Range.create(0, 0, 0, 3)]
      await cursors.addRanges(ranges)
      session = cursors.getSession(doc.bufnr)
      await nvim.call('setline', [1, ['foo', '']])
      await doc.synchronize()
      let c = await rangeCount()
      expect(c).toBe(1)
    })
  })

  describe('applyComposedEdit()', () => {
    async function setup(): Promise<CursorsSession> {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['bar foo foo', 'foo']])
      await doc.synchronize()
      let session = cursors.createSession(doc)
      session.addRanges([
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
        Range.create(1, 0, 1, 3),
      ])
      return session
    }

    it('should check change before first range', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['abc foob foob', 'foob'])
      expect(res).toBe(false)
    })

    it('should check change of first range', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar foo foob', 'foob'])
      expect(res).toBe(false)
    })

    it('should check delete exceed range', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar fofoo', 'foo'])
      expect(res).toBe(false)
    })

    it('should check content prepend', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar bfoo bfoo', 'bfoo'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 8),
        Range.create(0, 9, 0, 13),
        Range.create(1, 0, 1, 4),
      ])
      s = await setup()
      doc = await workspace.document
      res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar bfoo bfoo', 'xfoo'])
      expect(res).toBe(false)
    })

    it('should check content insert', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar fboo fboo', 'fboo'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 8),
        Range.create(0, 9, 0, 13),
        Range.create(1, 0, 1, 4),
      ])
    })

    it('should check content append', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar foob foob', 'foob'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 8),
        Range.create(0, 9, 0, 13),
        Range.create(1, 0, 1, 4),
      ])
    })

    it('should check content detete #1', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar oo oo', 'oo'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 6),
        Range.create(0, 7, 0, 9),
        Range.create(1, 0, 1, 2),
      ])
    })

    it('should check content delete #2', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar  ', ''])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 4),
        Range.create(0, 5, 0, 5),
        Range.create(1, 0, 1, 0),
      ])
    })

    it('should check content delete #3', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar fo fo', 'fo'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 6),
        Range.create(0, 7, 0, 9),
        Range.create(1, 0, 1, 2),
      ])
    })

    it('should check content change #1', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar fa fa', 'fa'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 6),
        Range.create(0, 7, 0, 9),
        Range.create(1, 0, 1, 2),
      ])
    })

    it('should check content change #1', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar fa fa', 'fa'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 6),
        Range.create(0, 7, 0, 9),
        Range.create(1, 0, 1, 2),
      ])
    })

    it('should check content change #2', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar ab ab', 'ab'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 6),
        Range.create(0, 7, 0, 9),
        Range.create(1, 0, 1, 2),
      ])
    })

    it('should check content change #3', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar xfa xfa', 'xfa'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
        Range.create(1, 0, 1, 3),
      ])
    })

    it('should check content change #4', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar xfao xfao', 'xfao'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 8),
        Range.create(0, 9, 0, 13),
        Range.create(1, 0, 1, 4),
      ])
    })

    it('should check surrond add', async () => {
      let s = await setup()
      let doc = await workspace.document
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar "foo" "foo"', '"foo"'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 9),
        Range.create(0, 10, 0, 15),
        Range.create(1, 0, 1, 5),
      ])
    })

    it('should check surrond remove', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['bar "foo" "foo"', '"foo"']])
      await doc.synchronize()
      let s = cursors.createSession(doc)
      s.addRanges([
        Range.create(0, 4, 0, 9),
        Range.create(0, 10, 0, 15),
        Range.create(1, 0, 1, 5),
      ])
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), ['bar foo foo', 'foo'])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 7),
        Range.create(0, 8, 0, 11),
        Range.create(1, 0, 1, 3),
      ])
    })

    it('should check surrond change', async () => {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['bar "foo" "foo"', '"foo"']])
      await doc.synchronize()
      let s = cursors.createSession(doc)
      s.addRanges([
        Range.create(0, 4, 0, 9),
        Range.create(0, 10, 0, 15),
        Range.create(1, 0, 1, 5),
      ])
      let res = s.applyComposedEdit(doc.textDocument.lines.slice(), [`bar 'foo' 'foo'`, `'foo'`])
      expect(res).toBe(true)
      expect(s.currentRanges).toEqual([
        Range.create(0, 4, 0, 9),
        Range.create(0, 10, 0, 15),
        Range.create(1, 0, 1, 5),
      ])
    })
  })

  describe('key mappings', () => {
    async function setup(): Promise<void> {
      let doc = await workspace.document
      await nvim.call('setline', [1, ['a', 'b', 'c']])
      await doc.synchronize()
      let session = cursors.createSession(doc)
      session.addRanges([
        Range.create(0, 0, 0, 1),
        Range.create(1, 0, 1, 1),
        Range.create(2, 0, 2, 1),
      ])
    }

    async function hasKeymap(key): Promise<boolean> {
      let buf = await nvim.buffer
      let keymaps = await buf.getKeymap('n') as any
      return keymaps.find(o => o.lhs == key) != null
    }

    it('should setup cancel keymap', async () => {
      await setup()
      let count = await rangeCount()
      expect(count).toBe(3)
      await nvim.input('<esc>')
      await helper.wait(50)
      count = await rangeCount()
      expect(count).toBe(0)
      let has = await hasKeymap('<Esc>')
      expect(has).toBe(false)
    })

    it('should next key wrapscan', async () => {
      await setup()
      await nvim.call('cursor', [1, 1])
      const next = async (line: number, character: number) => {
        await nvim.input('<C-n>')
        await helper.wait(30)
        let cursor = await nvim.call('coc#cursor#position')
        expect(cursor).toEqual([line, character])
      }
      await next(1, 0)
      await next(2, 0)
      await next(0, 0)
    })

    it('should previous key wrapscan', async () => {
      await setup()
      await nvim.call('cursor', [3, 1])
      const prev = async (line: number, character: number) => {
        await nvim.input('<C-p>')
        await helper.wait(30)
        let cursor = await nvim.call('coc#cursor#position')
        expect(cursor).toEqual([line, character])
      }
      await prev(1, 0)
      await prev(0, 0)
      await prev(2, 0)
    })

    it('should next key no wrapscan', async () => {
      helper.updateConfiguration('cursors.wrapscan', false)
      await setup()
      await nvim.call('cursor', [3, 1])
      const next = async (line: number, character: number) => {
        await nvim.input('<C-n>')
        await helper.wait(50)
        let cursor = await nvim.call('coc#cursor#position')
        expect(cursor).toEqual([line, character])
      }
      await next(2, 0)
    })

    it('should previous key no wrapscan', async () => {
      helper.updateConfiguration('cursors.wrapscan', false)
      await setup()
      await nvim.call('cursor', [1, 1])
      const prev = async (line: number, character: number) => {
        await nvim.input('<C-p>')
        await helper.wait(30)
        let cursor = await nvim.call('coc#cursor#position')
        expect(cursor).toEqual([line, character])
      }
      await prev(0, 0)
    })
  })
})
