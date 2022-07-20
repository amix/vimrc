import { Neovim } from '@chemzqm/neovim'
import { Disposable, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import SelectionRange from '../../handler/selectionRange'
import languages from '../../languages'
import workspace from '../../workspace'
import window from '../../window'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
let selection: SelectionRange

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  selection = helper.plugin.getHandler().selectionRange
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  disposables = []
})

describe('selectionRange', () => {
  describe('getSelectionRanges()', () => {
    it('should throw error when selectionRange provider does not exist', async () => {
      let doc = await helper.createDocument()
      await doc.synchronize()
      let err
      try {
        await selection.getSelectionRanges()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should return ranges', async () => {
      await helper.createDocument()
      disposables.push(languages.registerSelectionRangeProvider([{ language: '*' }], {
        provideSelectionRanges: _doc => {
          return [{
            range: Range.create(0, 0, 0, 1)
          }]
        }
      }))
      let res = await selection.getSelectionRanges()
      expect(res).toBeDefined()
      expect(Array.isArray(res)).toBe(true)
    })
  })

  describe('selectRange()', () => {
    async function getSelectedRange(): Promise<Range> {
      let m = await nvim.mode
      expect(m.mode).toBe('v')
      let bufnr = await nvim.call('bufnr', ['%'])
      await nvim.input('<esc>')
      let doc = workspace.getDocument(bufnr)
      let res = await window.getSelectedRange('v')
      return res
    }

    it('should select ranges forward', async () => {
      let doc = await helper.createDocument()
      let called = 0
      await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'foo\nbar\ntest\n')])
      await nvim.call('cursor', [1, 1])
      disposables.push(languages.registerSelectionRangeProvider([{ language: '*' }], {
        provideSelectionRanges: _doc => {
          called += 1
          let arr = [{
            range: Range.create(0, 0, 0, 1)
          }, {
            range: Range.create(0, 0, 0, 3)
          }, {
            range: Range.create(0, 0, 1, 3)
          }]
          return arr
        }
      }))
      await doc.synchronize()
      await selection.selectRange('', false)
      await selection.selectRange('', true)
      expect(called).toBe(1)
      let res = await getSelectedRange()
      expect(res).toEqual(Range.create(0, 0, 0, 1))
      await selection.selectRange('v', true)
      expect(called).toBe(2)
      res = await getSelectedRange()
      expect(res).toEqual(Range.create(0, 0, 0, 3))
      await selection.selectRange('v', true)
      expect(called).toBe(3)
      res = await getSelectedRange()
      expect(res).toEqual(Range.create(0, 0, 1, 3))
      await selection.selectRange('v', true)
      expect(called).toBe(4)
      let m = await nvim.mode
      expect(m.mode).toBe('n')
    })

    it('should select ranges backward', async () => {
      let doc = await helper.createDocument()
      await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'foo\nbar\ntest\n')])
      await nvim.call('cursor', [1, 1])
      disposables.push(languages.registerSelectionRangeProvider([{ language: '*' }], {
        provideSelectionRanges: _doc => {
          let arr = [{
            range: Range.create(0, 0, 0, 1)
          }, {
            range: Range.create(0, 0, 0, 3)
          }, {
            range: Range.create(0, 0, 1, 3)
          }]
          return arr
        }
      }))
      await doc.synchronize()
      await selection.selectRange('', true)
      let mode = await nvim.call('mode')
      expect(mode).toBe('v')
      await nvim.input('<esc>')
      await window.selectRange(Range.create(0, 0, 1, 3))
      await nvim.input('<esc>')
      await selection.selectRange('v', false)
      let r = await getSelectedRange()
      expect(r).toEqual(Range.create(0, 0, 0, 3))
      await nvim.input('<esc>')
      await selection.selectRange('v', false)
      r = await getSelectedRange()
      expect(r).toEqual(Range.create(0, 0, 0, 1))
      await nvim.input('<esc>')
      await selection.selectRange('v', false)
      mode = await nvim.call('mode')
      expect(mode).toBe('n')
    })
  })
})
