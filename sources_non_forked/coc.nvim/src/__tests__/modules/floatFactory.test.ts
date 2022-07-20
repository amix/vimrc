import { Neovim } from '@chemzqm/neovim'
import FloatFactory from '../../model/floatFactory'
import snippetManager from '../../snippets/manager'
import { Documentation } from '../../types'
import helper from '../helper'

let nvim: Neovim
let floatFactory: FloatFactory
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  floatFactory = new FloatFactory(nvim)
})

afterAll(async () => {
  await helper.shutdown()
  floatFactory.dispose()
})

afterEach(async () => {
  floatFactory.close()
  await helper.reset()
})

describe('FloatFactory', () => {
  describe('show()', () => {
    it('should show window', async () => {
      expect(floatFactory.window).toBe(null)
      expect(floatFactory.buffer).toBe(null)
      expect(floatFactory.bufnr).toBe(0)
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'.repeat(81)
      }]
      await floatFactory.show(docs, { rounded: true })
      expect(floatFactory.window).toBeDefined()
      expect(floatFactory.buffer).toBeDefined()
      let hasFloat = await nvim.call('coc#float#has_float')
      expect(hasFloat).toBe(1)
      await floatFactory.show([{ filetype: 'txt', content: '' }])
      expect(floatFactory.window).toBe(null)
    })

    it('should create window', async () => {
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'.repeat(81)
      }]
      await floatFactory.create(docs)
      expect(floatFactory.window).toBeDefined()
    })

    it('should catch error on create', async () => {
      let fn = floatFactory.unbind
      floatFactory.unbind = () => {
        throw new Error('bad')
      }
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'.repeat(81)
      }]
      await floatFactory.show(docs)
      floatFactory.unbind = fn
      let msg = await helper.getCmdline()
      expect(msg).toMatch('bad')
    })

    it('should show only one window', async () => {
      await helper.edit()
      await nvim.setLine('foo')
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await Promise.all([
        floatFactory.show(docs),
        floatFactory.show(docs)
      ])
      let count = 0
      let wins = await nvim.windows
      for (let win of wins) {
        let isFloat = await win.getVar('float')
        if (isFloat) count++
      }
      expect(count).toBe(1)
    })

    it('should close window when close called after create', async () => {
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'
      }]
      let p = floatFactory.show(docs)
      await helper.wait(10)
      floatFactory.close()
      await p
      let activated = await floatFactory.activated()
      expect(activated).toBe(false)
    })

    it('should not create on visual mode', async () => {
      await helper.createDocument()
      await nvim.call('cursor', [1, 1])
      await nvim.setLine('foo')
      await nvim.command('normal! v$')
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'
      }]
      await floatFactory.show(docs)
      expect(floatFactory.window).toBe(null)
    })

    it('should allow select mode', async () => {
      await helper.createDocument()
      await snippetManager.insertSnippet('${1:foo}')
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await floatFactory.show(docs)
      let { mode } = await nvim.mode
      expect(mode).toBe('s')
      await nvim.input('<esc>')
    })
  })

  describe('checkRetrigger', () => {
    it('should check retrigger', async () => {
      expect(floatFactory.checkRetrigger(99)).toBe(false)
      let bufnr = await nvim.call('bufnr', ['%'])
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'
      }]
      await floatFactory.show(docs)
      expect(floatFactory.checkRetrigger(99)).toBe(false)
      expect(floatFactory.checkRetrigger(bufnr)).toBe(true)
    })
  })

  describe('options', () => {
    it('should config maxHeight and maxWidth', async () => {
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'f'.repeat(80) + '\nbar',
      }]
      await floatFactory.show(docs, {
        maxWidth: 20,
        maxHeight: 1
      })
      let win = floatFactory.window
      expect(win).toBeDefined()
      let width = await win.width
      let height = await win.height
      expect(width).toBe(19)
      expect(height).toBe(1)
    })

    it('should set border, title, highlight, borderhighlight, cursorline', async () => {
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo\nbar'
      }]
      await floatFactory.show(docs, {
        border: [1, 1, 1, 1],
        title: 'title',
        highlight: 'Pmenu',
        borderhighlight: 'MoreMsg',
        cursorline: true
      })
      let activated = await floatFactory.activated()
      expect(activated).toBe(true)
    })

    it('should respect prefer top', async () => {
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo\nbar'
      }]
      await nvim.call('append', [1, ['', '', '']])
      await nvim.command('exe 4')
      await floatFactory.show(docs, { preferTop: true })
      let win = await helper.getFloat()
      expect(win).toBeDefined()
      let pos = await nvim.call('nvim_win_get_position', [win.id])
      expect(pos).toEqual([1, 0])
    })
  })

  describe('events', () => {
    it('should hide on BufEnter', async () => {
      await helper.edit()
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await floatFactory.show(docs)
      await nvim.command(`edit foo`)
      await helper.waitFor('coc#float#has_float', [], 0)
    })

    it('should hide on CursorMoved', async () => {
      await helper.createDocument()
      await nvim.setLine('foo')
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await floatFactory.show(docs)
      await helper.waitFloat()
      await nvim.input('$')
      await helper.waitFor('coc#float#has_float', [], 0)
    })

    it('should not hide when cursor position not changed', async () => {
      await helper.edit()
      await nvim.setLine('foo')
      let cursor = await nvim.eval("[line('.'), col('.')]")
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await floatFactory.show(docs)
      await nvim.call('cursor', [1, 2])
      await helper.wait(10)
      await nvim.call('cursor', cursor)
      await helper.wait(10)
      await helper.waitFor('coc#float#has_float', [], 1)
    })

    it('should preserve float when autohide disable and not overlap with pum', async () => {
      await helper.createDocument()
      let buf = await nvim.buffer
      await buf.setLines(['foo', '', '', '', 'f'], { start: 0, end: -1, strictIndexing: false })
      await nvim.call('cursor', [5, 2])
      await nvim.input('A')
      let docs: Documentation[] = [{
        filetype: 'markdown',
        content: 'foo'
      }]
      await floatFactory.show(docs, {
        preferTop: true,
        autoHide: false
      })
      let activated = await floatFactory.activated()
      expect(activated).toBe(true)
      await nvim.input('<C-n>')
      await helper.waitPopup()
      activated = await floatFactory.activated()
      expect(activated).toBe(true)
    })
  })
})
