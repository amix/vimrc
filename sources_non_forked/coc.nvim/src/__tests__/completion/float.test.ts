import { Neovim } from '@chemzqm/neovim'
import Floating from '../../completion/floating'
import sources from '../../sources'
import { CompleteResult, FloatConfig, ISource, SourceType } from '../../types'
import helper from '../helper'

let nvim: Neovim
let source: ISource
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  source = {
    name: 'float',
    priority: 10,
    enable: true,
    sourceType: SourceType.Native,
    doComplete: (): Promise<CompleteResult> => Promise.resolve({
      items: [{
        word: 'foo',
        info: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.'
      }, {
        word: 'foot',
        info: 'foot'
      }, {
        word: 'football',
      }]
    })
  }
  sources.addSource(source)
})

afterAll(async () => {
  sources.removeSource(source)
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
})

describe('completion float', () => {
  it('should not show float window when disabled', async () => {
    helper.updateConfiguration('suggest.floatEnable', false)
    await helper.edit()
    await nvim.input('if')
    await helper.visible('foo', 'float')
    let hasFloat = await nvim.call('coc#float#has_float')
    expect(hasFloat).toBe(0)
  })

  it('should cancel float window', async () => {
    await helper.edit()
    await nvim.input('if')
    await helper.visible('foo', 'float')
    let items = await helper.getItems()
    expect(items[0].word).toBe('foo')
    expect(items[0].info.length > 0).toBeTruthy()
    await helper.selectCompleteItem(0)
    await helper.wait(30)
    let hasFloat = await nvim.call('coc#float#has_float')
    expect(hasFloat).toBe(0)
  })

  it('should adjust float window position', async () => {
    await helper.edit()
    await nvim.setLine(' '.repeat(70))
    await nvim.input('Af')
    await helper.visible('foo', 'float')
    await nvim.input('<C-n>')
    await helper.wait(100)
    let floatWin = await helper.getFloat()
    let config = await floatWin.getConfig()
    expect(config.col + config.width).toBeLessThan(180)
  })

  it('should redraw float window on item change', async () => {
    await helper.edit()
    await nvim.setLine(' '.repeat(70))
    await nvim.input('Af')
    await helper.visible('foo', 'float')
    await nvim.call('nvim_select_popupmenu_item', [0, false, false, {}])
    await helper.wait(50)
    await nvim.input('<C-n>')
    await helper.wait(100)
    let floatWin = await helper.getFloat()
    let buf = await floatWin.buffer
    let lines = await buf.lines
    expect(lines.length).toBeGreaterThan(0)
    expect(lines[0]).toMatch('foot')
  })

  it('should hide float window when item info is empty', async () => {
    await helper.edit()
    await nvim.setLine(' '.repeat(70))
    await nvim.input('Af')
    await helper.visible('foo', 'float')
    await nvim.call('nvim_select_popupmenu_item', [0, false, false, {}])
    await helper.wait(10)
    await nvim.input('<C-n>')
    await helper.wait(10)
    await nvim.input('<C-n>')
    await helper.wait(100)
    let hasFloat = await nvim.call('coc#float#has_float')
    expect(hasFloat).toBe(0)
  })

  it('should hide float window after completion', async () => {
    await helper.edit()
    await nvim.setLine(' '.repeat(70))
    await nvim.input('Af')
    await helper.visible('foo', 'float')
    await nvim.input('<C-n>')
    await helper.wait(100)
    await nvim.input('<C-y>')
    await helper.wait(30)
    let hasFloat = await nvim.call('coc#float#has_float')
    expect(hasFloat).toBe(0)
  })
})

describe('float config', () => {
  beforeEach(async () => {
    await nvim.setLine('foob foot')
    await nvim.input('of')
    await nvim.input('<C-n>')
  })

  async function createFloat(config: Partial<FloatConfig>, docs = [{ filetype: 'txt', content: 'doc' }], isVim = false): Promise<Floating> {
    let floating = new Floating(nvim, isVim)
    let bounding = { col: 6, row: 2, height: 3, width: 16, scrollbar: false }
    await floating.show(docs, bounding, Object.assign({
      excludeImages: true,
      border: false,
    }, config))
    return floating
  }

  async function getFloat(): Promise<number> {
    let ids = await nvim.call('coc#float#get_float_win_list')
    return Array.isArray(ids) ? ids[0] || -1 : -1
  }

  async function getRelated(winid: number, kind: string): Promise<number> {
    if (!winid || winid == -1) return -1
    let win = nvim.createWindow(winid)
    let related = await win.getVar('related') as number[]
    if (!related || !related.length) return -1
    for (let id of related) {
      let w = nvim.createWindow(id)
      let v = await w.getVar('kind')
      if (v == kind) {
        return id
      }
    }
    return -1
  }

  it('should not shown with empty lines', async () => {
    await createFloat({}, [{ filetype: 'txt', content: '' }])
    let winid = await nvim.call('GetFloatWin')
    expect(winid).toBe(0)
  })

  it('should shown on vim', async () => {
    let float = await createFloat({}, [{ filetype: 'txt', content: 'ff' }], true)
    let winid = await nvim.call('GetFloatWin')
    expect(winid).toBeGreaterThan(0)
    float.close()
  })

  it('should show window with border', async () => {
    await createFloat({ border: true })
    let winid = await getFloat()
    expect(winid).toBeGreaterThan(0)
    let id = await getRelated(winid, 'border')
    expect(id).toBeGreaterThan(0)
  })

  it('should change window highlights', async () => {
    await createFloat({ border: true, highlight: 'WarningMsg', borderhighlight: 'MoreMsg' })
    let winid = await getFloat()
    expect(winid).toBeGreaterThan(0)
    let win = nvim.createWindow(winid)
    let res = await win.getOption('winhl') as string
    expect(res).toMatch('WarningMsg')
    let id = await getRelated(winid, 'border')
    expect(id).toBeGreaterThan(0)
    win = nvim.createWindow(id)
    res = await win.getOption('winhl') as string
    expect(res).toMatch('MoreMsg')
  })

  it('should add shadow and winblend', async () => {
    await createFloat({ shadow: true, winblend: 30 })
    let winid = await getFloat()
    expect(winid).toBeGreaterThan(0)
  })
})
