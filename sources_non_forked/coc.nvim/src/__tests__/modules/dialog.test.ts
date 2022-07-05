import { Neovim } from '@chemzqm/neovim'
import Dialog, { DialogButton } from '../../model/dialog'
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

describe('Dialog module', () => {
  it('should show dialog', async () => {
    let dialog = new Dialog(nvim, { content: '你好' })
    await dialog.show({})
    let winid = await dialog.winid
    let win = nvim.createWindow(winid)
    let width = await win.width
    expect(width).toBe(4)
    await nvim.call('coc#float#close', [winid])
  })

  it('should invoke callback with index -1', async () => {
    let callback = jest.fn()
    let dialog = new Dialog(nvim, { content: '你好', callback })
    await dialog.show({})
    let winid = await dialog.winid
    await nvim.call('coc#float#close', [winid])
    await helper.wait(50)
    expect(callback).toHaveBeenCalledWith(-1)
  })

  it('should invoke callback on click', async () => {
    let callback = jest.fn()
    let buttons: DialogButton[] = [{
      index: 0,
      text: 'yes'
    }, {
      index: 1,
      text: 'no'
    }]
    let dialog = new Dialog(nvim, { content: '你好', buttons, callback })
    await dialog.show({})
    let winid = await dialog.winid
    let btnwin = await nvim.call('coc#float#get_related', [winid, 'buttons'])
    await nvim.call('win_gotoid', [btnwin])
    await nvim.call('cursor', [2, 1])
    await nvim.call('coc#float#nvim_float_click', [])
    await helper.wait(50)
    expect(callback).toHaveBeenCalledWith(0)
  })
})
