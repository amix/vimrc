import { Neovim } from '@chemzqm/neovim'
import events from '../../events'
import helper from '../helper'

function wait(ms: number): Promise<void> {
  return new Promise(resolve => {
    setTimeout(() => {
      resolve()
    }, ms)
  })
}
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

describe('attach', () => {

  it('should listen CocInstalled', async () => {
    nvim.emit('notification', 'VimEnter')
    await helper.wait(100)
  })

  it('should not throw on event handler error', async () => {
    events.on('CursorHold', async () => {
      throw new Error('error')
    })
    let fn = jest.fn()
    nvim.emit('request', 'CocAutocmd', ['CursorHold'], {
      send: fn
    })
    await wait(100)
    expect(fn).toBeCalled()
  })

  it('should not throw when plugin method not found', async () => {
    let fn = jest.fn()
    nvim.emit('request', 'NotExists', [], {
      send: fn
    })
    await wait(100)
    expect(fn).toBeCalled()
  })
})
