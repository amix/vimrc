import { Neovim } from '@chemzqm/neovim'
import Plugin from '../plugin'
import helper from './helper'

let nvim: Neovim
let plugin: Plugin
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  plugin = helper.plugin
})

describe('Helper', () => {
  it('should setup', () => {
    expect(nvim).toBeTruthy()
    expect(plugin.isReady).toBeTruthy()
  })
})

afterAll(async () => {
  await helper.shutdown()
})
