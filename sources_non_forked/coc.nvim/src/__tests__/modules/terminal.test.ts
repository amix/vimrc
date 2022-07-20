import { Neovim } from '@chemzqm/neovim'
import helper from '../helper'
import TerminalModel from '../../model/terminal'

let nvim: Neovim
let terminal: TerminalModel
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  terminal = new TerminalModel('sh', [], nvim)
  await terminal.start(__dirname, { COC_TERMINAL: `option '-term'` })
})

afterAll(async () => {
  terminal.dispose()
  await helper.shutdown()
})

describe('terminal properties', () => {
  it('should get name', () => {
    let name = terminal.name
    expect(name).toBe('sh')
  })

  it('should have correct cwd and env', async () => {
    let bufnr = terminal.bufnr
    terminal.sendText('echo $PWD')
    await helper.wait(300)
    let lines = await nvim.call('getbufline', [bufnr, 1, '$']) as string[]
    expect(lines[0].trim().length).toBeGreaterThan(0)
    terminal.sendText('echo $COC_TERMINAL')
    await helper.wait(300)
    lines = await nvim.call('getbufline', [bufnr, 1, '$']) as string[]
    expect(lines.includes(`option '-term'`)).toBe(true)
  })

  it('should get pid', async () => {
    let pid = await terminal.processId
    expect(typeof pid).toBe('number')
  })

  it('should hide terminal window', async () => {
    await terminal.hide()
    let winnr = await nvim.call('bufwinnr', terminal.bufnr)
    expect(winnr).toBe(-1)
  })

  it('should show terminal window', async () => {
    await terminal.show()
    let winnr = await nvim.call('bufwinnr', terminal.bufnr)
    expect(winnr != -1).toBe(true)
  })
})
