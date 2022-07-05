import { Neovim } from '@chemzqm/neovim'
import os from 'os'
import path from 'path'
import which from 'which'
import Terminals from '../../core/terminals'
import window from '../../window'
import helper from '../helper'

let nvim: Neovim
let terminals: Terminals

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  terminals = new Terminals()
})

afterEach(() => {
  terminals.reset()
})

afterAll(async () => {
  await helper.shutdown()
})

describe('create terminal', () => {
  it('should use cleaned env', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash'),
      strictEnv: true
    })
    await helper.wait(50)
    terminal.sendText(`echo $NODE_ENV`, true)
    await helper.wait(50)
    let buf = nvim.createBuffer(terminal.bufnr)
    let lines = await buf.lines
    expect(lines.includes('test')).toBe(false)
  })

  it('should use custom shell command', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash')
    })
    let bufnr = terminal.bufnr
    let bufname = await nvim.call('bufname', [bufnr]) as string
    expect(bufname.includes('bash')).toBe(true)
  })

  it('should use custom cwd', async () => {
    let basename = path.basename(os.tmpdir())
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      cwd: os.tmpdir()
    })
    let bufnr = terminal.bufnr
    let bufname = await nvim.call('bufname', [bufnr]) as string
    expect(bufname.includes(basename)).toBe(true)
  })

  it('should have exit code', async () => {
    let exitStatus
    terminals.onDidCloseTerminal(terminal => {
      exitStatus = terminal.exitStatus
    })
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash'),
      strictEnv: true
    })
    await helper.wait(50)
    terminal.sendText('exit', true)
    await helper.waitFor('bufloaded', [terminal.bufnr], 0)
    await helper.wait(50)
    expect(exitStatus).toBeDefined()
    expect(exitStatus.code).toBeDefined()
  })

  it('should not throw when show & hide disposed terminal', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash')
    })
    terminal.dispose()
    await terminal.show()
    await terminal.hide()
  })

  it('should show terminal on current window', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash')
    })
    let winid = await nvim.call('bufwinid', [terminal.bufnr])
    expect(winid).toBeGreaterThan(0)
    await nvim.call('win_gotoid', [winid])
    await terminal.show()
  })

  it('should show terminal that shown', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash')
    })
    let res = await terminal.show(true)
    expect(res).toBe(true)
    expect(terminal.bufnr).toBeDefined()
    let winid = await nvim.call('bufwinid', [terminal.bufnr])
    let curr = await nvim.call('win_getid', [])
    expect(winid != curr).toBe(true)
  })

  it('should show hidden terminal', async () => {
    let terminal = await terminals.createTerminal(nvim, {
      name: 'test',
      shellPath: which.sync('bash')
    })
    await terminal.hide()
    await helper.wait(30)
    let res = await terminal.show()
    expect(res).toBe(true)
  })

  it('should create terminal', async () => {
    let terminal = await window.createTerminal({
      name: 'test',
    })
    expect(terminal).toBeDefined()
    expect(terminal.processId).toBeDefined()
    expect(terminal.name).toBeDefined()
    terminal.dispose()
    await helper.wait(30)
    expect(terminal.exitStatus).toEqual({ code: undefined })
  })
})
