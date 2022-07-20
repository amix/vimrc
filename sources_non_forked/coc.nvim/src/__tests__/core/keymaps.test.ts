import { Neovim } from '@chemzqm/neovim'
import workspace from '../../workspace'
import Keymaps from '../../core/keymaps'
import helper from '../helper'
import { Disposable } from 'vscode-languageserver-protocol'
import { disposeAll } from '../../util'

let nvim: Neovim
let keymaps: Keymaps
let disposables: Disposable[] = []

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  keymaps = workspace.keymaps
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('doKeymap()', () => {
  it('should not throw when key not mapped', async () => {
    await keymaps.doKeymap('<C-a>', '', '{C-a}')
  })
})

describe('registerKeymap()', () => {
  it('should throw for invalid key', async () => {
    let err
    try {
      keymaps.registerKeymap(['i'], '', jest.fn())
    } catch (e) {
      err = e
    }
    expect(err).toBeDefined()
  })

  it('should throw for duplicated key', async () => {
    keymaps.registerKeymap(['i'], 'tmp', jest.fn())
    let err
    try {
      keymaps.registerKeymap(['i'], 'tmp', jest.fn())
    } catch (e) {
      err = e
    }
    expect(err).toBeDefined()
  })

  it('should register insert key mapping', async () => {
    let fn = jest.fn()
    disposables.push(keymaps.registerKeymap(['i'], 'test', fn))
    await helper.wait(10)
    let res = await nvim.call('execute', ['verbose imap <Plug>(coc-test)'])
    expect(res).toMatch('coc#_insert_key')
  })
})

describe('registerExprKeymap()', () => {
  it('should visual key mapping', async () => {
    await nvim.setLine('foo')
    let called = false
    let fn = () => {
      called = true
      return ''
    }
    disposables.push(keymaps.registerExprKeymap('x', 'x', fn, true))
    await helper.wait(50)
    await nvim.command('normal! viw')
    await nvim.input('x<esc>')
    await helper.wait(50)
    expect(called).toBe(true)
  })
})
