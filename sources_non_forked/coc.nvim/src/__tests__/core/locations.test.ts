import { Neovim } from '@chemzqm/neovim'
import os from 'os'
import path from 'path'
import { Location, Range } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import workspace from '../../workspace'
import helper from '../helper'

let nvim: Neovim

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  await nvim.command(`source ${path.join(process.cwd(), 'autoload/coc/ui.vim')}`)
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
})

function createLocations(): Location[] {
  let uri = URI.file(__filename).toString()
  return [Location.create(uri, Range.create(0, 0, 1, 0)), Location.create(uri, Range.create(2, 0, 3, 0))]
}

describe('showLocations()', () => {
  it('should show location list by default', async () => {
    let locations = createLocations()
    await workspace.showLocations(locations)
    await helper.waitFor('bufname', ['%'], 'list:///location')
  })

  it('should fire autocmd when location list disabled', async () => {
    Object.assign(workspace.env, {
      locationlist: false
    })
    await nvim.exec(`
function OnLocationsChange()
  let g:called = 1
endfunction
autocmd User CocLocationsChange :call OnLocationsChange()`)
    let locations = createLocations()
    await workspace.showLocations(locations)
    await helper.waitFor('eval', [`get(g:,'called',0)`], 1)
  })

  it('should show quickfix when quickfix enabled', async () => {
    helper.updateConfiguration('coc.preferences.useQuickfixForLocations', true)
    let locations = createLocations()
    await workspace.showLocations(locations)
    await helper.waitFor('eval', [`&buftype`], 'quickfix')
  })

  it('should use customized quickfix open command', async () => {
    await nvim.setVar('coc_quickfix_open_command', 'copen 1')
    helper.updateConfiguration('coc.preferences.useQuickfixForLocations', true)
    let locations = createLocations()
    await workspace.showLocations(locations)
    await helper.waitFor('eval', [`&buftype`], 'quickfix')
    let win = await nvim.window
    let height = await win.height
    expect(height).toBe(1)
  })
})

describe('jumpTo()', () => {
  it('should jumpTo position', async () => {
    let uri = URI.file('/tmp/foo').toString()
    await workspace.jumpTo(uri, { line: 1, character: 1 })
    await nvim.command('setl buftype=nofile')
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toMatch('/foo')
    await buf.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    await workspace.jumpTo(uri, { line: 1, character: 1 })
    let pos = await nvim.call('getcurpos')
    expect(pos.slice(1, 3)).toEqual([2, 2])
  })

  it('should jumpTo uri without normalize', async () => {
    let uri = 'zipfile:///tmp/clojure-1.9.0.jar::clojure/core.clj'
    await workspace.jumpTo(uri)
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toBe(uri)
  })

  it('should jump without position', async () => {
    let uri = URI.file('/tmp/foo').toString()
    await workspace.jumpTo(uri)
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toMatch('/foo')
  })

  it('should jumpTo custom uri scheme', async () => {
    let uri = 'jdt://foo'
    await workspace.jumpTo(uri, { line: 1, character: 1 })
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toBe(uri)
  })

})

describe('openResource()', () => {
  it('should open resource', async () => {
    let uri = URI.file(path.join(os.tmpdir(), 'bar')).toString()
    await workspace.openResource(uri)
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toMatch('bar')
  })

  it('should open none file uri', async () => {
    workspace.registerTextDocumentContentProvider('jd', {
      provideTextDocumentContent: () => 'jd'
    })
    let uri = 'jd://abc'
    await workspace.openResource(uri)
    let buf = await nvim.buffer
    let name = await buf.name
    expect(name).toBe('jd://abc')
  })

  it('should open opened buffer', async () => {
    let buf = await helper.edit()
    let doc = workspace.getDocument(buf.id)
    await workspace.openResource(doc.uri)
    await helper.wait(30)
    let bufnr = await nvim.call('bufnr', '%')
    expect(bufnr).toBe(buf.id)
  })

  it('should open url', async () => {
    await helper.mockFunction('coc#ui#open_url', 0)
    let buf = await helper.edit()
    let uri = 'http://example.com'
    await workspace.openResource(uri)
    await helper.wait(30)
    let bufnr = await nvim.call('bufnr', '%')
    expect(bufnr).toBe(buf.id)
  })
})
