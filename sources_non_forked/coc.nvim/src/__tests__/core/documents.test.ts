import { Neovim } from '@chemzqm/neovim'
import os from 'os'
import path from 'path'
import fs from 'fs'
import { v4 as uuid } from 'uuid'
import Documents from '../../core/documents'
import events from '../../events'
import workspace from '../../workspace'
import helper from '../helper'

let documents: Documents
let nvim: Neovim

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  documents = workspace.documentsManager
})

afterEach(async () => {
  await helper.reset()
})

afterAll(async () => {
  await helper.shutdown()
})

describe('documents', () => {
  it('should get document', async () => {
    await helper.createDocument('bar')
    let doc = await helper.createDocument('foo')
    let res = documents.getDocument(doc.uri)
    expect(res.uri).toBe(doc.uri)
  })

  it('should create document', async () => {
    await helper.createDocument()
    let bufnrs = await nvim.call('coc#ui#open_files', [[__filename]]) as number[]
    let bufnr = bufnrs[0]
    let doc = workspace.getDocument(bufnr)
    expect(doc).toBeUndefined()
    doc = await documents.createDocument(bufnr)
    expect(doc).toBeDefined()
  })

  it('should check buffer rename on save', async () => {
    let doc = await workspace.document
    let bufnr = doc.bufnr
    let name = `${uuid()}.vim`
    let tmpfile = path.join(os.tmpdir(), name)
    await nvim.command(`write ${tmpfile}`)
    doc = workspace.getDocument(bufnr)
    expect(doc).toBeDefined()
    expect(doc.filetype).toBe('vim')
    expect(doc.bufname).toMatch(name)
    fs.unlinkSync(tmpfile)
  })

  it('should get current document', async () => {
    let p1 = workspace.document
    let p2 = workspace.document
    let arr = await Promise.all([p1, p2])
    expect(arr[0]).toBe(arr[1])
  })

  it('should get bufnrs', async () => {
    await workspace.document
    let bufnrs = documents.bufnrs
    expect(bufnrs.length).toBe(1)
  })

  it('should get uri', async () => {
    let doc = await workspace.document
    expect(documents.uri).toBe(doc.uri)
  })

  it('should attach events on vim', async () => {
    await documents.attach(nvim, workspace.env)
    let env = Object.assign(workspace.env, { isVim: true })
    documents.detach()
    await documents.attach(nvim, env)
    documents.detach()
    await events.fire('CursorMoved', [1, [1, 1]])
  })
})
