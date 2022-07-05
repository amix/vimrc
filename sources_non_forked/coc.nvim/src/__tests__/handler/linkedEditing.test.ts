import { Neovim } from '@chemzqm/neovim'
import { Disposable, Range, Position } from 'vscode-languageserver-protocol'
import LinkedEditingHandler from '../../handler/linkedEditing'
import languages from '../../languages'
import workspace from '../../workspace'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let handler: LinkedEditingHandler
let disposables: Disposable[] = []
let wordPattern: string | undefined
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  handler = helper.plugin.getHandler().linkedEditingHandler
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  helper.updateConfiguration('coc.preferences.enableLinkedEditing', true)
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

async function registerProvider(content: string, position: Position): Promise<void> {
  let doc = await workspace.document
  disposables.push(languages.registerLinkedEditingRangeProvider([{ language: '*' }], {
    provideLinkedEditingRanges: (doc, pos) => {
      let document = workspace.getDocument(doc.uri)
      let range = document.getWordRangeAtPosition(pos)
      if (!range) return null
      let text = doc.getText(range)
      let ranges: Range[] = document.getSymbolRanges(text)
      return { ranges, wordPattern }
    }
  }))
  await nvim.setLine(content)
  await doc.synchronize()
  await handler.enable(doc, position)
}

async function assertMatches(len: number): Promise<void> {
  let res = await nvim.call('getmatches') as any[]
  res = res.filter(o => o.group === 'CocLinkedEditing')
  expect(res.length).toBe(len)
}

describe('LinkedEditing', () => {
  it('should active and cancel on cursor moved', async () => {
    await registerProvider('foo foo a ', Position.create(0, 0))
    await assertMatches(2)
    await nvim.command(`normal! $`)
    await helper.wait(50)
    await assertMatches(0)
  })

  it('should active when moved to another word', async () => {
    await registerProvider('foo foo bar bar bar', Position.create(0, 0))
    await nvim.call('cursor', [1, 9])
    await helper.wait(50)
    await assertMatches(3)
  })

  it('should active on text change', async () => {
    let doc = await workspace.document
    await registerProvider('foo foo a ', Position.create(0, 0))
    await assertMatches(2)
    await nvim.call('cursor', [1, 1])
    await nvim.call('nvim_buf_set_text', [doc.bufnr, 0, 0, 0, 0, ['i']])
    await doc.synchronize()
    let line = await nvim.line
    expect(line).toBe('ifoo ifoo a ')
    await nvim.call('nvim_buf_set_text', [doc.bufnr, 0, 0, 0, 1, []])
    await doc.synchronize()
    line = await nvim.line
    expect(line).toBe('foo foo a ')
  })

  it('should cancel when change out of range', async () => {
    let doc = await workspace.document
    await registerProvider('foo foo bar', Position.create(0, 0))
    await assertMatches(2)
    await nvim.call('nvim_buf_set_text', [doc.bufnr, 0, 9, 0, 10, ['']])
    await doc.synchronize()
    await assertMatches(0)
  })

  it('should cancel on editor change', async () => {
    await registerProvider('foo foo a ', Position.create(0, 0))
    await nvim.command(`enew`)
    await helper.wait(50)
    await assertMatches(0)
  })

  it('should cancel when insert none word character', async () => {
    await registerProvider('foo foo a ', Position.create(0, 0))
    await nvim.call('cursor', [1, 4])
    await nvim.input('i')
    await nvim.input('a')
    await helper.wait(50)
    await assertMatches(2)
    await nvim.input('i')
    await nvim.input('@')
    await helper.wait(50)
    await assertMatches(0)
  })

  it('should cancel when insert not match wordPattern', async () => {
    wordPattern = '[A-Z]'
    await registerProvider('foo foo a ', Position.create(0, 0))
    await nvim.call('cursor', [1, 4])
    await nvim.input('i')
    await nvim.input('A')
    await helper.wait(50)
    await assertMatches(2)
    await nvim.input('i')
    await nvim.input('3')
    await helper.wait(50)
    await assertMatches(0)
  })

  it('should cancel request on cursor moved', async () => {
    disposables.push(languages.registerLinkedEditingRangeProvider([{ language: '*' }], {
      provideLinkedEditingRanges: (doc, pos, token) => {
        return new Promise(resolve => {
          token.onCancellationRequested(() => {
            clearTimeout(timer)
            resolve(null)
          })
          let timer = setTimeout(() => {
            let document = workspace.getDocument(doc.uri)
            let range = document.getWordRangeAtPosition(pos)
            if (!range) return resolve(null)
            let text = doc.getText(range)
            let ranges: Range[] = document.getSymbolRanges(text)
            resolve({ ranges, wordPattern })
          }, 1000)
        })
      }
    }))
    let doc = await workspace.document
    await nvim.setLine('foo foo  ')
    await doc.synchronize()
    await nvim.call('cursor', [1, 2])
    await helper.wait(30)
    await nvim.call('cursor', [1, 9])
    await helper.wait(30)
    await assertMatches(0)
  })
})
