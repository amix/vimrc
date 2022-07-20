import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable, FoldingRange, Range } from 'vscode-languageserver-protocol'
import FoldHandler from '../../handler/fold'
import languages from '../../languages'
import workspace from '../../workspace'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let folds: FoldHandler
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  folds = (helper.plugin as any).handler.fold
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  await helper.createDocument()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('Folds', () => {
  it('should return null when provider does not exist', async () => {
    let doc = await workspace.document
    let token = (new CancellationTokenSource()).token
    expect(await languages.provideFoldingRanges(doc.textDocument, {}, token)).toBe(null)
  })

  it('should return false when no fold ranges found', async () => {
    disposables.push(languages.registerFoldingRangeProvider([{ language: '*' }], {
      provideFoldingRanges(_doc) {
        return []
      }
    }))
    let res = await folds.fold()
    expect(res).toBe(false)
  })

  it('should fold all fold ranges', async () => {
    disposables.push(languages.registerFoldingRangeProvider([{ language: '*' }], {
      provideFoldingRanges(_doc) {
        return [FoldingRange.create(1, 3), FoldingRange.create(4, 6, 0, 0, 'comment')]
      }
    }))
    await nvim.call('setline', [1, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']])
    let res = await folds.fold()
    expect(res).toBe(true)
    let closed = await nvim.call('foldclosed', [2])
    expect(closed).toBe(2)
    closed = await nvim.call('foldclosed', [5])
    expect(closed).toBe(5)
  })

  it('should fold comment ranges', async () => {
    disposables.push(languages.registerFoldingRangeProvider([{ language: '*' }], {
      provideFoldingRanges(_doc) {
        return [FoldingRange.create(1, 3), FoldingRange.create(4, 6, 0, 0, 'comment')]
      }
    }))
    await nvim.call('setline', [1, ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']])
    let res = await folds.fold('comment')
    expect(res).toBe(true)
    let closed = await nvim.call('foldclosed', [2])
    expect(closed).toBe(-1)
    closed = await nvim.call('foldclosed', [5])
    expect(closed).toBe(5)
  })
})
