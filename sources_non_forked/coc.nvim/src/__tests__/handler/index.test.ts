import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import Handler from '../../handler/index'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let handler: Handler
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  handler = (helper.plugin as any).handler
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

describe('Handler', () => {
  describe('hasProvider', () => {
    it('should check provider for document', async () => {
      let res = await handler.hasProvider('definition')
      expect(res).toBe(false)
    })
  })

  describe('checkProvier', () => {
    it('should throw error when provider not found', async () => {
      let doc = await helper.createDocument()
      let err
      try {
        handler.checkProvier('definition', doc.textDocument)
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })
  })

  describe('withRequestToken', () => {
    it('should cancel previous request when called again', async () => {
      let cancelled = false
      let p = handler.withRequestToken('test', token => {
        return new Promise(s => {
          token.onCancellationRequested(() => {
            cancelled = true
            clearTimeout(timer)
            s(undefined)
          })
          let timer = setTimeout(() => {
            s(undefined)
          }, 3000)
        })
      }, false)
      setTimeout(async () => {
        await handler.withRequestToken('test', () => {
          return Promise.resolve(undefined)
        }, false)
      }, 50)
      await p
      expect(cancelled).toBe(true)
    })

    it('should cancel request on insert start', async () => {
      let cancelled = false
      let p = handler.withRequestToken('test', token => {
        return new Promise(s => {
          token.onCancellationRequested(() => {
            cancelled = true
            clearTimeout(timer)
            s(undefined)
          })
          let timer = setTimeout(() => {
            s(undefined)
          }, 3000)
        })
      }, false)
      await nvim.input('i')
      await p
      expect(cancelled).toBe(true)
    })
  })
})
