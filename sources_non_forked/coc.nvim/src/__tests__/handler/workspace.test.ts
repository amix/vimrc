import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import WorkspaceHandler from '../../handler/workspace'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import extensions from '../../extensions'
import helper from '../helper'

let nvim: Neovim
let handler: WorkspaceHandler
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  handler = helper.plugin.getHandler().workspace
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('Workspace handler', () => {
  describe('methods', () => {
    it('should open log', async () => {
      await handler.openLog()
      let bufname = await nvim.call('bufname', ['%']) as string
      expect(bufname.endsWith('coc-nvim.log')).toBe(true)
    })

    it('should get configuration of current document', async () => {
      let config = await handler.getConfiguration('suggest')
      let wait = config.get<number>('triggerCompletionWait')
      expect(wait).toBe(0)
    })

    it('should get root patterns', async () => {
      let doc = await helper.createDocument()
      let patterns = handler.getRootPatterns(doc.bufnr)
      expect(patterns).toBeDefined()
    })
  })

  describe('doKeymap()', () => {
    it('should return default value when key mapping does not exist', async () => {
      let res = await handler.doKeymap('not_exists', '', '<C-a')
      expect(res).toBe('')
    })

    it('should support repeat key mapping', async () => {
      let called = false
      await nvim.command('nmap do <Plug>(coc-test)')
      disposables.push(workspace.registerKeymap(['n'], 'test', () => {
        called = true
      }, { repeat: true, silent: true, sync: false }))
      await helper.wait(100)
      await nvim.call('feedkeys', ['do', 'i'])
      await helper.wait(30)
      expect(called).toBe(true)
    })
  })

  describe('snippetCheck()', () => {
    it('should return false when coc-snippets not found', async () => {
      expect(await handler.snippetCheck(true, false)).toBe(false)
    })

    it('should check jump', async () => {
      expect(await handler.snippetCheck(false, true)).toBe(false)
    })

    it('should check expand by coc-snippets', async () => {
      let has = extensions.has
      let getExtensionApi = extensions.getExtensionApi
      extensions.has = () => {
        return true
      }
      extensions.getExtensionApi = () => {
        return {
          expandable: () => {
            return true
          }
        }
      }
      disposables.push({
        dispose: () => {
          extensions.has = has
          extensions.getExtensionApi = getExtensionApi
        }
      })
      let res = await handler.snippetCheck(true, false)
      expect(res).toBe(true)
    })
  })
})
