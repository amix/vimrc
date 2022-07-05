import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable, Position, Range } from 'vscode-languageserver-protocol'
import InlayHintHandler from '../../handler/inlayHint/index'
import { InlayHint } from '../../inlayHint'
import languages from '../../languages'
import { isValidInlayHint, sameHint } from '../../provider/inlayHintManager'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper from '../helper'

let nvim: Neovim
let handler: InlayHintHandler
let disposables: Disposable[] = []
let ns: number
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  handler = helper.plugin.getHandler().inlayHintHandler
  ns = await nvim.createNamespace('coc-inlayHint')
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('InlayHint', () => {
  describe('utils', () => {
    it('should check same hint', () => {
      let hint = InlayHint.create(Position.create(0, 0), 'foo')
      expect(sameHint(hint, InlayHint.create(Position.create(0, 0), 'bar'))).toBe(false)
      expect(sameHint(hint, InlayHint.create(Position.create(0, 0), [{ value: 'foo' }]))).toBe(true)
    })

    it('should check valid hint', () => {
      let hint = InlayHint.create(Position.create(0, 0), 'foo')
      expect(isValidInlayHint(hint, Range.create(0, 0, 1, 0))).toBe(true)
      expect(isValidInlayHint(InlayHint.create(Position.create(0, 0), ''), Range.create(0, 0, 1, 0))).toBe(false)
      expect(isValidInlayHint(InlayHint.create(Position.create(3, 0), 'foo'), Range.create(0, 0, 1, 0))).toBe(false)
      expect(isValidInlayHint({ label: 'f' } as any, Range.create(0, 0, 1, 0))).toBe(false)
    })
  })

  describe('provideInlayHints', () => {
    it('should not throw when failed', async () => {
      disposables.push(languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: () => {
          return Promise.reject(new Error('Test failure'))
        }
      }))
      let doc = await workspace.document
      let tokenSource = new CancellationTokenSource()
      let res = await languages.provideInlayHints(doc.textDocument, Range.create(0, 0, 1, 0), tokenSource.token)
      expect(res).toEqual([])
    })

    it('should merge provide results', async () => {
      disposables.push(languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: () => {
          return [InlayHint.create(Position.create(0, 0), 'foo')]
        }
      }))
      disposables.push(languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: () => {
          return [
            InlayHint.create(Position.create(0, 0), 'foo'),
            InlayHint.create(Position.create(1, 0), 'bar'),
            InlayHint.create(Position.create(5, 0), 'bad')]
        }
      }))
      let doc = await workspace.document
      let tokenSource = new CancellationTokenSource()
      let res = await languages.provideInlayHints(doc.textDocument, Range.create(0, 0, 3, 0), tokenSource.token)
      expect(res.length).toBe(2)
    })

    it('should resolve inlay hint', async () => {
      disposables.push(languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: () => {
          return [InlayHint.create(Position.create(0, 0), 'foo')]
        },
        resolveInlayHint: hint => {
          hint.tooltip = 'tooltip'
          return hint
        }
      }))
      let doc = await workspace.document
      let tokenSource = new CancellationTokenSource()
      let res = await languages.provideInlayHints(doc.textDocument, Range.create(0, 0, 1, 0), tokenSource.token)
      let resolved = await languages.resolveInlayHint(res[0], tokenSource.token)
      expect(resolved.tooltip).toBe('tooltip')
      resolved = await languages.resolveInlayHint(resolved, tokenSource.token)
      expect(resolved.tooltip).toBe('tooltip')
    })

    it('should not resolve when cancelled', async () => {
      disposables.push(languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: () => {
          return [InlayHint.create(Position.create(0, 0), 'foo')]
        },
        resolveInlayHint: (hint, token) => {
          return new Promise(resolve => {
            token.onCancellationRequested(() => {
              clearTimeout(timer)
              resolve(null)
            })
            let timer = setTimeout(() => {
              resolve(Object.assign({}, hint, { tooltip: 'tooltip' }))
            }, 200)
          })
        }
      }))
      let doc = await workspace.document
      let tokenSource = new CancellationTokenSource()
      let res = await languages.provideInlayHints(doc.textDocument, Range.create(0, 0, 1, 0), tokenSource.token)
      let p = languages.resolveInlayHint(res[0], tokenSource.token)
      tokenSource.cancel()
      let resolved = await p
      expect(resolved.tooltip).toBeUndefined()
    })
  })

  describe('setVirtualText', () => {
    async function registerProvider(content: string): Promise<Disposable> {
      let doc = await workspace.document
      let disposable = languages.registerInlayHintsProvider([{ language: '*' }], {
        provideInlayHints: (document, range) => {
          let content = document.getText(range)
          let lines = content.split(/\r?\n/)
          let hints: InlayHint[] = []
          for (let i = 0; i < lines.length; i++) {
            let line = lines[i]
            if (!line.length) continue
            let parts = line.split(/\s+/)
            hints.push(...parts.map(s => InlayHint.create(Position.create(range.start.line + i, line.length), s)))
          }
          return hints
        }
      })
      await doc.buffer.setLines(content.split(/\n/), { start: 0, end: -1 })
      await doc.synchronize()
      return disposable
    }

    async function waitRefresh(bufnr: number) {
      let buf = handler.getItem(bufnr)
      return new Promise<void>((resolve, reject) => {
        let timer = setTimeout(() => {
          reject(new Error('not refresh after 1s'))
        }, 1000)
        buf.onDidRefresh(() => {
          clearTimeout(timer)
          resolve()
        })
      })
    }

    it('should not refresh when languageId not match', async () => {
      let doc = await workspace.document
      disposables.push(languages.registerInlayHintsProvider([{ language: 'javascript' }], {
        provideInlayHints: () => {
          let hint = InlayHint.create(Position.create(0, 0), 'foo')
          return [hint]
        }
      }))
      await nvim.setLine('foo')
      await doc.synchronize()
      await helper.wait(30)
      let markers = await doc.buffer.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBe(0)
    })

    it('should refresh on text change', async () => {
      let buf = await nvim.buffer
      let disposable = await registerProvider('foo')
      disposables.push(disposable)
      await waitRefresh(buf.id)
      await buf.setLines(['a', 'b', 'c'], { start: 0, end: -1 })
      await waitRefresh(buf.id)
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBe(3)
      let item = handler.getItem(buf.id)
      await item.renderRange()
      expect(item.current.length).toBe(3)
    })

    it('should refresh on provider dispose', async () => {
      let buf = await nvim.buffer
      let disposable = await registerProvider('foo bar')
      await waitRefresh(buf.id)
      disposable.dispose()
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBe(0)
      let item = handler.getItem(buf.id)
      expect(item.current.length).toBe(0)
      await item.renderRange()
      expect(item.current.length).toBe(0)
    })

    it('should refresh on scroll', async () => {
      let arr = new Array(200)
      let content = arr.fill('foo').join('\n')
      let buf = await nvim.buffer
      let disposable = await registerProvider(content)
      disposables.push(disposable)
      await waitRefresh(buf.id)
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      let len = markers.length
      await nvim.command('normal! G')
      await waitRefresh(buf.id)
      await nvim.input('<C-y>')
      await waitRefresh(buf.id)
      markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBeGreaterThan(len)
    })

    it('should cancel previous render', async () => {
      let buf = await nvim.buffer
      let disposable = await registerProvider('foo')
      disposables.push(disposable)
      await waitRefresh(buf.id)
      let item = handler.getItem(buf.id)
      await item.renderRange()
      await item.renderRange()
      expect(item.current.length).toBe(1)
    })
  })
})

