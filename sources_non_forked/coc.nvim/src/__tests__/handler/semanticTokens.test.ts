import { Buffer, Neovim } from '@chemzqm/neovim'
import fs from 'fs'
import os from 'os'
import path from 'path'
import { Disposable, Range, SemanticTokensLegend } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import commandManager from '../../commands'
import SemanticTokens from '../../handler/semanticTokens/index'
import languages from '../../languages'
import { disposeAll } from '../../util'
import window from '../../window'
import workspace from '../../workspace'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let ns: number
let disposables: Disposable[] = []
let highlighter: SemanticTokens
let legend: SemanticTokensLegend = {
  tokenTypes: [
    "comment",
    "keyword",
    "string",
    "number",
    "regexp",
    "operator",
    "namespace",
    "type",
    "struct",
    "class",
    "interface",
    "enum",
    "enumMember",
    "typeParameter",
    "function",
    "method",
    "property",
    "macro",
    "variable",
    "parameter",
    "angle",
    "arithmetic",
    "attribute",
    "bitwise",
    "boolean",
    "brace",
    "bracket",
    "builtinType",
    "character",
    "colon",
    "comma",
    "comparison",
    "constParameter",
    "dot",
    "escapeSequence",
    "formatSpecifier",
    "generic",
    "label",
    "lifetime",
    "logical",
    "operator",
    "parenthesis",
    "punctuation",
    "selfKeyword",
    "semicolon",
    "typeAlias",
    "union",
    "unresolvedReference"
  ],
  tokenModifiers: [
    "documentation",
    "declaration",
    "definition",
    "static",
    "abstract",
    "deprecated",
    "readonly",
    "constant",
    "controlFlow",
    "injected",
    "mutable",
    "consuming",
    "async",
    "library",
    "public",
    "unsafe",
    "attribute",
    "trait",
    "callable",
    "intraDocLink"
  ]
}

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  ns = await nvim.call('coc#highlight#create_namespace', ['semanticTokens'])
  highlighter = helper.plugin.getHandler().semanticHighlighter
})

afterAll(async () => {
  await helper.shutdown()
})

const defaultResult = {
  resultId: '1',
  data: [
    0, 0, 2, 1, 0,
    0, 3, 4, 14, 2,
    0, 4, 1, 41, 0,
    0, 1, 1, 41, 3,
    0, 2, 1, 25, 0,
    1, 4, 8, 17, 0,
    0, 8, 1, 41, 0,
    0, 1, 3, 2, 0,
    0, 3, 1, 41, 0,
    0, 1, 1, 44, 0,
    1, 0, 1, 25, 0,
  ]
}

function registerRangeProvider(filetype: string, fn: (range: Range) => number[]): Disposable {
  return languages.registerDocumentRangeSemanticTokensProvider([{ language: filetype }], {
    provideDocumentRangeSemanticTokens: (_, range) => {
      return {
        data: fn(range)
      }
    }
  }, legend)
}

function registerProvider(): void {
  disposables.push(languages.registerDocumentSemanticTokensProvider([{ language: 'rust' }], {
    provideDocumentSemanticTokens: () => {
      return defaultResult
    },
    provideDocumentSemanticTokensEdits: (_, previousResultId) => {
      if (previousResultId !== '1') return undefined
      return {
        resultId: '2',
        edits: [{
          start: 0,
          deleteCount: 0,
          data: [0, 0, 3, 1, 0]
        }]
      }
    }
  }, legend))
}

async function createRustBuffer(): Promise<Buffer> {
  helper.updateConfiguration('semanticTokens.filetypes', ['rust'])
  registerProvider()
  let code = `fn main() {
    println!("H");
}`
  let buf = await nvim.buffer
  await nvim.command('setf rust')
  await buf.setLines(code.split('\n'), { start: 0, end: -1, strictIndexing: false })
  let doc = await workspace.document
  await doc.patchChange()
  return buf
}

afterEach(async () => {
  helper.updateConfiguration('semanticTokens.filetypes', [])
  await helper.reset()
  disposeAll(disposables)
})

describe('semanticTokens', () => {
  describe('showHighlightInfo()', () => {
    it('should show error when buffer not attached', async () => {
      await nvim.command('h')
      await highlighter.showHighlightInfo()
      let line = await helper.getCmdline()
      expect(line).toMatch('not attached')
      await highlighter.inspectSemanticToken()
    })

    it('should show message when not enabled', async () => {
      await helper.edit('t.txt')
      await highlighter.showHighlightInfo()
      let buf = await nvim.buffer
      let lines = await buf.lines
      expect(lines[2]).toMatch('not enabled for current filetype')
    })

    it('should show semantic tokens info', async () => {
      await createRustBuffer()
      await highlighter.highlightCurrent()
      await commandManager.executeCommand('semanticTokens.checkCurrent')
      let buf = await nvim.buffer
      let lines = await buf.lines
      let content = lines.join('\n')
      expect(content).toMatch('Semantic highlight groups used by current buffer')
    })

    it('should show highlight info for empty legend', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', ['*'])
      disposables.push(languages.registerDocumentRangeSemanticTokensProvider([{ language: '*' }], {
        provideDocumentRangeSemanticTokens: (_, range) => {
          return {
            data: []
          }
        }
      }, { tokenModifiers: [], tokenTypes: [] }))
      await highlighter.showHighlightInfo()
      await highlighter.showHighlightInfo()
      let buf = await nvim.buffer
      let lines = await buf.lines
      let content = lines.join('\n')
      expect(content).toMatch('No token')
    })
  })

  describe('highlightCurrent()', () => {
    it('should refresh highlights', async () => {
      await createRustBuffer()
      await nvim.command('hi link CocSemDeclarationFunction MoreMsg')
      await nvim.command('hi link CocSemDocumentation Statement')
      await window.moveTo({ line: 0, character: 4 })
      await highlighter.highlightCurrent()
      await commandManager.executeCommand('semanticTokens.inspect')
      let win = await helper.getFloat()
      let buf = await win.buffer
      let lines = await buf.lines
      let content = lines.join('\n')
      expect(content).toMatch('CocSemDeclarationFunction')
      await window.moveTo({ line: 1, character: 0 })
      await commandManager.executeCommand('semanticTokens.inspect')
      win = await helper.getFloat()
      expect(win).toBeUndefined()
    })

    it('should refresh highlights by command', async () => {
      await helper.edit()
      let err
      try {
        await commandManager.executeCommand('semanticTokens.refreshCurrent')
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should refresh when buffer visible', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', ['rust'])
      let code = `fn main() {
    println!("H");
}`
      let buf = await nvim.buffer
      await nvim.command('setf rust')
      await buf.setLines(code.split('\n'), { start: 0, end: -1, strictIndexing: false })
      await helper.wait(10)
      let doc = await workspace.document
      await doc.synchronize()
      let item = await highlighter.getCurrentItem()
      expect(item.enabled).toBe(false)
      await nvim.command('edit bar')
      registerProvider()
      expect(item.enabled).toBe(true)
      await helper.wait(20)
      await nvim.command(`b ${buf.id}`)
      await item.waitRefresh()
      expect(item.highlights).toBeDefined()
    })

    it('should reuse exists tokens when version not changed', async () => {
      let doc = await helper.createDocument('t.vim')
      await doc.applyEdits([{ range: Range.create(0, 0, 0, 0), newText: 'let' }])
      let fn = jest.fn()
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      disposables.push(languages.registerDocumentSemanticTokensProvider([{ language: 'vim' }], {
        provideDocumentSemanticTokens: () => {
          fn()
          return new Promise(resolve => {
            resolve({
              resultId: '1',
              data: [0, 0, 3, 1, 0]
            })
          })
        }
      }, legend))
      let item = await highlighter.getCurrentItem()
      item.cancel()
      await item.doHighlight()
      await item.doHighlight()
      expect(fn).toBeCalledTimes(1)
    })

    it('should only highlight limited range on update', async () => {
      let doc = await helper.createDocument('t.vim')
      let fn = jest.fn()
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      disposables.push(languages.registerDocumentSemanticTokensProvider([{ language: 'vim' }], {
        provideDocumentSemanticTokens: (doc, token) => {
          let text = doc.getText()
          if (!text.trim()) {
            return Promise.resolve({ resultId: '1', data: [] })
          }
          fn()
          let lines = text.split('\n')
          let data = [0, 0, 1, 1, 0]
          for (let i = 0; i < lines.length; i++) {
            data.push(1, 0, 1, 1, 0)
          }
          return new Promise(resolve => {
            token.onCancellationRequested(() => {
              clearTimeout(timer)
              resolve(undefined)
            })
            let timer = setTimeout(() => {
              resolve({ resultId: '1', data })
            }, 50)
          })
        }
      }, legend))
      let item = await highlighter.getCurrentItem()
      await item.doHighlight()
      let newLine = 'l\n'
      await doc.applyEdits([{ range: Range.create(0, 0, 0, 0), newText: `${newLine.repeat(2000)}` }])
      await item.doHighlight()
      await item.waitRefresh()
      expect(fn).toBeCalled()
      let buf = nvim.createBuffer(doc.bufnr)
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      let len = markers.length
      expect(len).toBeLessThan(400)
      await nvim.command('normal! gg')
      await helper.wait(50)
      await nvim.command('normal! 200G')
      await helper.wait(50)
      markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBeGreaterThan(len)
    })

    it('should highlight hidden buffer on shown', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', ['rust'])
      registerProvider()
      let code = 'fn main() {\n  println!("H"); \n}'
      let filepath = path.join(os.tmpdir(), 'a.rs')
      fs.writeFileSync(filepath, code, 'utf8')
      let uri = URI.file(filepath).toString()
      await workspace.loadFile(uri)
      let doc = workspace.getDocument(uri)
      let item = highlighter.getItem(doc.bufnr)
      let fn = jest.fn()
      item.onDidRefresh(() => {
        fn()
      })
      let buf = doc.buffer
      await helper.wait(10)
      expect(doc.filetype).toBe('rust')
      expect(fn).toBeCalledTimes(0)
      await nvim.command(`b ${buf.id}`)
      await helper.wait(50)
      expect(fn).toBeCalledTimes(1)
    })

    it('should not highlight on shown when document not changed', async () => {
      let fn = jest.fn()
      let buf = await createRustBuffer()
      let item = await highlighter.getCurrentItem()
      await item.waitRefresh()
      await nvim.command('enew')
      item.doHighlight = async () => {
        fn()
      }
      await nvim.command(`b ${buf.id}`)
      await helper.wait(100)
      expect(fn).toBeCalledTimes(0)
    })
  })

  describe('clear highlights', () => {
    it('should clear highlights of current buffer', async () => {
      await createRustBuffer()
      await highlighter.highlightCurrent()
      let buf = await nvim.buffer
      let markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBeGreaterThan(0)
      await commandManager.executeCommand('semanticTokens.clearCurrent')
      markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBe(0)
    })

    it('should clear all highlights', async () => {
      await createRustBuffer()
      await highlighter.highlightCurrent()
      let buf = await nvim.buffer
      await commandManager.executeCommand('semanticTokens.clearAll')
      let markers = await buf.getExtMarks(ns, 0, -1)
      expect(markers.length).toBe(0)
    })
  })

  describe('rangeProvider', () => {
    it('should invoke range provider first time when both kinds exist', async () => {
      let fn = jest.fn()
      disposables.push(registerRangeProvider('rust', () => {
        fn()
        return []
      }))
      let buf = await createRustBuffer()
      let item = highlighter.getItem(buf.id)
      await item.waitRefresh()
      await helper.wait(50)
      expect(fn).toBeCalled()
    })

    it('should do range highlight first time', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      let r: Range
      disposables.push(registerRangeProvider('vim', range => {
        r = range
        return [0, 0, 3, 1, 0]
      }))
      let filepath = await createTmpFile('let')
      fs.renameSync(filepath, filepath + '.vim')
      let doc = await helper.createDocument(filepath + '.vim')
      expect(doc.filetype).toBe('vim')
      await helper.waitValue(() => {
        return typeof r !== 'undefined'
      }, true)
    })

    it('should do range highlight after cursor moved', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      let doc = await helper.createDocument('t.vim')
      let r: Range
      expect(doc.filetype).toBe('vim')
      await nvim.call('setline', [2, (new Array(200).fill(''))])
      await doc.applyEdits([{ range: Range.create(0, 0, 0, 0), newText: 'let' }])
      await helper.wait(50)
      disposables.push(registerRangeProvider('vim', range => {
        r = range
        return []
      }))
      await nvim.command('normal! G')
      await helper.wait(100)
      expect(r).toBeDefined()
      expect(r.end).toEqual({ line: 201, character: 0 })
    })

    it('should only cancel range highlight request', async () => {
      let rangeCancelled = false
      disposables.push(languages.registerDocumentRangeSemanticTokensProvider([{ language: 'vim' }], {
        provideDocumentRangeSemanticTokens: (_, range, token) => {
          return new Promise(resolve => {
            token.onCancellationRequested(() => {
              clearTimeout(timeout)
              rangeCancelled = true
              resolve(null)
            })
            let timeout = setTimeout(() => {
              resolve({ data: [] })
            }, 500)
          })
        }
      }, legend))
      disposables.push(languages.registerDocumentSemanticTokensProvider([{ language: 'vim' }], {
        provideDocumentSemanticTokens: (_, token) => {
          return new Promise(resolve => {
            resolve({
              resultId: '1',
              data: [0, 0, 3, 1, 0]
            })
          })
        }
      }, legend))
      let doc = await helper.createDocument('t.vim')
      await doc.applyEdits([{ range: Range.create(0, 0, 0, 0), newText: 'let' }])
      let item = await highlighter.getCurrentItem()
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      item.cancel()
      let p = item.doHighlight()
      await helper.wait(10)
      item.cancel(true)
      await p
    })
  })

  describe('triggerSemanticTokens', () => {
    it('should be disabled by default', async () => {
      helper.updateConfiguration('semanticTokens.filetypes', [])
      await workspace.document
      const curr = await highlighter.getCurrentItem()
      expect(curr.enabled).toBe(false)
    })

    it('should be enabled', async () => {
      await createRustBuffer()
      const curr = await highlighter.getCurrentItem()
      expect(curr.enabled).toBe(true)
    })

    it('should get legend by API', async () => {
      await createRustBuffer()
      const doc = await workspace.document
      const l = languages.getLegend(doc.textDocument)
      expect(l).toEqual(legend)
    })

    it('should doHighlight', async () => {
      await createRustBuffer()
      const doc = await workspace.document
      await nvim.call('CocAction', 'semanticHighlight')
      const highlights = await nvim.call("coc#highlight#get_highlights", [doc.bufnr, 'semanticTokens'])
      expect(highlights.length).toBeGreaterThan(0)
      expect(highlights[0][0]).toBe('CocSemKeyword')
    })
  })

  describe('delta update', () => {
    it('should perform highlight update', async () => {
      await createRustBuffer()
      let buf = await nvim.buffer
      await highlighter.highlightCurrent()
      await window.moveTo({ line: 0, character: 0 })
      let doc = await workspace.document
      await nvim.input('if')
      await helper.wait(50)
      await doc.synchronize()
      let curr = await highlighter.getCurrentItem()
      await curr.forceHighlight()
      let markers = await buf.getExtMarks(ns, 0, -1, { details: true })
      expect(markers.length).toBeGreaterThan(0)
    })
  })

  describe('checkState', () => {
    it('should throw for invalid state', async () => {
      let doc = await workspace.document
      const toThrow = (cb: () => void) => {
        expect(cb).toThrow(Error)
      }
      let item = highlighter.getItem(doc.bufnr)
      toThrow(() => {
        item.checkState()
      })
      helper.updateConfiguration('semanticTokens.filetypes', ['*'])
      toThrow(() => {
        item.checkState()
      })
      toThrow(() => {
        // eslint-disable-next-line @typescript-eslint/ban-ts-comment
        // @ts-ignore
        workspace._env.updateHighlight = false
        item.checkState()
      })
      let enabled = item.enabled
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore
      workspace._env.updateHighlight = true
      expect(enabled).toBe(false)
      doc.detach()
      toThrow(() => {
        item.checkState()
      })
    })
  })

  describe('enabled', () => {
    it('should check if buffer enabled for semanticTokens', async () => {
      let doc = await workspace.document
      let item = highlighter.getItem(doc.bufnr)
      disposables.push(languages.registerDocumentRangeSemanticTokensProvider([{ language: '*' }], {
        provideDocumentRangeSemanticTokens: (_, range) => {
          return {
            data: []
          }
        }
      }, { tokenModifiers: [], tokenTypes: [] }))
      expect(item.enabled).toBe(false)
      helper.updateConfiguration('semanticTokens.filetypes', ['vim'])
      expect(item.enabled).toBe(false)
      helper.updateConfiguration('semanticTokens.filetypes', ['*'])
      expect(item.enabled).toBe(true)
      doc.detach()
      expect(item.enabled).toBe(false)
    })
  })
})
