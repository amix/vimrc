import { Neovim } from '@chemzqm/neovim'
import { Disposable, MarkedString, Hover, Range, TextEdit, Position } from 'vscode-languageserver-protocol'
import HoverHandler from '../../handler/hover'
import { URI } from 'vscode-uri'
import languages from '../../languages'
import { disposeAll } from '../../util'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let hover: HoverHandler
let disposables: Disposable[] = []
let hoverResult: Hover
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  hover = helper.plugin.getHandler().hover
})

afterAll(async () => {
  await helper.shutdown()
})

beforeEach(async () => {
  await helper.createDocument()
  disposables.push(languages.registerHoverProvider([{ language: '*' }], {
    provideHover: (_doc, _pos, _token) => {
      return hoverResult
    }
  }))
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

async function getDocumentText(): Promise<string> {
  let lines = await nvim.call('getbufline', ['coc://document', 1, '$']) as string[]
  return lines.join('\n')
}

describe('Hover', () => {
  describe('onHover', () => {
    it('should return false when hover not found', async () => {
      hoverResult = null
      let res = await hover.onHover('preview')
      expect(res).toBe(false)
    })

    it('should show MarkupContent hover', async () => {
      hoverResult = { contents: { kind: 'plaintext', value: 'my hover' } }
      await hover.onHover('preview')
      let res = await getDocumentText()
      expect(res).toMatch('my hover')
    })

    it('should show MarkedString hover', async () => {
      hoverResult = { contents: 'string hover' }
      disposables.push(languages.registerHoverProvider([{ language: '*' }], {
        provideHover: (_doc, _pos, _token) => {
          return { contents: { language: 'typescript', value: 'language hover' } }
        }
      }))
      await hover.onHover('preview')
      let res = await getDocumentText()
      expect(res).toMatch('string hover')
      expect(res).toMatch('language hover')
    })

    it('should show MarkedString hover array', async () => {
      hoverResult = { contents: ['foo', { language: 'typescript', value: 'bar' }] }
      await hover.onHover('preview')
      let res = await getDocumentText()
      expect(res).toMatch('foo')
      expect(res).toMatch('bar')
    })

    it('should highlight hover range', async () => {
      await nvim.setLine('var')
      await nvim.command('normal! 0')
      hoverResult = { contents: ['foo'], range: Range.create(0, 0, 0, 3) }
      await hover.onHover('preview')
      let res = await nvim.call('getmatches') as any[]
      expect(res.length).toBe(1)
      expect(res[0].group).toBe('CocHoverRange')
      await helper.wait(600)
      res = await nvim.call('getmatches')
      expect(res.length).toBe(0)
    })
  })

  describe('previewHover', () => {
    it('should echo hover message', async () => {
      hoverResult = { contents: ['foo'] }
      let res = await hover.onHover('echo')
      expect(res).toBe(true)
      let msg = await helper.getCmdline()
      expect(msg).toMatch('foo')
    })

    it('should show hover in float window', async () => {
      hoverResult = { contents: { kind: 'markdown', value: '```typescript\nconst foo:number\n```' } }
      await hover.onHover('float')
      let win = await helper.getFloat()
      expect(win).toBeDefined()
      let lines = await nvim.eval(`getbufline(winbufnr(${win.id}),1,'$')`)
      expect(lines).toEqual(['const foo:number'])
    })
  })

  describe('getHover', () => {
    it('should get hover from MarkedString array', async () => {
      hoverResult = { contents: ['foo', { language: 'typescript', value: 'bar' }] }
      disposables.push(languages.registerHoverProvider([{ language: '*' }], {
        provideHover: (_doc, _pos, _token) => {
          return { contents: { language: 'typescript', value: 'MarkupContent hover' } }
        }
      }))
      disposables.push(languages.registerHoverProvider([{ language: '*' }], {
        provideHover: (_doc, _pos, _token) => {
          return { contents: MarkedString.fromPlainText('MarkedString hover') }
        }
      }))
      let res = await hover.getHover()
      expect(res.includes('foo')).toBe(true)
      expect(res.includes('bar')).toBe(true)
      expect(res.includes('MarkupContent hover')).toBe(true)
      expect(res.includes('MarkedString hover')).toBe(true)
    })

    it('should filter empty hover message', async () => {
      hoverResult = { contents: [''] }
      let res = await hover.getHover()
      expect(res.length).toBe(0)
    })
  })

  describe('definitionHover', () => {
    it('should load definition from buffer', async () => {
      hoverResult = { contents: 'string hover' }
      let doc = await helper.createDocument()
      await nvim.call('cursor', [1, 1])
      await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'foo\nbar')])
      disposables.push(languages.registerDefinitionProvider([{ language: '*' }], {
        provideDefinition() {
          return [{
            targetUri: doc.uri,
            targetRange: Range.create(0, 0, 1, 3),
            targetSelectionRange: Range.create(0, 0, 0, 3),
          }]
        }
      }))
      await hover.definitionHover('preview')
      let res = await getDocumentText()
      expect(res).toBe('string hover\n\nfoo\nbar')
    })

    it('should load definition link from file', async () => {
      let fsPath = await createTmpFile('foo\nbar\n')
      hoverResult = { contents: 'string hover' }
      let doc = await helper.createDocument()
      await nvim.call('cursor', [1, 1])
      await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'foo\nbar')])
      disposables.push(languages.registerDefinitionProvider([{ language: '*' }], {
        provideDefinition() {
          return [{
            targetUri: URI.file(fsPath).toString(),
            targetRange: Range.create(0, 0, 1, 3),
            targetSelectionRange: Range.create(0, 0, 0, 3),
          }]
        }
      }))
      await hover.definitionHover('preview')
      let res = await getDocumentText()
      expect(res).toBe('string hover\n\nfoo\nbar')
    })
  })
})
