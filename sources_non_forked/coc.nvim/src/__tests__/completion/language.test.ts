import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import { CompletionItem, CompletionList, InsertTextFormat, Position, Range, TextEdit } from 'vscode-languageserver-types'
import languages from '../../languages'
import { CompletionItemProvider } from '../../provider'
import snippetManager from '../../snippets/manager'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  await helper.reset()
})

describe('language source', () => {
  describe('additionalTextEdits', () => {
    it('should fix cursor position with plain text on additionalTextEdits', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'foo',
          filterText: 'foo',
          additionalTextEdits: [TextEdit.insert(Position.create(0, 0), 'a\nbar')]
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('if')
      await helper.waitPopup()
      await helper.selectCompleteItem(0)
      await helper.waitFor('getline', ['.'], 'barfoo')
    })

    it('should fix cursor position with snippet on additionalTextEdits', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'if',
          insertTextFormat: InsertTextFormat.Snippet,
          textEdit: { range: Range.create(0, 0, 0, 1), newText: 'if($1)' },
          additionalTextEdits: [TextEdit.insert(Position.create(0, 0), 'bar ')],
          preselect: true
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('ii')
      await helper.waitPopup()
      let res = await helper.getItems()
      let idx = res.findIndex(o => o.menu == '[edit]')
      await helper.selectCompleteItem(idx)
      await helper.waitFor('col', ['.'], 8)
    })

    it('should fix cursor position with plain text snippet on additionalTextEdits', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'if',
          insertTextFormat: InsertTextFormat.Snippet,
          textEdit: { range: Range.create(0, 0, 0, 2), newText: 'do$0' },
          additionalTextEdits: [TextEdit.insert(Position.create(0, 0), 'bar ')],
          preselect: true
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('iif')
      await helper.waitPopup()
      let items = await helper.getItems()
      let idx = items.findIndex(o => o.word == 'do' && o.menu == '[edit]')
      await helper.selectCompleteItem(idx)
      await helper.waitFor('getline', ['.'], 'bar do')
      await helper.waitFor('col', ['.'], 7)
    })

    it('should fix cursor position with nested snippet on additionalTextEdits', async () => {
      let res = await snippetManager.insertSnippet('func($1)$0')
      expect(res).toBe(true)
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'if',
          insertTextFormat: InsertTextFormat.Snippet,
          insertText: 'do$0',
          additionalTextEdits: [TextEdit.insert(Position.create(0, 0), 'bar ')],
          preselect: true
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('if')
      await helper.waitPopup()
      await helper.selectCompleteItem(0)
      await helper.waitFor('getline', ['.'], 'bar func(do)')
      let [, lnum, col] = await nvim.call('getcurpos')
      expect(lnum).toBe(1)
      expect(col).toBe(12)
    })

    it('should fix cursor position and keep placeholder with snippet on additionalTextEdits', async () => {
      let text = 'foo0bar1'
      await nvim.setLine(text)
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'var',
          insertTextFormat: InsertTextFormat.Snippet,
          textEdit: { range: Range.create(0, text.length + 1, 0, text.length + 1), newText: '${1:foo} = foo0bar1' },
          additionalTextEdits: [TextEdit.del(Range.create(0, 0, 0, text.length + 1))],
          preselect: true
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider, ['.']))
      await nvim.input('A.')
      await helper.waitPopup()
      let res = await helper.getItems()
      let idx = res.findIndex(o => o.menu == '[edit]')
      await helper.selectCompleteItem(idx)
      await helper.waitFor('getline', ['.'], 'foo = foo0bar1')
      await helper.wait(50)
      expect(snippetManager.session).toBeDefined()
      let [, lnum, col] = await nvim.call('getcurpos')
      expect(lnum).toBe(1)
      expect(col).toBe(3)
    })

    it('should cancel current snippet session when additionalTextEdits inside snippet', async () => {
      await nvim.input('i')
      await snippetManager.insertSnippet('foo($1, $2)$0', true)
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'bar',
          insertTextFormat: InsertTextFormat.Snippet,
          textEdit: { range: Range.create(0, 4, 0, 5), newText: 'bar($1)' },
          additionalTextEdits: [TextEdit.del(Range.create(0, 0, 0, 3))]
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider, ['.']))
      await nvim.input('b')
      await helper.waitPopup()
      let res = await helper.getItems()
      let idx = res.findIndex(o => o.menu == '[edit]')
      await helper.selectCompleteItem(idx)
      await helper.waitFor('getline', ['.'], '(bar(), )')
      let col = await nvim.call('col', ['.'])
      expect(col).toBe(6)
    })
  })

  describe('filterText', () => {
    it('should fix input for snippet item', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'foo',
          filterText: 'foo',
          insertText: '${1:foo}($2)',
          insertTextFormat: InsertTextFormat.Snippet,
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('snippets-test', 'st', null, provider))
      await nvim.input('if')
      await helper.waitPopup()
      await nvim.input('<C-n>')
      await helper.waitFor('getline', ['.'], 'foo')
    })

    it('should fix filterText of complete item', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'name',
          sortText: '11',
          textEdit: {
            range: Range.create(0, 1, 0, 2),
            newText: '?.name'
          }
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('name', 'N', null, provider, ['.']))
      await nvim.setLine('t')
      await nvim.input('A.')
      await helper.waitPopup()
      await helper.selectCompleteItem(0)
      await helper.waitFor('getline', ['.'], 't?.name')
    })
  })

  describe('inComplete result', () => {
    it('should filter in complete request', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (doc, pos, token, context): Promise<CompletionList> => {
          let option = (context as any).option
          if (context.triggerCharacter == '.') {
            return {
              isIncomplete: true,
              items: [
                {
                  label: 'foo'
                }, {
                  label: 'bar'
                }
              ]
            }
          }
          if (option.input == 'f') {
            if (token.isCancellationRequested) return
            return {
              isIncomplete: true,
              items: [
                {
                  label: 'foo'
                }
              ]
            }
          }
          if (option.input == 'fo') {
            if (token.isCancellationRequested) return
            return {
              isIncomplete: false,
              items: [
                {
                  label: 'foo'
                }
              ]
            }
          }
        }
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider, ['.']))
      await nvim.input('i.')
      await helper.waitPopup()
      await nvim.input('fo')
      await helper.wait(50)
      let res = await helper.getItems()
      expect(res.length).toBe(1)
    })
  })

  describe('textEdit', () => {
    it('should fix bad range', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: 'foo',
          filterText: 'foo',
          textEdit: { range: Range.create(0, 0, 0, 0), newText: 'foo' },
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('if')
      await helper.waitPopup()
      await helper.selectCompleteItem(0)
      await helper.waitFor('getline', ['.'], 'foo')
    })

    it('should applyEdits for empty word', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (): Promise<CompletionItem[]> => [{
          label: '',
          filterText: '!',
          textEdit: { range: Range.create(0, 0, 0, 1), newText: 'foo' },
          data: { word: '' }
        }]
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider, ['!']))
      await nvim.input('i!')
      await helper.waitPopup()
      await helper.selectCompleteItem(0)
      await helper.waitFor('getline', ['.'], 'foo')
    })

    it('should provide word when textEdit after startcol', async () => {
      // some LS would send textEdit after first character,
      // need fix the word from newText
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (_, position): Promise<CompletionItem[]> => {
          if (position.line != 0) return null
          return [{
            label: 'bar',
            filterText: 'ar',
            textEdit: {
              range: Range.create(0, 1, 0, 1),
              newText: 'ar'
            }
          }]
        }
      }
      disposables.push(languages.registerCompletionItemProvider('edits', 'edit', null, provider))
      await nvim.input('ib')
      await helper.waitPopup()
      let context = await nvim.getVar('coc#_context') as any
      expect(context.start).toBe(1)
      expect(context.candidates[0].word).toBe('ar')
    })

    it('should adjust completion position by textEdit start position', async () => {
      let provider: CompletionItemProvider = {
        provideCompletionItems: async (_document, _position, _token, context): Promise<CompletionItem[]> => {
          if (!context.triggerCharacter) return
          return [{
            label: 'foo',
            textEdit: {
              range: Range.create(0, 0, 0, 1),
              newText: '?foo'
            }
          }]
        }
      }
      disposables.push(languages.registerCompletionItemProvider('fix', 'f', null, provider, ['?']))
      await nvim.input('i?')
      await helper.waitPopup()
      await nvim.eval('feedkeys("\\<C-n>", "in")')
      await helper.waitFor('getline', ['.'], '?foo')
    })
  })
})
