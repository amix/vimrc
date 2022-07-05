import { Neovim } from '@chemzqm/neovim'
import { Disposable, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import Rename from '../../handler/rename'
import languages from '../../languages'
import { disposeAll } from '../../util'
import helper from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
let rename: Rename

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  rename = helper.plugin.getHandler().rename
})

function getWordRangeAtPosition(doc: TextDocument, position: Position): Range | null {
  let lines = doc.getText().split(/\r?\n/)
  let line = lines[position.line]
  if (line.length == 0 || position.character >= line.length) return null
  if (!/\w/.test(line[position.character])) return null
  let start = position.character
  let end = position.character + 1
  if (!/\w/.test(line[start])) {
    return Range.create(position, { line: position.line, character: position.character + 1 })
  }
  while (start >= 0) {
    let ch = line[start - 1]
    if (!ch || !/\w/.test(ch)) break
    start = start - 1
  }
  while (end <= line.length) {
    let ch = line[end]
    if (!ch || !/\w/.test(ch)) break
    end = end + 1
  }
  return Range.create(position.line, start, position.line, end)
}

function getSymbolRanges(textDocument: TextDocument, word: string): Range[] {
  let res: Range[] = []
  let str = ''
  let content = textDocument.getText()
  for (let i = 0, l = content.length; i < l; i++) {
    let ch = content[i]
    if ('-' == ch && str.length == 0) {
      continue
    }
    let isKeyword = /\w/.test(ch)
    if (isKeyword) {
      str = str + ch
    }
    if (str.length > 0 && !isKeyword && str == word) {
      res.push(Range.create(textDocument.positionAt(i - str.length), textDocument.positionAt(i)))
    }
    if (!isKeyword) {
      str = ''
    }
  }
  return res
}

beforeEach(() => {
  disposables.push(languages.registerRenameProvider([{ language: 'javascript' }], {
    provideRenameEdits: (doc, position: Position, newName: string) => {
      let range = getWordRangeAtPosition(doc, position)
      if (range) {
        let word = doc.getText(range)
        if (word) {
          let ranges = getSymbolRanges(doc, word)
          return {
            changes: {
              [doc.uri]: ranges.map(o => TextEdit.replace(o, newName))
            }
          }
        }
      }
      return undefined
    },
    prepareRename: (doc, position) => {
      let range = getWordRangeAtPosition(doc, position)
      return range ? { range, placeholder: doc.getText(range) } : null
    }
  }))
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  disposables = []
})

describe('rename handler', () => {
  describe('getWordEdit', () => {
    it('should not throw when provider not found', async () => {
      await helper.edit()
      let res = await rename.getWordEdit()
      expect(res).toBe(null)
    })

    it('should return null when prepare failed', async () => {
      let doc = await helper.createDocument('t.js')
      await nvim.setLine('你')
      await doc.synchronize()
      let res = await rename.getWordEdit()
      expect(res).toBe(null)
    })

    it('should return workspace edit', async () => {
      let doc = await helper.createDocument('t.js')
      await nvim.setLine('foo foo')
      await doc.synchronize()
      let res = await rename.getWordEdit()
      expect(res).toBeDefined()
      expect(res.changes[doc.uri].length).toBe(2)
    })

    it('should extract words from buffer', async () => {
      let doc = await helper.createDocument('t')
      await nvim.setLine('你 你 你')
      await doc.synchronize()
      let res = await rename.getWordEdit()
      expect(res).toBeDefined()
      expect(res.changes[doc.uri].length).toBe(3)
    })
  })

  describe('rename', () => {
    it('should throw when provider not found', async () => {
      await helper.edit()
      let err
      try {
        await rename.rename('foo')
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should return false for invalid position', async () => {
      await helper.createDocument('t.js')
      let res = await rename.rename('foo')
      expect(res).toBe(false)
    })

    it('should use newName from placeholder', async () => {
      await helper.createDocument('t.js')
      await nvim.setLine('foo foo foo')
      let p = rename.rename()
      await helper.wait(50)
      await nvim.input('<C-u>')
      await helper.wait(10)
      await nvim.input('bar')
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBe(true)
    })

    it('should return false for empty name', async () => {
      await helper.createDocument('t.js')
      await nvim.setLine('foo foo foo')
      let p = rename.rename()
      await helper.wait(50)
      await nvim.input('<C-u>')
      await helper.wait(20)
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBe(false)
    })

    it('should use newName from range', async () => {
      disposables.push(languages.registerRenameProvider([{ language: '*' }], {
        provideRenameEdits: (doc, position: Position, newName: string) => {
          let range = getWordRangeAtPosition(doc, position)
          if (range) {
            let word = doc.getText(range)
            if (word) {
              let ranges = getSymbolRanges(doc, word)
              return {
                changes: {
                  [doc.uri]: ranges.map(o => TextEdit.replace(o, newName))
                }
              }
            }
          }
          return undefined
        },
        prepareRename: (doc, position) => {
          let range = getWordRangeAtPosition(doc, position)
          return range ? range : null
        }
      }))
      await helper.createDocument()
      await nvim.setLine('foo foo foo')
      let p = rename.rename()
      await helper.wait(50)
      await nvim.input('<C-u>')
      await helper.wait(10)
      await nvim.input('bar')
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBe(true)
      await helper.waitFor('getline', ['.'], 'bar bar bar')
    })

    it('should use newName from cword', async () => {
      disposables.push(languages.registerRenameProvider([{ language: '*' }], {
        provideRenameEdits: (doc, position: Position, newName: string) => {
          let range = getWordRangeAtPosition(doc, position)
          if (range) {
            let word = doc.getText(range)
            if (word) {
              let ranges = getSymbolRanges(doc, word)
              return {
                changes: {
                  [doc.uri]: ranges.map(o => TextEdit.replace(o, newName))
                }
              }
            }
          }
          return undefined
        }
      }))
      await helper.createDocument()
      await nvim.setLine('foo foo foo')
      let p = rename.rename()
      await helper.wait(50)
      await nvim.input('<C-u>')
      await helper.wait(50)
      await nvim.input('bar')
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBe(true)
      let line = await nvim.getLine()
      expect(line).toBe('bar bar bar')
    })

    it('should return false when result is empty', async () => {
      disposables.push(languages.registerRenameProvider([{ language: '*' }], {
        provideRenameEdits: () => {
          return null
        }
      }))
      await helper.createDocument()
      await nvim.setLine('foo foo foo')
      let p = rename.rename()
      await helper.wait(50)
      await nvim.input('<C-u>')
      await helper.wait(10)
      await nvim.input('bar')
      await nvim.input('<cr>')
      let res = await p
      expect(res).toBe(false)
    })
  })
})
