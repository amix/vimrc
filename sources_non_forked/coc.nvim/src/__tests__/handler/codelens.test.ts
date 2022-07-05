import { Neovim } from '@chemzqm/neovim'
import { Command, CodeLens, Disposable, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import commands from '../../commands'
import events from '../../events'
import CodeLensHandler from '../../handler/codelens/index'
import CodeLensBuffer, { getCommands } from '../../handler/codelens/buffer'
import languages from '../../languages'
import { disposeAll } from '../../util'
import helper from '../helper'
import workspace from '../../workspace'

let nvim: Neovim
let codeLens: CodeLensHandler
let disposables: Disposable[] = []
let srcId: number

jest.setTimeout(10000)
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  srcId = await nvim.createNamespace('coc-codelens')
  codeLens = helper.plugin.getHandler().codeLens
})

beforeEach(() => {
  helper.updateConfiguration('codeLens.enable', true)
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
})

async function createBufferWithCodeLens(): Promise<CodeLensBuffer> {
  disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
    provideCodeLenses: () => {
      return [{
        range: Range.create(0, 0, 0, 1)
      }]
    },
    resolveCodeLens: codeLens => {
      codeLens.command = Command.create('save', '__save', 1, 2, 3)
      return codeLens
    }
  }))
  let doc = await helper.createDocument('e.js')
  await nvim.call('setline', [1, ['a', 'b', 'c']])
  await doc.synchronize()
  await codeLens.checkProvider()
  return codeLens.buffers.getItem(doc.bufnr)
}

describe('codeLenes featrue', () => {
  it('should do codeLenes request and resolve codeLenes', async () => {
    let buf = await createBufferWithCodeLens()
    let doc = await workspace.document
    let codelens = buf.currentCodeLens
    expect(codelens).toBeDefined()
    expect(codelens[0].command).toBeDefined()
    let markers = await helper.getMarkers(doc.bufnr, srcId)
    expect(markers.length).toBe(1)
  })

  it('should refresh on empty changes', async () => {
    await createBufferWithCodeLens()
    let doc = await workspace.document
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await doc.synchronize()
    let markers = await helper.getMarkers(doc.bufnr, srcId)
    expect(markers.length).toBe(1)
  })

  it('should work with empty codeLens', async () => {
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: () => {
        return []
      }
    }))
    let doc = await helper.createDocument('t.js')
    let buf = codeLens.buffers.getItem(doc.bufnr)
    let codelens = buf.currentCodeLens
    expect(codelens).toBeUndefined()
  })

  it('should change codeLenes position', async () => {
    let fn = jest.fn()
    helper.updateConfiguration('codeLens.position', 'eol')
    disposables.push(commands.registerCommand('__save', (...args) => {
      fn(...args)
    }))
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: () => {
        return [{
          range: Range.create(0, 0, 0, 1)
        }]
      },
      resolveCodeLens: codeLens => {
        codeLens.command = Command.create('save', '__save', 1, 2, 3)
        return codeLens
      }
    }))
    let doc = await helper.createDocument('example.js')
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await codeLens.checkProvider()
    let res = await doc.buffer.getExtMarks(srcId, 0, -1, { details: true })
    expect(res.length).toBeGreaterThan(0)
    let arr = res[0][3]['virt_text']
    expect(arr[0][0]).toBe('save')
  })

  it('should refresh codeLens on CursorHold', async () => {
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: document => {
        let n = document.lineCount
        let arr: any[] = []
        for (let i = 0; i <= n - 2; i++) {
          arr.push({
            range: Range.create(i, 0, i, 1),
            command: Command.create('save', '__save', i)
          })
        }
        return arr
      }
    }))
    let doc = await helper.createDocument('example.js')
    await helper.wait(100)
    let markers = await helper.getMarkers(doc.bufnr, srcId)
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await doc.synchronize()
    await events.fire('CursorHold', [doc.bufnr])
    await helper.wait(200)
    markers = await helper.getMarkers(doc.bufnr, srcId)
    expect(markers.length).toBe(3)
  })

  it('should cancel codeLenes request on document change', async () => {
    let cancelled = false
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: (_, token) => {
        return new Promise(resolve => {
          token.onCancellationRequested(() => {
            cancelled = true
            clearTimeout(timer)
            resolve(null)
          })
          let timer = setTimeout(() => {
            resolve([{
              range: Range.create(0, 0, 0, 1)
            }, {
              range: Range.create(1, 0, 1, 1)
            }])
          }, 2000)
          disposables.push({
            dispose: () => {
              clearTimeout(timer)
            }
          })
        })
      },
      resolveCodeLens: codeLens => {
        codeLens.command = Command.create('save', '__save')
        return codeLens
      }
    }))
    let doc = await helper.createDocument('codelens.js')
    await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'a\nb\nc')])
    expect(cancelled).toBe(true)
  })

  it('should resolve on CursorMoved', async () => {
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: () => {
        return [{
          range: Range.create(90, 0, 90, 1)
        }, {
          range: Range.create(91, 0, 91, 1)
        }]
      },
      resolveCodeLens: async codeLens => {
        codeLens.command = Command.create('save', '__save')
        return codeLens
      }
    }))
    let doc = await helper.createDocument('example.js')
    let arr = new Array(100)
    arr.fill('')
    await nvim.call('setline', [1, arr])
    await doc.synchronize()
    await codeLens.checkProvider()
    await nvim.command('normal! gg')
    await nvim.command('normal! G')
    await helper.wait(100)
    let buf = codeLens.buffers.getItem(doc.bufnr)
    let codelens = buf.currentCodeLens
    expect(codelens).toBeDefined()
    expect(codelens[0].command).toBeDefined()
    expect(codelens[1].command).toBeDefined()
  })

  it('should invoke codeLenes action', async () => {
    let fn = jest.fn()
    disposables.push(commands.registerCommand('__save', (...args) => {
      fn(...args)
    }))
    await createBufferWithCodeLens()
    await helper.doAction('codeLensAction')
    expect(fn).toBeCalledWith(1, 2, 3)
    await nvim.command('normal! G')
    await helper.doAction('codeLensAction')
  })

  it('should use picker for multiple codeLenses', async () => {
    let fn = jest.fn()
    disposables.push(commands.registerCommand('__save', (...args) => {
      fn(...args)
    }))
    disposables.push(commands.registerCommand('__delete', (...args) => {
      fn(...args)
    }))
    disposables.push(languages.registerCodeLensProvider([{ language: 'javascript' }], {
      provideCodeLenses: () => {
        return [{
          range: Range.create(0, 0, 0, 1),
          command: Command.create('save', '__save', 1, 2, 3)
        }, {
          range: Range.create(0, 1, 0, 2),
          command: Command.create('save', '__delete', 4, 5, 6)
        }]
      }
    }))
    let doc = await helper.createDocument('example.js')
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await doc.synchronize()
    await codeLens.checkProvider()
    let p = helper.doAction('codeLensAction')
    await helper.waitFloat()
    await nvim.input('<cr>')
    await p
    expect(fn).toBeCalledWith(1, 2, 3)
  })

  it('should refresh for failed codeLens request', async () => {
    let called = 0
    let fn = jest.fn()
    disposables.push(commands.registerCommand('__save', (...args) => {
      fn(...args)
    }))
    disposables.push(commands.registerCommand('__foo', (...args) => {
      fn(...args)
    }))
    disposables.push(languages.registerCodeLensProvider([{ language: '*' }], {
      provideCodeLenses: () => {
        called++
        if (called == 1) {
          return null
        }
        return [{
          range: Range.create(0, 0, 0, 1),
          command: Command.create('foo', '__foo')
        }]
      }
    }))
    disposables.push(languages.registerCodeLensProvider([{ language: '*' }], {
      provideCodeLenses: () => {
        return [{
          range: Range.create(0, 0, 0, 1),
          command: Command.create('save', '__save')
        }]
      }
    }))
    let doc = await helper.createDocument('example.js')
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await codeLens.checkProvider()
    let markers = await helper.getMarkers(doc.buffer.id, srcId)
    expect(markers.length).toBeGreaterThan(0)
    let codeLensBuffer = codeLens.buffers.getItem(doc.buffer.id)
    await codeLensBuffer.forceFetch()
    let curr = codeLensBuffer.currentCodeLens
    expect(curr.length).toBeGreaterThan(1)
  })

  it('should use custom separator & position', async () => {
    helper.updateConfiguration('codeLens.separator', '|')
    helper.updateConfiguration('codeLens.position', 'eol')
    let doc = await helper.createDocument('example.js')
    await nvim.call('setline', [1, ['a', 'b', 'c']])
    await doc.synchronize()
    disposables.push(languages.registerCodeLensProvider([{ language: '*' }], {
      provideCodeLenses: () => {
        return [{
          range: Range.create(0, 0, 1, 0),
          command: Command.create('save', '__save')
        }, {
          range: Range.create(0, 0, 1, 0),
          command: Command.create('save', '__save')
        }]
      }
    }))
    await codeLens.checkProvider()
    let res = await doc.buffer.getExtMarks(srcId, 0, -1, { details: true })
    expect(res.length).toBe(1)
  })

  it('should get commands from codeLenses', async () => {
    expect(getCommands(1, undefined)).toEqual([])
    let codeLenses = [CodeLens.create(Range.create(0, 0, 0, 0))]
    expect(getCommands(0, codeLenses)).toEqual([])
    codeLenses = [CodeLens.create(Range.create(0, 0, 1, 0)), CodeLens.create(Range.create(2, 0, 3, 0))]
    codeLenses[0].command = Command.create('save', '__save')
    expect(getCommands(0, codeLenses).length).toEqual(1)
  })
})
