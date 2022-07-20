import { Neovim } from '@chemzqm/neovim'
import fs from 'fs'
import os from 'os'
import path from 'path'
import { Disposable, Emitter } from 'vscode-languageserver-protocol'
import { Location, Position, Range, TextEdit } from 'vscode-languageserver-types'
import { URI } from 'vscode-uri'
import events from '../../events'
import { TextDocumentContentProvider } from '../../provider'
import { ConfigurationTarget } from '../../types'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
let tmpFolder = path.join(os.tmpdir(), `coc-${process.pid}`)

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  if (!fs.existsSync(tmpFolder)) fs.mkdirSync(tmpFolder)
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  disposables = []
})

describe('workspace properties', () => {

  it('should have initialized', () => {
    let { nvim, rootPath, uri, insertMode, workspaceFolder, cwd, documents, textDocuments } = workspace
    expect(insertMode).toBe(false)
    expect(nvim).toBeTruthy()
    expect(documents.length).toBe(1)
    expect(textDocuments.length).toBe(1)
    expect(rootPath).toBe(process.cwd())
    expect(cwd).toBe(process.cwd())
    let floatSupported = workspace.floatSupported
    expect(floatSupported).toBe(true)
    let { pluginRoot } = workspace
    expect(typeof pluginRoot).toBe('string')
    let { isVim, isNvim } = workspace
    expect(isVim).toBe(false)
    expect(isNvim).toBe(true)
    expect(uri).toBeDefined()
    expect(workspaceFolder).toBeUndefined()
    let watchmanPath = workspace.getWatchmanPath()
    expect(watchmanPath == null || typeof watchmanPath === 'string').toBe(true)
    let folder = workspace.getWorkspaceFolder(uri)
    expect(folder).toBeUndefined()
  })

  it('should get filetyps', async () => {
    await helper.edit('f.js')
    let filetypes = workspace.filetypes
    expect(filetypes.has('javascript')).toBe(true)
    let languageIds = workspace.languageIds
    expect(languageIds.has('javascript')).toBe(true)
  })

  it('should get channelNames', async () => {
    let names = workspace.channelNames
    expect(Array.isArray(names)).toBe(true)
  })

  it('should work with deprecated method', async () => {
    await nvim.setLine('foo')
    await workspace['moveTo'](Position.create(0, 1))
    let col = await nvim.call('col', ['.'])
    expect(col).toBe(2)
  })
})

describe('workspace methods', () => {
  it('should call vim method', async () => {
    let res = await workspace.callAsync('bufnr', ['%'])
    expect(typeof res).toBe('number')
    let obj: any = workspace.env
    obj.isVim = true
    disposables.push({
      dispose: () => {
        obj.isVim = false
      }
    })
    res = await workspace.callAsync('bufnr', ['%'])
    expect(typeof res).toBe('number')
  })

  it('should get the document', async () => {
    let doc = await workspace.document
    let buf = await nvim.buffer
    expect(doc.buffer.equals(buf)).toBeTruthy()
    doc = workspace.getDocument(doc.uri)
    expect(doc.buffer.equals(buf)).toBeTruthy()
  })

  it('should get attached document', async () => {
    let fn = () => {
      workspace.getAttachedDocument('file://not_exists')
    }
    expect(fn).toThrow(Error)
    await nvim.command(`edit +setl\\ buftype=nofile [tree]`)
    let doc = await workspace.document
    expect(doc.attached).toBe(false)
    fn = () => {
      workspace.getAttachedDocument(doc.bufnr)
    }
    expect(fn).toThrow(Error)
  })

  it('should get format options of without bufnr', async () => {
    let opts = await workspace.getFormatOptions()
    expect(opts.insertSpaces).toBe(true)
    expect(opts.tabSize).toBe(2)
  })

  it('should get format options of current buffer', async () => {
    let buf = await nvim.buffer
    await buf.setVar('coc_trim_trailing_whitespace', 1)
    await buf.setVar('coc_trim_final_newlines', 1)
    await buf.setOption('shiftwidth', 8)
    await buf.setOption('expandtab', false)
    let doc = workspace.getDocument(buf.id)
    let opts = await workspace.getFormatOptions(doc.uri)
    expect(opts).toEqual({
      tabSize: 8,
      insertSpaces: false,
      insertFinalNewline: true,
      trimTrailingWhitespace: true,
      trimFinalNewlines: true
    })
  })

  it('should get format options when uri does not exist', async () => {
    let uri = URI.file('/tmp/foo').toString()
    let opts = await workspace.getFormatOptions(uri)
    expect(opts.insertSpaces).toBe(true)
    expect(opts.tabSize).toBe(2)
  })

  it('should get config files', async () => {
    let file = workspace.getConfigFile(ConfigurationTarget.Global)
    expect(file).toBeFalsy()
    file = workspace.getConfigFile(ConfigurationTarget.User)
    expect(file).toBeTruthy()
    file = workspace.getConfigFile(ConfigurationTarget.Workspace)
    expect(file).toBeTruthy()
  })

  it('should create file watcher', async () => {
    let watcher = workspace.createFileSystemWatcher('**/*.ts')
    expect(watcher).toBeDefined()
  })

  it('should get quickfix item from Location', async () => {
    let filepath = await createTmpFile('quickfix')
    let uri = URI.file(filepath).toString()
    let p = Position.create(0, 0)
    let loc = Location.create(uri, Range.create(p, p))
    let item = await workspace.getQuickfixItem(loc)
    expect(item.filename).toBe(filepath)
    expect(item.text).toBe('quickfix')
  })

  it('should get quickfix list from Locations', async () => {
    let filepathA = await createTmpFile('fileA:1\nfileA:2\nfileA:3')
    let uriA = URI.file(filepathA).toString()
    let filepathB = await createTmpFile('fileB:1\nfileB:2\nfileB:3')
    let uriB = URI.file(filepathB).toString()
    let p1 = Position.create(0, 0)
    let p2 = Position.create(1, 0)
    let locations: Location[] = []
    locations.push(Location.create(uriA, Range.create(p1, p1)))
    locations.push(Location.create(uriA, Range.create(p2, p2)))
    locations.push(Location.create(uriB, Range.create(p1, p1)))
    locations.push(Location.create(uriB, Range.create(p2, p2)))
    let items = await workspace.getQuickfixList(locations)
    expect(items[0].filename).toBe(filepathA)
    expect(items[0].text).toBe('fileA:1')
    expect(items[1].filename).toBe(filepathA)
    expect(items[1].text).toBe('fileA:2')
    expect(items[2].filename).toBe(filepathB)
    expect(items[2].text).toBe('fileB:1')
    expect(items[3].filename).toBe(filepathB)
    expect(items[3].text).toBe('fileB:2')
  })

  it('should get line of document', async () => {
    let doc = await workspace.document
    await nvim.setLine('abc')
    let line = await workspace.getLine(doc.uri, 0)
    expect(line).toBe('abc')
  })

  it('should get line of file', async () => {
    let filepath = await createTmpFile('quickfix')
    let uri = URI.file(filepath).toString()
    let line = await workspace.getLine(uri, 0)
    expect(line).toBe('quickfix')
  })

  it('should read content from buffer', async () => {
    let doc = await workspace.document
    await doc.applyEdits([{ range: Range.create(0, 0, 0, 0), newText: 'foo' }])
    let line = await workspace.readFile(doc.uri)
    expect(line).toBe('foo\n')
  })

  it('should read content from file', async () => {
    let filepath = await createTmpFile('content')
    let content = await workspace.readFile(URI.file(filepath).toString())
    expect(content).toBe(content)
  })

  it('should expand filepath', async () => {
    let home = os.homedir()
    let res = workspace.expand('~/$NODE_ENV/')
    expect(res.startsWith(home)).toBeTruthy()
    expect(res).toContain(process.env.NODE_ENV)

    res = workspace.expand('$HOME/$NODE_ENV/')
    expect(res.startsWith(home)).toBeTruthy()
    expect(res).toContain(process.env.NODE_ENV)
  })

  it('should expand variables', async () => {
    expect(workspace.expand('${workspace}/foo')).toBe(`${workspace.root}/foo`)
    expect(workspace.expand('${env:NODE_ENV}')).toBe(process.env.NODE_ENV)
    expect(workspace.expand('${cwd}')).toBe(workspace.cwd)
    let folder = path.dirname(workspace.root)
    expect(workspace.expand('${workspaceFolderBasename}')).toBe(folder)
    await helper.edit('bar.ts')
    expect(workspace.expand('${file}')).toContain('bar')
    expect(workspace.expand('${fileDirname}')).toBe(path.dirname(__dirname))
    expect(workspace.expand('${fileExtname}')).toBe('.ts')
    expect(workspace.expand('${fileBasename}')).toBe('bar.ts')
    expect(workspace.expand('${fileBasenameNoExtension}')).toBe('bar')
  })

  it('should run command', async () => {
    let res = await workspace.runCommand('ls', __dirname, 1)
    expect(res).toMatch('workspace')
  })

  it('should resolve module path if exists', async () => {
    let res = await workspace.resolveModule('typescript')
    res = await workspace.resolveModule('typescript')
    expect(res).toBeTruthy()
  })

  it('should not resolve module if it does not exist', async () => {
    let res = await workspace.resolveModule('foo')
    res = await workspace.resolveModule('foo')
    expect(res).toBeFalsy()
  })

  it('should return match score for document', async () => {
    let doc = await helper.createDocument('tmp.xml')
    expect(workspace.match(['xml'], doc.textDocument)).toBe(10)
    expect(workspace.match(['wxml'], doc.textDocument)).toBe(0)
    expect(workspace.match([{ language: 'xml' }], doc.textDocument)).toBe(10)
    expect(workspace.match([{ language: 'wxml' }], doc.textDocument)).toBe(0)
    expect(workspace.match([{ pattern: '**/*.xml' }], doc.textDocument)).toBe(5)
    expect(workspace.match([{ pattern: '**/*.html' }], doc.textDocument)).toBe(0)
    expect(workspace.match([{ scheme: 'file' }], doc.textDocument)).toBe(5)
    expect(workspace.match([{ scheme: 'term' }], doc.textDocument)).toBe(0)
    expect(workspace.match([{ language: 'xml' }, { scheme: 'file' }], doc.textDocument)).toBe(10)
  })

  it('should rename buffer', async () => {
    let doc = await helper.createDocument('a')
    let fsPath = URI.parse(doc.uri).fsPath.replace(/a$/, 'b')
    disposables.push(Disposable.create(() => {
      if (fs.existsSync(fsPath)) fs.unlinkSync(fsPath)
    }))
    let p = workspace.renameCurrent()
    await helper.wait(50)
    await nvim.input('<backspace>b<cr>')
    await p
    let name = await nvim.eval('bufname("%")') as string
    expect(name.endsWith('b')).toBe(true)
  })

  it('should rename file', async () => {
    let fsPath = path.join(tmpFolder, 'x')
    let newPath = path.join(tmpFolder, 'b')
    disposables.push(Disposable.create(() => {
      if (fs.existsSync(fsPath)) fs.unlinkSync(fsPath)
      if (fs.existsSync(newPath)) fs.unlinkSync(newPath)
    }))
    fs.writeFileSync(fsPath, 'foo', 'utf8')
    await helper.createDocument(fsPath)
    let p = workspace.renameCurrent()
    await helper.waitFor('mode', [], 'c')
    await nvim.input('<backspace>b<cr>')
    await p
    let name = await nvim.eval('bufname("%")') as string
    expect(name.endsWith('b')).toBe(true)
    expect(fs.existsSync(newPath)).toBe(true)
    let content = fs.readFileSync(newPath, 'utf8')
    expect(content).toMatch(/foo/)
  })

  it('should handle will save event', async () => {
    async function doRename() {
      let fsPath = await createTmpFile('foo', disposables)
      let newPath = path.join(path.dirname(fsPath), 'new_file')
      disposables.push(Disposable.create(() => {
        if (fs.existsSync(newPath)) fs.unlinkSync(newPath)
      }))
      await workspace.renameFile(fsPath, newPath, { overwrite: true })
      if (fs.existsSync(newPath)) fs.unlinkSync(newPath)
    }
    let called = false
    let disposable = workspace.onWillRenameFiles(e => {
      let p = new Promise<void>(resolve => {
        setTimeout(() => {
          called = true
          resolve()
        }, 10)
      })
      e.waitUntil(p)
    })
    await doRename()
    disposable.dispose()
    expect(called).toBe(true)
    called = false
    disposable = workspace.onWillRenameFiles(e => {
      called = true
      e.waitUntil(Promise.resolve({ changes: {} }))
    })
    await doRename()
    expect(called).toBe(true)
    disposable.dispose()
  })
})

describe('workspace utility', () => {

  it('should create database', async () => {
    let db = workspace.createDatabase('test')
    let res = db.exists('xyz')
    expect(res).toBe(false)
    db.destroy()
  })

  it('should get current state', async () => {
    let buf = await helper.edit()
    await buf.setLines(['foo', 'bar'], { start: 0, end: -1, strictIndexing: false })
    await nvim.call('cursor', [2, 2])
    let doc = workspace.getDocument(buf.id)
    let state = await workspace.getCurrentState()
    expect(doc.uri).toBe(state.document.uri)
    expect(state.position).toEqual({ line: 1, character: 1 })
  })

  it('should findUp to tsconfig.json from current file', async () => {
    await helper.edit(path.join(__dirname, 'edit'))
    let filepath = await workspace.findUp('tsconfig.json')
    expect(filepath).toMatch('tsconfig.json')
  })

  it('should findUp from current file ', async () => {
    await helper.edit('foo')
    let filepath = await workspace.findUp('tsconfig.json')
    expect(filepath).toMatch('tsconfig.json')
  })

  it('should not findUp from file in other directory', async () => {
    await nvim.command(`edit ${path.join(os.tmpdir(), 'foo')}`)
    let filepath = await workspace.findUp('tsconfig.json')
    expect(filepath).toBeNull()
  })

  it('should register autocmd', async () => {
    let event: any
    let eventCount = 0
    let disposables = []
    disposables.push(workspace.registerAutocmd({
      event: 'TextYankPost',
      arglist: ['v:event'],
      callback: ev => {
        eventCount += 1
        event = ev
      }
    }))
    disposables.push(workspace.registerAutocmd({
      event: ['InsertEnter', 'CursorMoved'],
      callback: () => {
        eventCount += 1
      }
    }))
    await nvim.setLine('foo')
    await helper.wait(30)
    await nvim.command('normal! yy')
    await helper.wait(30)
    await nvim.command('normal! Abar')
    await helper.wait(30)
    expect(event.regtype).toBe('V')
    expect(event.operator).toBe('y')
    expect(event.regcontents).toEqual(['foo'])
    expect(eventCount).toBeGreaterThan(2)
    disposables.forEach(d => d.dispose())
  })

  it('should regist keymap', async () => {
    let fn = jest.fn()
    await nvim.command('nmap go <Plug>(coc-echo)')
    let disposable = workspace.registerKeymap(['n', 'v'], 'echo', fn, { sync: true })
    await helper.wait(30)
    let { mode } = await nvim.mode
    expect(mode).toBe('n')
    await nvim.call('feedkeys', ['go', 'i'])
    await helper.wait(50)
    expect(fn).toBeCalledTimes(1)
    disposable.dispose()
    await nvim.call('feedkeys', ['go', 'i'])
    await helper.wait(50)
    expect(fn).toBeCalledTimes(1)
  })

  it('should regist expr keymap', async () => {
    let called = false
    let fn = () => {
      called = true
      return '""'
    }
    await nvim.input('i')
    let { mode } = await nvim.mode
    expect(mode).toBe('i')
    let disposable = workspace.registerExprKeymap('i', '"', fn)
    await helper.wait(30)
    await nvim.call('feedkeys', ['"', 't'])
    await helper.wait(30)
    expect(called).toBe(true)
    let line = await nvim.line
    expect(line).toBe('""')
    disposable.dispose()
  })

  it('should regist buffer expr keymap', async () => {
    let fn = () => '""'
    await nvim.input('i')
    let disposable = workspace.registerExprKeymap('i', '"', fn, true)
    await helper.wait(30)
    await nvim.call('feedkeys', ['"', 't'])
    await helper.wait(30)
    let line = await nvim.line
    expect(line).toBe('""')
    disposable.dispose()
  })

  it('should watch options', async () => {
    let fn = jest.fn()
    workspace.watchOption('showmode', fn, disposables)
    workspace.watchOption('showmode', fn)
    await helper.wait(30)
    await nvim.command('set showmode')
    await helper.wait(30)
    expect(fn).toBeCalled()
    await nvim.command('noa set noshowmode')
  })

  it('should watch global', async () => {
    let fn = jest.fn()
    workspace.watchGlobal('x', fn, disposables)
    workspace.watchGlobal('x', fn)
    workspace.watchGlobal('x')
    await nvim.command('let g:x = 1')
    await helper.wait(30)
    expect(fn).toBeCalled()
  })

  it('should check nvim version', async () => {
    expect(workspace.has('patch-7.4.248')).toBe(false)
    expect(workspace.has('nvim-0.5.0')).toBe(true)
    expect(workspace.has('nvim-0.9.0')).toBe(false)
  })
})

describe('workspace events', () => {

  it('should listen to fileType change', async () => {
    let buf = await helper.edit()
    await nvim.command('setf xml')
    await helper.wait(50)
    let doc = workspace.getDocument(buf.id)
    expect(doc.filetype).toBe('xml')
  })

  it('should fire onDidOpenTextDocument', async () => {
    let fn = jest.fn()
    workspace.onDidOpenTextDocument(fn, null, disposables)
    await helper.edit()
    await helper.wait(30)
    expect(fn).toHaveBeenCalledTimes(1)
  })

  it('should fire onDidChangeTextDocument', async () => {
    let fn = jest.fn()
    await helper.edit()
    workspace.onDidChangeTextDocument(fn, null, disposables)
    await nvim.setLine('foo')
    let doc = await workspace.document
    doc.forceSync()
    await helper.wait(20)
    expect(fn).toHaveBeenCalledTimes(1)
  })

  it('should fire onDidChangeConfiguration', async () => {
    let fn = jest.fn()
    let disposable = workspace.onDidChangeConfiguration(e => {
      disposable.dispose()
      expect(e.affectsConfiguration('tsserver')).toBe(true)
      expect(e.affectsConfiguration('tslint')).toBe(false)
      fn()
    })
    let config = workspace.getConfiguration('tsserver')
    config.update('enable', false)
    expect(fn).toHaveBeenCalledTimes(1)
    config.update('enable', undefined)
  })

  it('should get empty configuration for none exists section', () => {
    let config = workspace.getConfiguration('notexists')
    let keys = Object.keys(config)
    expect(keys.length).toBe(0)
  })

  it('should fire onWillSaveUntil', async () => {
    let doc = await workspace.document
    let filepath = URI.parse(doc.uri).fsPath
    let fn = jest.fn()
    let disposable = workspace.onWillSaveTextDocument(event => {
      let promise = new Promise<TextEdit[]>(resolve => {
        fn()
        let edit: TextEdit = {
          newText: 'foo',
          range: Range.create(0, 0, 0, 0)
        }
        resolve([edit])
      })
      event.waitUntil(promise)
    })
    await nvim.setLine('bar')
    await helper.wait(30)
    await events.fire('BufWritePre', [doc.bufnr, doc.bufname])
    await helper.wait(30)
    let content = doc.getDocumentContent()
    expect(content.startsWith('foobar')).toBe(true)
    disposable.dispose()
    expect(fn).toBeCalledTimes(1)
    if (fs.existsSync(filepath)) {
      fs.unlinkSync(filepath)
    }
  })

  it('should not work for async waitUntil', async () => {
    let doc = await helper.createDocument()
    let filepath = URI.parse(doc.uri).fsPath
    let disposable = workspace.onWillSaveTextDocument(event => {
      setTimeout(() => {
        let edit: TextEdit = {
          newText: 'foo',
          range: Range.create(0, 0, 0, 0)
        }
        event.waitUntil(Promise.resolve([edit]))
      }, 30)
    })
    await nvim.setLine('bar')
    await helper.wait(30)
    await nvim.command('wa')
    let content = doc.getDocumentContent()
    expect(content).toMatch('bar')
    disposable.dispose()
    if (fs.existsSync(filepath)) {
      fs.unlinkSync(filepath)
    }
  })

  it('should only use first returned textEdits', async () => {
    let doc = await helper.createDocument()
    let filepath = URI.parse(doc.uri).fsPath
    disposables.push(Disposable.create(() => {
      if (fs.existsSync(filepath)) {
        fs.unlinkSync(filepath)
      }
    }))
    workspace.onWillSaveTextDocument(event => {
      event.waitUntil(Promise.resolve(undefined))
    }, null, disposables)
    workspace.onWillSaveTextDocument(event => {
      let promise = new Promise<TextEdit[]>(resolve => {
        setTimeout(() => {
          let edit: TextEdit = {
            newText: 'foo',
            range: Range.create(0, 0, 0, 0)
          }
          resolve([edit])
        }, 10)
      })
      event.waitUntil(promise)
    }, null, disposables)
    workspace.onWillSaveTextDocument(event => {
      let promise = new Promise<TextEdit[]>(resolve => {
        setTimeout(() => {
          let edit: TextEdit = {
            newText: 'bar',
            range: Range.create(0, 0, 0, 0)
          }
          resolve([edit])
        }, 30)
      })
      event.waitUntil(promise)
    }, null, disposables)
    await nvim.setLine('bar')
    await helper.wait(30)
    await nvim.command('wa')
    let content = doc.getDocumentContent()
    expect(content).toMatch('foo')
  })

  it('should attach & detach', async () => {
    let buf = await helper.edit()
    await nvim.command('CocDisable')
    let doc = workspace.getDocument(buf.id)
    expect(doc).toBeUndefined()
    await nvim.command('CocEnable')
    doc = workspace.getDocument(buf.id)
    expect(doc.bufnr).toBe(buf.id)
  })
})

describe('workspace textDocument content provider', () => {

  it('should regist document content provider', async () => {
    let provider: TextDocumentContentProvider = {
      provideTextDocumentContent: (_uri, _token): string => 'sample text'
    }
    workspace.registerTextDocumentContentProvider('test', provider)
    await nvim.command('edit test://1')
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual(['sample text'])
  })

  it('should react on change event of document content provider', async () => {
    let text = 'foo'
    let emitter = new Emitter<URI>()
    let event = emitter.event
    let provider: TextDocumentContentProvider = {
      onDidChange: event,
      provideTextDocumentContent: (_uri, _token): string => text
    }
    workspace.registerTextDocumentContentProvider('jdk', provider)
    workspace.autocmds.setupDynamicAutocmd(true)
    await nvim.command('edit jdk://1')
    await workspace.document
    text = 'bar'
    emitter.fire(URI.parse('jdk://1'))
    await helper.waitFor('getline', ['.'], 'bar')
  })
})

describe('workspace registerBufferSync', () => {
  it('should regist', async () => {
    await helper.createDocument()
    let created = 0
    let deleted = 0
    let changed = 0
    let disposable = workspace.registerBufferSync(() => {
      created = created + 1
      return {
        dispose: () => {
          deleted += 1
        },
        onChange: () => {
          changed += 1
        }
      }
    })
    disposables.push(disposable)
    let doc = await helper.createDocument()
    expect(created).toBe(2)
    await doc.applyEdits([TextEdit.insert(Position.create(0, 0), 'foo')])
    expect(changed).toBe(1)
    await nvim.command('bd!')
    expect(deleted).toBe(1)
  })

  it('should invoke onTextChange', async () => {
    let called = 0
    disposables.push(workspace.registerBufferSync(() => {
      return {
        dispose: () => {
        },
        onTextChange: () => {
          called = called + 1
        }
      }
    }))
    let doc = await helper.createDocument()
    await nvim.setLine('foo')
    await doc.synchronize()
    expect(called).toBe(1)
  })
})
