import { Buffer, Neovim } from '@chemzqm/neovim'
import { HighlightItem } from '@chemzqm/neovim/lib/api/Buffer'
import fs from 'fs-extra'
import os from 'os'
import path from 'path'
import { Disposable, Emitter } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import commands from '../../commands'
import events from '../../events'
import languages from '../../languages'
import { TreeItem, TreeItemCollapsibleState } from '../../tree'
import { MessageLevel } from '../../types'
import { disposeAll } from '../../util'
import window from '../../window'
import workspace from '../../workspace'
import extensions from '../../extensions'
import Notification from '../../model/notification'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []

interface FileNode {
  filepath: string
  isFolder?: boolean
}

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

beforeEach(() => {
  helper.updateConfiguration('coc.preferences.enableMessageDialog', true)
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  disposables = []
})

describe('window', () => {
  describe('functions', () => {
    it('should get offset', async () => {
      let buf = await nvim.buffer
      await nvim.call('setline', [buf.id, ['bar', 'foo']])
      await nvim.call('cursor', [2, 2])
      let n = await window.getOffset()
      expect(n).toBe(5)
    })

    it('should selected range', async () => {
      await nvim.setLine('foobar')
      await nvim.command('normal! viw')
      await nvim.eval(`feedkeys("\\<Esc>", 'in')`)
      let range = await window.getSelectedRange('v')
      expect(range).toEqual({ start: { line: 0, character: 0 }, end: { line: 0, character: 6 } })
    })

    it('should run terminal command', async () => {
      let res = await window.runTerminalCommand('ls', __dirname)
      expect(res.success).toBe(true)
    })

    it('should open temimal buffer', async () => {
      let bufnr = await window.openTerminal('ls', { autoclose: false, keepfocus: false })
      let curr = await nvim.eval('bufnr("%")')
      expect(curr).toBe(bufnr)
      let buftype = await nvim.eval('&buftype')
      expect(buftype).toBe('terminal')
    })

    it('should create outputChannel', () => {
      let channel = window.createOutputChannel('channel')
      expect(channel.name).toBe('channel')
    })

    it('should create TreeView instance', async () => {
      let emitter = new Emitter<FileNode | undefined>()
      let removed = false
      let treeView = window.createTreeView('files', {
        treeDataProvider: {
          onDidChangeTreeData: emitter.event,
          getChildren: root => {
            if (root) return undefined
            if (removed) return [{ filepath: '/foo/a', isFolder: true }]
            return [{ filepath: '/foo/a', isFolder: true }, { filepath: '/foo/b.js' }]
          },
          getTreeItem: (node: FileNode) => {
            let { filepath, isFolder } = node
            return new TreeItem(URI.file(filepath), isFolder ? TreeItemCollapsibleState.Collapsed : TreeItemCollapsibleState.None)
          },
        }
      })
      disposables.push(emitter)
      disposables.push(treeView)
      await treeView.show()
      let filetype = await nvim.eval('&filetype')
      expect(filetype).toBe('coctree')
    })

    it('should show outputChannel', async () => {
      window.createOutputChannel('channel')
      window.showOutputChannel('channel')
      let buf = await nvim.buffer
      let name = await buf.name
      expect(name).toMatch('channel')
    })

    it('should not show none exists channel', async () => {
      let buf = await nvim.buffer
      let bufnr = buf.id
      window.showOutputChannel('NONE')
      await helper.wait(20)
      buf = await nvim.buffer
      expect(buf.id).toBe(bufnr)
    })

    it('should get cursor position', async () => {
      await nvim.setLine('       ')
      await nvim.call('cursor', [1, 3])
      let pos = await window.getCursorPosition()
      expect(pos).toEqual({
        line: 0,
        character: 2
      })
    })

    it('should moveTo position in insert mode', async () => {
      await nvim.setLine('foo')
      await nvim.input('i')
      await window.moveTo({ line: 0, character: 3 })
      let col = await nvim.call('col', '.')
      expect(col).toBe(4)
      let virtualedit = await nvim.getOption('virtualedit')
      expect(virtualedit).toBe('')
    })

    it('should choose quickpick', async () => {
      let p = window.showQuickpick(['a', 'b'])
      await helper.wait(50)
      await nvim.input('1')
      await nvim.input('<CR>')
      let res = await p
      expect(res).toBe(0)
    })

    it('should cancel quickpick', async () => {
      let p = window.showQuickpick(['a', 'b'])
      await helper.wait(50)
      await nvim.input('<esc>')
      let res = await p
      expect(res).toBe(-1)
    })

    it('should show prompt', async () => {
      let p = window.showPrompt('prompt')
      await helper.wait(50)
      await nvim.input('y')
      let res = await p
      expect(res).toBe(true)
    })

    it('should show dialog', async () => {
      let dialog = await window.showDialog({ content: 'foo' })
      let winid = await dialog.winid
      expect(winid).toBeDefined()
      expect(winid).toBeGreaterThan(1000)
    })

    it('should show menu', async () => {
      let p = window.showMenuPicker(['a', 'b', 'c'], 'choose item')
      await helper.wait(50)
      let exists = await nvim.call('coc#float#has_float', [])
      expect(exists).toBe(1)
      await nvim.input('2')
      let res = await p
      expect(res).toBe(1)
    })

    it('should return select items for picker', async () => {
      let curr = await nvim.call('win_getid')
      let p = window.showPickerDialog(['foo', 'bar'], 'select')
      await helper.waitFloat()
      await helper.wait(30)
      await nvim.input(' ')
      await nvim.input('<cr>')
      let res = await p
      let winid = await nvim.call('win_getid')
      expect(winid).toBe(curr)
      expect(res).toEqual(['foo'])
    })

    it('should throw when workspace folder does not exist', async () => {
      helper.updateConfiguration('coc.preferences.rootPatterns', [])
      await nvim.command('enew')
      let err
      try {
        await window.openLocalConfig()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
      await nvim.command(`e ${path.join(os.tmpdir(), 'a')}`)
      err
      try {
        await window.openLocalConfig()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
      await nvim.command(`e t.md`)
      await nvim.command('setf markdown')
      err
      try {
        await window.openLocalConfig()
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })

    it('should open local config', async () => {
      let dir = path.join(os.tmpdir(), '.vim')
      if (fs.existsSync(dir)) {
        fs.emptyDirSync(dir)
        fs.rmdirSync(dir)
      }
      if (!fs.existsSync(path.join(os.tmpdir(), '.git'))) {
        fs.mkdirSync(path.join(os.tmpdir(), '.git'))
      }
      await helper.edit(path.join(os.tmpdir(), 't'))
      let root = workspace.root
      expect(root).toBe(os.tmpdir())
      let p = window.openLocalConfig()
      await helper.wait(50)
      await nvim.input('y')
      await p
      let bufname = await nvim.call('bufname', ['%'])
      expect(bufname).toMatch('coc-settings.json')
    })

    describe('should get messageLevel', () => {
      helper.updateConfiguration('coc.preferences.messageLevel', 'error')
      let level = window.messageLevel
      expect(level).toBe(MessageLevel.Error)
      helper.updateConfiguration('coc.preferences.messageLevel', 'warning')
      level = window.messageLevel
      expect(level).toBe(MessageLevel.Warning)
    })
  })

  describe('window input', () => {
    it('should request input', async () => {
      let winid = await nvim.call('win_getid')
      let p = window.requestInput('Name')
      await helper.wait(50)
      await nvim.input('bar<enter>')
      let res = await p
      let curr = await nvim.call('win_getid')
      expect(curr).toBe(winid)
      expect(res).toBe('bar')
    })

    it('should return empty string when input empty', async () => {
      let p = window.requestInput('Name')
      await helper.wait(30)
      await nvim.input('<enter>')
      let res = await p
      expect(res).toBe('')
    })

    it('should emit change event', async () => {
      let input = await window.createInputBox('', '', {})
      disposables.push(input)
      let curr: string
      input.onDidChange(text => {
        curr = text
      })
      await nvim.input('abc')
      await helper.waitValue((() => {
        return curr
      }), 'abc')
      input.title = 'foo'
      expect(input.title).toBe('foo')
      input.loading = true
      expect(input.loading).toBe(true)
      input.borderhighlight = 'WarningMsg'
      expect(input.borderhighlight).toBe('WarningMsg')
    })

    it('should not check bufnr for events', async () => {
      let input = await window.createInputBox('', undefined, {})
      disposables.push(input)
      let bufnr = input.bufnr
      let called = false
      input.onDidChange(() => {
        called = true
      })
      await events.fire('BufWinLeave', [bufnr + 1])
      await events.fire('PromptInsert', ['', bufnr + 1])
      await events.fire('TextChangedI', [bufnr + 1, {
        lnum: 1,
        col: 1,
        line: '',
        changedtick: 0,
        pre: ''
      }])
      expect(called).toBe(false)
      expect(input.bufnr).toBeDefined()
      expect(input.dimension).toBeDefined()
    })
  })

  describe('window showMessage', () => {
    async function ensureNotification(idx: number): Promise<void> {
      let ids = await nvim.call('coc#float#get_float_win_list')
      expect(ids.length).toBe(1)
      let win = nvim.createWindow(ids[0])
      let kind = await win.getVar('kind')
      expect(kind).toBe('notification')
      let bufnr = await nvim.call('winbufnr', [win.id])
      await events.fire('FloatBtnClick', [bufnr, idx])
    }
    it('should echo lines', async () => {
      await window.echoLines(['a', 'b'])
      let ch = await nvim.call('screenchar', [79, 1])
      let s = String.fromCharCode(ch)
      expect(s).toBe('a')
    })

    it('should echo multiple lines with truncate', async () => {
      await window.echoLines(['a', 'b', 'd', 'e'], true)
      let ch = await nvim.call('screenchar', [79, 1])
      let s = String.fromCharCode(ch)
      expect(s).toBe('a')
    })

    it('should show messages', async () => {
      window.showMessage('error', 'error')
      window.showMessage('warning', 'warning')
      window.showMessage('moremsg', 'more')
    })

    it('should show information message', async () => {
      let p = window.showInformationMessage('information message', 'first', 'second')
      await ensureNotification(0)
      let res = await p
      expect(res).toBe('first')
    })

    it('should show warning message', async () => {
      let p = window.showWarningMessage('warning message', 'first', 'second')
      await ensureNotification(1)
      let res = await p
      expect(res).toBe('second')
    })

    it('should show error message', async () => {
      let p = window.showErrorMessage('error message', 'first', 'second')
      await ensureNotification(0)
      let res = await p
      expect(res).toBe('first')
    })

    it('should prefer menu picker for notification message', async () => {
      helper.updateConfiguration('notification.preferMenuPicker', true)
      let p = window.showErrorMessage('error message', 'first', 'second')
      await helper.waitFloat()
      await nvim.input('1')
      let res = await p
      expect(res).toBe('first')
    })
  })

  describe('window parseSource()', () => {
    it('should parse sour name', async () => {
      expect(window.parseSource('\n\n')).toBeUndefined()
      expect(window.parseSource(`\n\n${path.join(process.cwd(), 'a/b.js')}:1:1`)).toBe('coc.nvim')
      expect(window.parseSource(`\n\n at Foo(${path.join(process.cwd(), 'a/b.js')}:1:1)`)).toBe('coc.nvim')
      let info = extensions.getExtensionsInfo()
      expect(window.parseSource(`\n\n${info[0].filepath}:1:1`)).toBe(info[0].name)
      let filepath = path.join(info[0].directory, 'a/b/c.js')
      expect(window.parseSource(`\n\n${filepath}:1:1`)).toBe(info[0].name)
    })
  })

  describe('window notifications', () => {
    it('should show notification with options', async () => {
      await window.showNotification({
        content: 'my notification',
        title: 'title',
      })
      let ids = await nvim.call('coc#float#get_float_win_list')
      expect(ids.length).toBe(1)
      let win = nvim.createWindow(ids[0])
      let kind = await win.getVar('kind')
      expect(kind).toBe('notification')
      let winid = await nvim.call('coc#float#get_related', [win.id, 'border'])
      let bufnr = await nvim.call('winbufnr', [winid])
      let buf = nvim.createBuffer(bufnr)
      let lines = await buf.lines
      expect(lines[0].includes('title')).toBe(true)
    })

    it('should ignore events of other buffers', async () => {
      let bufnr = workspace.bufnr
      let notification = new Notification(nvim, {})
      await events.fire('BufWinLeave', [bufnr + 1])
      await events.fire('FloatBtnClick', [bufnr + 1, 1])
      notification.dispose()
    })

    it('should throw on showNotification when no dialog support', async () => {
      Object.assign(workspace.env, { dialog: false })
      disposables.push(Disposable.create(() => {
        Object.assign(workspace.env, { dialog: true })
      }))
      let fn = async () => {
        await window.showNotification({
          content: 'my notification',
          title: 'title',
        })
      }
      await expect(fn()).rejects.toThrow(Error)
    })

    it('should show notification without border', async () => {
      helper.updateConfiguration('notification.border', false)
      await window.showNotification({
        content: 'my notification',
        title: 'title',
      })
      let win = await helper.getFloat()
      let height = await nvim.call('coc#float#get_height', [win.id])
      expect(height).toBe(2)
    })

    it('should show progress notification', async () => {
      let called = 0
      let res = await window.withProgress({ title: 'Downloading', cancellable: true }, (progress, token) => {
        let n = 0
        return new Promise(resolve => {
          let interval = setInterval(() => {
            progress.report({ message: 'progress', increment: 1 })
            n = n + 10
            called = called + 1
            if (n == 100) {
              clearInterval(interval)
              resolve('done')
            }
          }, 10)
          token.onCancellationRequested(() => {
            clearInterval(interval)
            resolve(undefined)
          })
        })
      })
      expect(called).toBeGreaterThan(8)
      expect(res).toBe('done')
    })

    it('should cancel progress notification on window close', async () => {
      let called = 0
      let p = window.withProgress({ title: 'Downloading', cancellable: true }, (progress, token) => {
        let n = 0
        return new Promise(resolve => {
          let interval = setInterval(() => {
            progress.report({ message: 'progress', increment: 1 })
            n = n + 10
            called = called + 1
            if (n == 100) {
              clearInterval(interval)
              resolve('done')
            }
          }, 10)
          token.onCancellationRequested(() => {
            clearInterval(interval)
            resolve(undefined)
          })
        })
      })
      await helper.wait(30)
      await nvim.call('coc#float#close_all', [])
      let res = await p
      expect(called).toBeLessThan(10)
      expect(res).toBe(undefined)
    })

    it('should cancel progress when resolved', async () => {
      let called = 0
      let p = window.withProgress({ title: 'Process' }, () => {
        called = called + 1
        return Promise.resolve()
      })
      await p
      let win = await helper.getFloat()
      if (win) {
        let res = await nvim.call('coc#window#get_var', [win.id, 'closing'])
        expect(res).toBe(1)
      }
      expect(called).toBe(1)
    })

    it('should be disabled by configuration', async () => {
      helper.updateConfiguration('notification.disabledProgressSources', ['test'])
      let p = window.withProgress({ title: 'Downloading', source: 'test' }, (progress, token) => {
        let n = 0
        return new Promise(resolve => {
          let interval = setInterval(() => {
            progress.report({ message: 'progress', increment: 1 })
            n = n + 1
            if (n == 10) {
              clearInterval(interval)
              resolve('done')
            }
          }, 10)
        })
      })
      await helper.wait(30)
      let win = await helper.getFloat()
      expect(win).toBeUndefined()
      let res = await p
      expect(res).toBe('done')
    })

    it('should show error message when rejected', async () => {
      let p = window.withProgress({ title: 'Process' }, () => {
        return Promise.reject(new Error('Unable to fetch'))
      })
      let res = await p
      expect(res).toBe(undefined)
      let cmdline = await helper.getCmdline()
      expect(cmdline).toMatch(/Unable to fetch/)
    })
  })

  describe('diffHighlights', () => {
    let ns = 'window-test'
    let priority = 99
    let ns_id: number
    beforeAll(async () => {
      ns_id = await nvim.call('coc#highlight#create_namespace', [ns])
    })

    async function createFile(): Promise<Buffer> {
      let content = 'foo\nbar'
      let file = await createTmpFile(content)
      return await helper.edit(file)
    }

    async function setHighlights(hls: HighlightItem[]): Promise<void> {
      let bufnr = await nvim.call('bufnr', ['%']) as number
      let arr = hls.map(o => [o.hlGroup, o.lnum, o.colStart, o.colEnd, o.combine === false ? 0 : 1, o.end_incl ? 1 : 0, o.start_incl ? 1 : 0])
      await nvim.call('coc#highlight#set', [bufnr, ns, arr, priority])
    }

    it('should add new highlights', async () => {
      let buf = await createFile()
      let items: HighlightItem[] = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 0,
        colEnd: 3
      }]
      let res = await window.diffHighlights(buf.id, ns, items)
      expect(res).toBeDefined()
      expect(res.add.length).toBe(1)
      await window.applyDiffHighlights(buf.id, ns, priority, res)
      let markers = await buf.getExtMarks(ns_id, 0, -1, { details: true })
      expect(markers.length).toBe(1)
      expect(markers[0][3].end_col).toBe(3)
    })

    it('should return empty diff', async () => {
      let buf = await createFile()
      let items: HighlightItem[] = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 0,
        colEnd: 3
      }]
      await setHighlights(items)
      let res = await window.diffHighlights(buf.id, ns, items)
      expect(res).toBeDefined()
      expect(res.remove).toEqual([])
      expect(res.add).toEqual([])
      expect(res.removeMarkers).toEqual([])
    })

    it('should remove and add highlights', async () => {
      let buf = await createFile()
      let items: HighlightItem[] = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 0,
        colEnd: 3
      }]
      await setHighlights(items)
      items = [{
        hlGroup: 'Search',
        lnum: 1,
        colStart: 0,
        colEnd: 3
      }]
      let res = await window.diffHighlights(buf.id, ns, items)
      expect(res).toBeDefined()
      expect(res.add.length).toBe(1)
      expect(res.removeMarkers.length).toBe(1)
      await window.applyDiffHighlights(buf.id, ns, priority, res)
      let markers = await buf.getExtMarks(ns_id, 0, -1, { details: true })
      expect(markers.length).toBe(1)
      expect(markers[0][1]).toBe(1)
      expect(markers[0][3].end_col).toBe(3)
    })

    it('should update highlights of single line', async () => {
      let buf = await createFile()
      let items: HighlightItem[] = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 0,
        colEnd: 1
      }, {
        hlGroup: 'Search',
        lnum: 1,
        colStart: 2,
        colEnd: 3
      }]
      await setHighlights(items)
      items = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 2,
        colEnd: 3
      }]
      let res = await window.diffHighlights(buf.id, ns, items)
      expect(res).toBeDefined()
      expect(res.add.length).toBe(1)
      expect(res.removeMarkers.length).toBe(2)
      await window.applyDiffHighlights(buf.id, ns, priority, res)
      let markers = await buf.getExtMarks(ns_id, 0, -1, { details: true })
      expect(markers.length).toBe(1)
      expect(markers[0][1]).toBe(0)
      expect(markers[0][3].end_col).toBe(3)
    })

    it('should not use extmarks on neovim < 0.5.1', async () => {
      let fn = workspace.has
      workspace.has = feature => {
        if (feature == 'nvim-0.5.1') return false
        return fn.apply(workspace, [feature])
      }
      disposables.push({
        dispose: () => {
          workspace.has = fn
        }
      })
      let buf = await createFile()
      let items: HighlightItem[] = [{
        hlGroup: 'Search',
        lnum: 0,
        colStart: 0,
        colEnd: 1
      }, {
        hlGroup: 'Search',
        lnum: 0,
        colStart: 2,
        colEnd: 3
      }]
      await setHighlights(items)
      let res = await window.diffHighlights(buf.id, ns, [])
      expect(res).toEqual({
        remove: [0], add: [], removeMarkers: []
      })
      await window.applyDiffHighlights(buf.id, ns, priority, res, true)
    })
  })

  describe('checkBuffer', () => {
    async function checkFloat(content: string) {
      let win = await helper.getFloat()
      expect(win).toBeDefined()
      let buf = await win.buffer
      let lines = await buf.lines
      expect(lines.join('\n')).toMatch(content)
    }

    it('should should error message for document not attached', async () => {
      await nvim.command('edit t|let b:coc_enabled = 0')
      await window.bufferCheck()
      await checkFloat('not attached')
      await nvim.call('coc#float#close_all', [])
      await nvim.command('edit +setl\\ buftype=nofile b')
      await window.bufferCheck()
      await checkFloat('not attached')
      await nvim.call('coc#float#close_all', [])
      await nvim.setVar('coc_max_filesize', 10240)
      let filepath = path.join(process.cwd(), 'data/schema.json')
      await helper.edit(filepath)
      await window.bufferCheck()
      await checkFloat('not attached')
      await nvim.call('coc#float#close_all', [])
    })

    it('should show state of current buffer', async () => {
      disposables.push(languages.registerDocumentFormatProvider(['*'], {
        provideDocumentFormattingEdits: () => {
          return []
        }
      }))
      await commands.executeCommand('document.checkBuffer')
      await checkFloat('Provider state')
      await nvim.call('coc#float#close_all', [])
    })
  })
})
