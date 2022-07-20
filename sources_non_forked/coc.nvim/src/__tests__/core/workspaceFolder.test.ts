import { Neovim } from '@chemzqm/neovim'
import fs from 'fs'
import os from 'os'
import path from 'path'
import { Disposable, WorkspaceFoldersChangeEvent } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import Configurations from '../../configuration/index'
import WorkspaceFolderController from '../../core/workspaceFolder'
import { PatternType } from '../../types'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper from '../helper'

let workspaceFolder: WorkspaceFolderController
let configurations: Configurations
let disposables: Disposable[] = []
let nvim: Neovim

function updateConfiguration(key: string, value: any, defaults: any): void {
  configurations.updateUserConfig({ [key]: value })
  disposables.push({
    dispose: () => {
      configurations.updateUserConfig({ [key]: defaults })
    }
  })
}

beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
  let userConfigFile = path.join(process.env.COC_VIMCONFIG, 'coc-settings.json')
  configurations = new Configurations(userConfigFile, {
    $removeConfigurationOption: () => {},
    $updateConfigurationOption: () => {}
  })
  workspaceFolder = new WorkspaceFolderController(configurations)
})

afterEach(async () => {
  await helper.reset()
  disposeAll(disposables)
  workspaceFolder.reset()
})

afterAll(async () => {
  await helper.shutdown()
})

describe('WorkspaceFolderController', () => {
  describe('asRelativePath()', () => {
    function assertAsRelativePath(input: string, expected: string, includeWorkspace?: boolean) {
      const actual = workspaceFolder.getRelativePath(input, includeWorkspace)
      expect(actual).toBe(expected)
    }

    it('should get relative path', async () => {
      workspaceFolder.addWorkspaceFolder(`/Coding/Applications/NewsWoWBot`, false)
      assertAsRelativePath('/Coding/Applications/NewsWoWBot/bernd/das/brot', 'bernd/das/brot')
      assertAsRelativePath('/Apps/DartPubCache/hosted/pub.dartlang.org/convert-2.0.1/lib/src/hex.dart',
        '/Apps/DartPubCache/hosted/pub.dartlang.org/convert-2.0.1/lib/src/hex.dart')
      assertAsRelativePath('', '')
      assertAsRelativePath('/foo/bar', '/foo/bar')
      assertAsRelativePath('in/out', 'in/out')
    })

    it('should asRelativePath, same paths, #11402', async () => {
      const root = '/home/aeschli/workspaces/samples/docker'
      const input = '/home/aeschli/workspaces/samples/docker'
      workspaceFolder.addWorkspaceFolder(root, false)
      assertAsRelativePath(input, input)
      const input2 = '/home/aeschli/workspaces/samples/docker/a.file'
      assertAsRelativePath(input2, 'a.file')
    })

    it('should asRelativePath, not workspaceFolder', async () => {
      expect(workspace.getRelativePath('')).toBe('')
      assertAsRelativePath('/foo/bar', '/foo/bar')
    })

    it('should asRelativePath, multiple folders', () => {
      workspaceFolder.addWorkspaceFolder(`/Coding/One`, false)
      workspaceFolder.addWorkspaceFolder(`/Coding/Two`, false)
      assertAsRelativePath('/Coding/One/file.txt', 'One/file.txt')
      assertAsRelativePath('/Coding/Two/files/out.txt', 'Two/files/out.txt')
      assertAsRelativePath('/Coding/Two2/files/out.txt', '/Coding/Two2/files/out.txt')
    })

    it('should slightly inconsistent behaviour of asRelativePath and getWorkspaceFolder, #31553', async () => {
      workspaceFolder.addWorkspaceFolder(`/Coding/One`, false)
      workspaceFolder.addWorkspaceFolder(`/Coding/Two`, false)

      assertAsRelativePath('/Coding/One/file.txt', 'One/file.txt')
      assertAsRelativePath('/Coding/One/file.txt', 'One/file.txt', true)
      assertAsRelativePath('/Coding/One/file.txt', 'file.txt', false)
      assertAsRelativePath('/Coding/Two/files/out.txt', 'Two/files/out.txt')
      assertAsRelativePath('/Coding/Two/files/out.txt', 'Two/files/out.txt', true)
      assertAsRelativePath('/Coding/Two/files/out.txt', 'files/out.txt', false)
      assertAsRelativePath('/Coding/Two2/files/out.txt', '/Coding/Two2/files/out.txt')
      assertAsRelativePath('/Coding/Two2/files/out.txt', '/Coding/Two2/files/out.txt', true)
      assertAsRelativePath('/Coding/Two2/files/out.txt', '/Coding/Two2/files/out.txt', false)
    })
  })

  describe('setWorkspaceFolders()', () => {
    it('should set valid folders', async () => {
      workspaceFolder.setWorkspaceFolders([os.tmpdir(), '/a/not_exists'])
      let folders = workspaceFolder.workspaceFolders
      expect(folders.length).toBe(2)
    })
  })

  describe('getWorkspaceFolder()', () => {
    it('should get workspaceFolder by uri', async () => {
      let res = workspaceFolder.getWorkspaceFolder(URI.parse('untitled://1'))
      expect(res).toBeUndefined()
      res = workspaceFolder.getWorkspaceFolder(URI.file('/a/b'))
      expect(res).toBeUndefined()
      let filepath = path.join(process.cwd(), 'a/b')
      workspaceFolder.setWorkspaceFolders([process.cwd()])
      res = workspaceFolder.getWorkspaceFolder(URI.file(filepath))
      expect(URI.parse(res.uri).fsPath).toBe(process.cwd())
    })
  })

  describe('getRootPatterns()', () => {
    it('should get patterns from b:coc_root_patterns', async () => {
      await nvim.command('edit t.vim | let b:coc_root_patterns=["foo"]')
      await nvim.command('setf vim')
      let doc = await workspace.document
      let res = workspaceFolder.getRootPatterns(doc, PatternType.Buffer)
      expect(res).toEqual(['foo'])
    })

    it('should get patterns from languageserver', async () => {
      updateConfiguration('languageserver', {
        test: {
          filetypes: ['vim'],
          rootPatterns: ['bar']
        }
      }, {})
      workspaceFolder.addRootPattern('vim', ['foo'])
      await nvim.command('edit t.vim')
      await nvim.command('setf vim')
      let doc = await workspace.document
      let res = workspaceFolder.getRootPatterns(doc, PatternType.LanguageServer)
      expect(res).toEqual(['bar', 'foo'])
    })

    it('should get patterns from user configuration', async () => {
      let doc = await workspace.document
      let res = workspaceFolder.getRootPatterns(doc, PatternType.Global)
      expect(res.includes('.git')).toBe(true)
    })
  })

  describe('resolveRoot()', () => {
    const cwd = process.cwd()
    const expand = (input: string) => {
      return workspace.expand(input)
    }

    it('should resolve to cwd for file in cwd', async () => {
      updateConfiguration('coc.preferences.rootPatterns', [], ['.git', '.hg', '.projections.json'])
      let file = path.join(os.tmpdir(), 'foo')
      await nvim.command(`edit ${file}`)
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, os.tmpdir(), false, expand)
      expect(res).toBe(os.tmpdir())
    })

    it('should not fallback to cwd as workspace folder', async () => {
      updateConfiguration('coc.preferences.rootPatterns', [], ['.git', '.hg', '.projections.json'])
      updateConfiguration('workspace.workspaceFolderFallbackCwd', false, true)
      let file = path.join(os.tmpdir(), 'foo')
      await nvim.command(`edit ${file}`)
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, os.tmpdir(), false, expand)
      expect(res).toBe(null)
    })

    it('should return null for untitled buffer', async () => {
      await nvim.command('enew')
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, cwd, false, expand)
      expect(res).toBe(null)
    })

    it('should respect ignored filetypes', async () => {
      updateConfiguration('workspace.ignoredFiletypes', ['vim'], [])
      await nvim.command('edit t.vim')
      await nvim.command('setf vim')
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, cwd, false, expand)
      expect(res).toBe(null)
    })

    it('should respect workspaceFolderCheckCwd', async () => {
      let called = 0
      disposables.push(workspaceFolder.onDidChangeWorkspaceFolders(() => {
        called++
      }))
      workspaceFolder.addRootPattern('vim', ['.vim'])
      await nvim.command('edit a/.vim/t.vim')
      await nvim.command('setf vim')
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, cwd, true, expand)
      expect(res).toBe(process.cwd())
      await nvim.command('edit a/foo')
      doc = await workspace.document
      res = workspaceFolder.resolveRoot(doc, cwd, true, expand)
      expect(res).toBe(process.cwd())
      expect(called).toBe(1)
    })

    it('should respect ignored folders', async () => {
      updateConfiguration('workspace.ignoredFolders', ['$HOME/foo', '$HOME'], [])
      let file = path.join(os.homedir(), '.vim/bar')
      workspaceFolder.addRootPattern('vim', ['.vim'])
      await nvim.command(`edit ${file}`)
      await nvim.command('setf vim')
      let doc = await workspace.document
      let res = workspaceFolder.resolveRoot(doc, path.join(os.homedir(), 'foo'), true, expand)
      expect(res).toBe(null)
    })

    describe('bottomUpFileTypes', () => {
      it('should respect specific filetype', async () => {
        updateConfiguration('coc.preferences.rootPatterns', ['.vim'], ['.git', '.hg', '.projections.json'])
        updateConfiguration('workspace.bottomUpFiletypes', ['vim'], [])
        let root = path.join(os.tmpdir(), 'a')
        let dir = path.join(root, '.vim')
        if (!fs.existsSync(dir)) {
          fs.mkdirSync(dir, { recursive: true })
        }
        let file = path.join(dir, 'foo')
        await nvim.command(`edit ${file}`)
        await nvim.command('setf vim')
        let doc = await workspace.document
        let res = workspaceFolder.resolveRoot(doc, file, true, expand)
        expect(res).toBe(root)
      })

      it('should respect wildcard', async () => {
        updateConfiguration('coc.preferences.rootPatterns', ['.vim'], ['.git', '.hg', '.projections.json'])
        updateConfiguration('workspace.bottomUpFiletypes', ['*'], [])
        let root = path.join(os.tmpdir(), 'a')
        let dir = path.join(root, '.vim')
        if (!fs.existsSync(dir)) {
          fs.mkdirSync(dir, { recursive: true })
          await helper.wait(30)
        }
        let file = path.join(dir, 'foo')
        await nvim.command(`edit ${file}`)
        let doc = await workspace.document
        let res = workspaceFolder.resolveRoot(doc, file, true, expand)
        expect(res).toBe(root)
      })
    })
  })

  describe('renameWorkspaceFolder()', () => {
    it('should rename workspaceFolder', async () => {
      let e: WorkspaceFoldersChangeEvent
      disposables.push(workspaceFolder.onDidChangeWorkspaceFolders(ev => {
        e = ev
      }))
      let cwd = process.cwd()
      workspaceFolder.addWorkspaceFolder(cwd, false)
      workspaceFolder.addWorkspaceFolder(cwd, false)
      workspaceFolder.renameWorkspaceFolder(cwd, path.join(cwd, '.vim'))
      expect(e.removed.length).toBe(1)
      expect(e.added.length).toBe(1)
    })
  })

  describe('removeWorkspaceFolder()', () => {
    it('should remote workspaceFolder', async () => {
      let e: WorkspaceFoldersChangeEvent
      disposables.push(workspaceFolder.onDidChangeWorkspaceFolders(ev => {
        e = ev
      }))
      let cwd = process.cwd()
      workspaceFolder.addWorkspaceFolder(cwd, false)
      workspaceFolder.removeWorkspaceFolder(cwd)
      workspaceFolder.removeWorkspaceFolder('/a/b')
      expect(e.removed.length).toBe(1)
      expect(e.added.length).toBe(0)
    })
  })
})
