import { findUp, checkFolder, isGitIgnored, readFileLine, readFileLines, writeFile, fixDriver, renameAsync, isParentFolder, parentDirs, inDirectory, getFileLineCount, sameFile, resolveRoot, statAsync } from '../../util/fs'
import path from 'path'
import fs from 'fs'
import os from 'os'

describe('fs', () => {
  describe('stat()', () => {
    it('fs statAsync', async () => {
      let res = await statAsync(__filename)
      expect(res).toBeDefined
      expect(res.isFile()).toBe(true)
    })

    it('fs statAsync #1', async () => {
      let res = await statAsync(path.join(__dirname, 'file_not_exist'))
      expect(res).toBeNull
    })
  })

  describe('checkFolder()', () => {
    it('should check file in folder', async () => {
      let cwd = process.cwd()
      let res = await checkFolder(cwd, 'package.json')
      expect(res).toBe(true)
      res = await checkFolder(cwd, '**/schema.json')
      expect(res).toBe(true)
      res = await checkFolder(cwd, 'not_exists_fs')
      expect(res).toBe(false)
      res = await checkFolder(os.homedir(), 'not_exists_fs', 10)
      expect(res).toBe(false)
    })
  })

  describe('renameAsync()', () => {
    it('should rename file', async () => {
      let filepath = path.join(os.tmpdir(), 'foo')
      await writeFile(filepath, 'foo')
      let dest = path.join(os.tmpdir(), 'bar')
      await renameAsync(filepath, dest)
      let exists = fs.existsSync(dest)
      expect(exists).toBe(true)
      fs.unlinkSync(dest)
    })

    it('should throw when file does not exist', async () => {
      let err
      try {
        await renameAsync('/foo/bar', '/a')
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })
  })

  describe('getFileLineCount', () => {
    it('should throw when file does not exist', async () => {
      let err
      try {
        await getFileLineCount('/foo/bar')
      } catch (e) {
        err = e
      }
      expect(err).toBeDefined()
    })
  })

  describe('sameFile', () => {
    it('should be casesensitive', () => {
      expect(sameFile('/a', '/A', false)).toBe(false)
    })
  })

  describe('readFileLine', () => {
    it('should read line', async () => {
      let res = await readFileLine(__filename, 1)
      expect(res).toBeDefined()
    })

    it('should throw when file does not exist', async () => {
      const fn = async () => {
        await readFileLine(__filename + 'fooobar', 1)
      }
      await expect(fn()).rejects.toThrow(Error)
    })
  })

  describe('readFileLines', () => {
    it('should throw when file does not exist', async () => {
      const fn = async () => {
        await readFileLines(__filename + 'fooobar', 0, 3)
      }
      await expect(fn()).rejects.toThrow(Error)
    })

    it('should read lines', async () => {
      let res = await readFileLines(__filename, 0, 1)
      expect(res.length).toBe(2)
    })
  })

  describe('isGitIgnored()', () => {
    it('should be not ignored', async () => {
      let res = await isGitIgnored(__filename)
      expect(res).toBeFalsy
    })

    it('should be ignored', async () => {
      let res = await isGitIgnored('')
      expect(res).toBe(false)
      res = await isGitIgnored(path.join(os.tmpdir(), 'foo'))
      expect(res).toBe(false)
      res = await isGitIgnored(path.resolve(__dirname, '../lib/index.js.map'))
      expect(res).toBe(false)
      res = await isGitIgnored(__filename)
      expect(res).toBe(false)
      let filepath = path.join(os.tmpdir(), 'foo')
      fs.writeFileSync(filepath, '', { encoding: 'utf8' })
      res = await isGitIgnored(filepath)
      expect(res).toBe(false)
      fs.unlinkSync(filepath)
    })
  })

  describe('inDirectory', () => {
    it('should support wildcard', async () => {
      let res = inDirectory(__dirname, ['**/file_not_exist.json'])
      expect(res).toBe(false)
    })
  })

  describe('parentDirs', () => {
    it('get parentDirs', () => {
      let dirs = parentDirs('/a/b/c')
      expect(dirs).toEqual(['/', '/a', '/a/b'])
    })
  })

  describe('isParentFolder', () => {
    it('check parent folder', () => {
      expect(isParentFolder('/a', '/a/b')).toBe(true)
      expect(isParentFolder('/a/b', '/a/b/')).toBe(false)
      expect(isParentFolder('/a/b', '/a/b')).toBe(false)
      expect(isParentFolder('/a/b', '/a/b', true)).toBe(true)
      expect(isParentFolder('//', '/', true)).toBe(true)
      expect(isParentFolder('/a/b/', '/a/b/c', true)).toBe(true)
    })
  })

  describe('fixDriver', () => {
    it('should fix driver', async () => {
      expect(fixDriver('c:/foo', 'win32')).toBe('C:/foo')
    })
  })

  describe('resolveRoot', () => {
    it('resolve root consider root path', () => {
      let res = resolveRoot(__dirname, ['.git'])
      expect(res).toMatch('coc.nvim')
    })

    it('should ignore glob pattern', () => {
      let res = resolveRoot(__dirname, [path.basename(__filename)], undefined, false, false, ["**/__tests__/**"])
      expect(res).toBeFalsy()
    })

    it('should ignore glob pattern bottom up', () => {
      let res = resolveRoot(__dirname, [path.basename(__filename)], undefined, true, false, ["**/__tests__/**"])
      expect(res).toBeFalsy()
    })

    it('should resolve from parent folders', () => {
      let root = path.resolve(__dirname, '../extensions/snippet-sample')
      let res = resolveRoot(root, ['package.json'])
      expect(res.endsWith('coc.nvim')).toBe(true)
    })

    it('should resolve from parent folders with bottom-up method', () => {
      let root = path.resolve(__dirname, '../extensions/snippet-sample')
      let res = resolveRoot(root, ['package.json'], null, true)
      expect(res.endsWith('extensions')).toBe(true)
    })

    it('should resolve to cwd', () => {
      let root = path.resolve(__dirname, '../extensions/test/')
      let res = resolveRoot(root, ['package.json'], root, false, true)
      expect(res).toBe(root)
    })

    it('should resolve to root', () => {
      let root = path.resolve(__dirname, '../extensions/test/')
      let res = resolveRoot(root, ['package.json'], root, false, false)
      expect(res).toBe(path.resolve(__dirname, '../../../'))
    })

    it('should not resolve to home', () => {
      let res = resolveRoot(__dirname, ['.config'], undefined, false, false, [os.homedir()])
      expect(res != os.homedir()).toBeTruthy()
    })
  })

  describe('findUp', () => {
    it('findUp by filename', () => {
      let filepath = findUp('package.json', __dirname)
      expect(filepath).toMatch('coc.nvim')
      filepath = findUp('not_exists', __dirname)
      expect(filepath).toBeNull()
    })

    it('findUp by filenames', async () => {
      let filepath = findUp(['src'], __dirname)
      expect(filepath).toMatch('coc.nvim')
    })
  })

})
