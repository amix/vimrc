'use strict'
import { exec } from 'child_process'
import fs from 'fs-extra'
import minimatch from 'minimatch'
import os from 'os'
import path from 'path'
import readline from 'readline'
import util from 'util'
import glob from 'glob'
import * as platform from './platform'
const logger = require('./logger')('util-fs')

export type OnReadLine = (line: string) => void

export async function statAsync(filepath: string): Promise<fs.Stats | null> {
  let stat = null
  try {
    stat = await fs.stat(filepath)
  } catch (e) {}
  return stat
}

export function renameAsync(oldPath: string, newPath: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs.rename(oldPath, newPath, err => {
      if (err) return reject(err)
      resolve()
    })
  })
}

export async function isGitIgnored(fullpath: string): Promise<boolean> {
  if (!fullpath) return false
  let stat = await statAsync(fullpath)
  if (!stat || !stat.isFile()) return false
  let root = null
  try {
    let { stdout } = await util.promisify(exec)('git rev-parse --show-toplevel', { cwd: path.dirname(fullpath) })
    root = stdout.trim()
  } catch (e) {}
  if (!root) return false
  let file = path.relative(root, fullpath)
  try {
    let { stdout } = await util.promisify(exec)(`git check-ignore ${file}`, { cwd: root })
    return stdout.trim() == file
  } catch (e) {}
  return false
}

export function isFolderIgnored(folder: string, ignored: string[] = []): boolean {
  if (!ignored || !ignored.length) return false
  return ignored.some(p => minimatch(folder, p, { dot: true }))
}

export function resolveRoot(folder: string, subs: string[], cwd?: string, bottomup = false, checkCwd = true, ignored: string[] = []): string | null {
  let dir = fixDriver(folder)
  if (checkCwd
    && cwd
    && isParentFolder(cwd, dir, true)
    && !isFolderIgnored(cwd, ignored)
    && inDirectory(cwd, subs)) return cwd
  let parts = dir.split(path.sep)
  if (bottomup) {
    while (parts.length > 0) {
      let dir = parts.join(path.sep)
      if (!isFolderIgnored(dir, ignored) && inDirectory(dir, subs)) {
        return dir
      }
      parts.pop()
    }
    return null
  } else {
    let curr: string[] = [parts.shift()]
    for (let part of parts) {
      curr.push(part)
      let dir = curr.join(path.sep)
      if (!isFolderIgnored(dir, ignored) && inDirectory(dir, subs)) {
        return dir
      }
    }
    return null
  }
}

export async function checkFolder(dir: string, pattern: string, timeout = 500): Promise<boolean> {
  return new Promise((resolve, reject) => {
    let timer = setTimeout(() => {
      gl.abort()
      resolve(false)
    }, timeout)
    let find = false
    let gl = glob(pattern, {
      nosort: true,
      dot: true,
      cwd: dir,
      nodir: true,
      absolute: false
    }, err => {
      clearTimeout(timer)
      if (err) return reject(err)
      resolve(find)
    })
    gl.on('match', () => {
      clearTimeout(timer)
      find = true
      gl.abort()
      resolve(true)
    })
    gl.on('end', () => {
      clearTimeout(timer)
      resolve(find)
    })
  })
}

export function inDirectory(dir: string, subs: string[]): boolean {
  try {
    let files = fs.readdirSync(dir)
    for (let pattern of subs) {
      // note, only '*' expanded
      let is_wildcard = (pattern.includes('*'))
      let res = is_wildcard ?
        (minimatch.match(files, pattern, { nobrace: true, noext: true, nocomment: true, nonegate: true, dot: true }).length !== 0) :
        (files.includes(pattern))
      if (res) return true
    }
  } catch (e) {
    // could be failed without permission
  }
  return false
}

export function findUp(name: string | string[], cwd: string): string {
  let root = path.parse(cwd).root
  let subs = Array.isArray(name) ? name : [name]
  while (cwd && cwd !== root) {
    let find = inDirectory(cwd, subs)
    if (find) {
      for (let sub of subs) {
        let filepath = path.join(cwd, sub)
        if (fs.existsSync(filepath)) {
          return filepath
        }
      }
    }
    cwd = path.dirname(cwd)
  }
  return null
}

export function readFile(fullpath: string, encoding: string): Promise<string> {
  return new Promise((resolve, reject) => {
    fs.readFile(fullpath, encoding, (err, content) => {
      if (err) reject(err)
      resolve(content)
    })
  })
}

export function getFileLineCount(filepath: string): Promise<number> {
  let i
  let count = 0
  return new Promise((resolve, reject) => {
    fs.createReadStream(filepath)
      .on('error', e => reject(e))
      .on('data', chunk => {
        for (i = 0; i < chunk.length; ++i) if (chunk[i] == 10) count++
      })
      .on('end', () => resolve(count))
  })
}

export function readFileLines(fullpath: string, start: number, end: number): Promise<string[]> {
  if (!fs.existsSync(fullpath)) {
    return Promise.reject(new Error(`file does not exist: ${fullpath}`))
  }
  let res: string[] = []
  const input = fs.createReadStream(fullpath, { encoding: 'utf8' })
  const rl = readline.createInterface({
    input,
    crlfDelay: Infinity,
    terminal: false
  } as any)
  let n = 0
  return new Promise((resolve, reject) => {
    rl.on('line', line => {
      if (n >= start && n <= end) {
        res.push(line)
      }
      if (n == end) {
        rl.close()
      }
      n = n + 1
    })
    rl.on('close', () => {
      resolve(res)
      input.close()
    })
    rl.on('error', reject)
  })
}

export function readFileLine(fullpath: string, count: number): Promise<string> {
  if (!fs.existsSync(fullpath)) {
    return Promise.reject(new Error(`file does not exist: ${fullpath}`))
  }
  const input = fs.createReadStream(fullpath, { encoding: 'utf8' })
  const rl = readline.createInterface({
    input,
    crlfDelay: Infinity,
    terminal: false
  } as any)
  let n = 0
  return new Promise((resolve, reject) => {
    rl.on('line', line => {
      if (n == count) {
        if (n == 0 && line.startsWith('\uFEFF')) {
          // handle BOM
          line = line.slice(1)
        }
        rl.close()
        input.close()
        resolve(line)
        return
      }
      n = n + 1
    })
    rl.on('error', reject)
  })
}

export function sameFile(fullpath: string | null, other: string | null, caseInsensitive?: boolean): boolean {
  caseInsensitive = typeof caseInsensitive == 'boolean' ? caseInsensitive : platform.isWindows || platform.isMacintosh
  if (!fullpath || !other) return false
  if (caseInsensitive) return fullpath.toLowerCase() === other.toLowerCase()
  return fullpath === other
}

export function fileStartsWith(dir: string, pdir: string) {
  let caseInsensitive = platform.isWindows || platform.isMacintosh
  if (caseInsensitive) return dir.toLowerCase().startsWith(pdir.toLowerCase())
  return dir.startsWith(pdir)
}

export async function writeFile(fullpath: string, content: string): Promise<void> {
  await fs.writeFile(fullpath, content, { encoding: 'utf8' })
}

export function isFile(uri: string): boolean {
  return uri.startsWith('file:')
}

export function parentDirs(pth: string): string[] {
  let { root, dir } = path.parse(pth)
  if (dir === root) return [root]
  const dirs = [root]
  const parts = dir.slice(root.length).split(path.sep)
  for (let i = 1; i <= parts.length; i++) {
    dirs.push(path.join(root, parts.slice(0, i).join(path.sep)))
  }
  return dirs
}

export function isParentFolder(folder: string, filepath: string, checkEqual = false): boolean {
  let pdir = fixDriver(path.resolve(path.normalize(folder)))
  let dir = fixDriver(path.resolve(path.normalize(filepath)))
  if (pdir == '//') pdir = '/'
  if (sameFile(pdir, dir)) return checkEqual ? true : false
  if (pdir.endsWith(path.sep)) return fileStartsWith(dir, pdir)
  return fileStartsWith(dir, pdir) && dir[pdir.length] == path.sep
}

// use uppercase for windows driver
export function fixDriver(filepath: string, platform = os.platform()): string {
  if (platform != 'win32' || filepath[1] != ':') return filepath
  return filepath[0].toUpperCase() + filepath.slice(1)
}
