'use strict'
import { exec, ExecOptions } from 'child_process'
import debounce from 'debounce'
import fs from 'fs'
import isuri from 'isuri'
import path from 'path'
import { Disposable, MarkupContent, MarkupKind } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import which from 'which'
import * as platform from './platform'
export { platform }
const logger = require('./logger')('util-index')

export type MapMode = 'n' | 'i' | 'v' | 'x' | 's' | 'o'

export const CONFIG_FILE_NAME = 'coc-settings.json'

export function isMarkdown(content: MarkupContent | string | undefined): boolean {
  if (MarkupContent.is(content) && content.kind == MarkupKind.Markdown) {
    return true
  }
  return false
}

export function wait(ms: number): Promise<void> {
  if (ms <= 0) return Promise.resolve(undefined)
  return new Promise(resolve => {
    setTimeout(() => {
      resolve(undefined)
    }, ms)
  })
}

export function waitNextTick(fn?: () => void): Promise<void> {
  return new Promise(resolve => {
    process.nextTick(() => {
      if (fn) fn()
      resolve(undefined)
    })
  })
}

export function waitImmediate(): Promise<void> {
  return new Promise(resolve => {
    setImmediate(() => {
      resolve(undefined)
    })
  })
}

export function getUri(fullpath: string, id: number, buftype: string, isCygwin: boolean): string {
  if (!fullpath) return `untitled:${id}`
  // https://github.com/neoclide/coc-java/issues/82
  if (platform.isWindows && !isCygwin && !fullpath.startsWith('jdt://')) fullpath = path.win32.normalize(fullpath)
  if (path.isAbsolute(fullpath)) return URI.file(fullpath).toString()
  if (isuri.isValid(fullpath)) return URI.parse(fullpath).toString()
  if (buftype != '') return `${buftype}:${id}`
  return `unknown:${id}`
}

export function disposeAll(disposables: Disposable[]): void {
  while (disposables.length) {
    const item = disposables.pop()
    if (item) {
      item.dispose()
    }
  }
}

export function executable(command: string): boolean {
  try {
    which.sync(command)
  } catch (e) {
    return false
  }
  return true
}

export function runCommand(cmd: string, opts: ExecOptions = {}, timeout?: number): Promise<string> {
  if (!platform.isWindows) {
    opts.shell = opts.shell || process.env.SHELL
  }
  opts.maxBuffer = 500 * 1024
  return new Promise<string>((resolve, reject) => {
    let timer: NodeJS.Timer
    if (timeout) {
      timer = setTimeout(() => {
        reject(new Error(`timeout after ${timeout}s`))
      }, timeout * 1000)
    }
    exec(cmd, opts, (err, stdout, stderr) => {
      if (timer) clearTimeout(timer)
      if (err) {
        reject(new Error(`exited with ${err.code}\n${err}\n${stderr}`))
        return
      }
      resolve(stdout)
    })
  })
}

export function watchFile(filepath: string, onChange: () => void): Disposable {
  let callback = debounce(onChange, 100)
  try {
    let watcher = fs.watch(filepath, {
      persistent: true,
      recursive: false,
      encoding: 'utf8'
    }, () => {
      callback()
    })
    return Disposable.create(() => {
      callback.clear()
      watcher.close()
    })
  } catch (e) {
    return Disposable.create(() => {
      callback.clear()
    })
  }
}

export function isRunning(pid: number): boolean {
  try {
    let res: any = process.kill(pid, 0)
    return res == true
  }
  catch (e) {
    return e['code'] === 'EPERM'
  }
}

export function getKeymapModifier(mode: MapMode): string {
  if (mode == 'n' || mode == 'o' || mode == 'x' || mode == 'v') return '<C-U>'
  if (mode == 'i') return '<C-o>'
  if (mode == 's') return '<Esc>'
  return ''
}

export function concurrent<T>(arr: T[], fn: (val: T) => Promise<void>, limit = 3): Promise<void> {
  if (arr.length == 0) return Promise.resolve()
  let finished = 0
  let total = arr.length
  let remain = arr.slice()
  return new Promise(resolve => {
    let run = (val): void => {
      let cb = () => {
        finished = finished + 1
        if (finished == total) {
          resolve()
        } else if (remain.length) {
          let next = remain.shift()
          run(next)
        }
      }
      fn(val).then(cb, cb)
    }
    for (let i = 0; i < Math.min(limit, remain.length); i++) {
      let val = remain.shift()
      run(val)
    }
  })
}
