'use strict'
import { Neovim } from '@chemzqm/neovim'
import { ChildProcess, spawn } from 'child_process'
import { EventEmitter } from 'events'
import path from 'path'
import readline from 'readline'
import { Range } from 'vscode-languageserver-types'
import Highlighter from '../../model/highligher'
import { ansiparse } from '../../util/ansiparse'
import { Mutex } from '../../util/mutex'
import window from '../../window'
import RefactorBuffer, { FileItem, FileItemDef } from './buffer'
const logger = require('../../util/logger')('handler-search')

const defaultArgs = ['--color', 'ansi', '--colors', 'path:fg:black', '--colors', 'line:fg:green', '--colors', 'match:fg:red', '--no-messages', '--heading', '-n']
const controlCode = '\x1b'

// emit FileItem
class Task extends EventEmitter {
  private process: ChildProcess
  public start(cmd: string, args: string[], cwd: string): void {
    this.process = spawn(cmd, args, { cwd })
    this.process.on('error', e => {
      this.emit('error', e.message)
    })
    const rl = readline.createInterface(this.process.stdout)
    let start: number
    let fileItem: FileItemDef
    let lines: string[] = []
    let highlights: Range[] = []
    let create = true
    rl.on('line', content => {
      if (content.includes(controlCode)) {
        let items = ansiparse(content)
        if (items[0].foreground == 'black') {
          fileItem = { filepath: path.join(cwd, items[0].text), ranges: [] }
          return
        }
        let normalLine = items[0].foreground == 'green'
        if (normalLine) {
          let lnum = parseInt(items[0].text, 10) - 1
          let padlen = items[0].text.length + 1
          if (create) {
            start = lnum
            create = false
          }
          let line = ''
          for (let item of items) {
            if (item.foreground == 'red') {
              let l = lnum - start
              let c = line.length - padlen
              highlights.push(Range.create(l, c, l, c + item.text.length))
            }
            line += item.text
          }
          let currline = line.slice(padlen)
          lines.push(currline)
        }
      } else {
        let fileEnd = content.trim().length == 0
        if (fileItem && (fileEnd || content.trim() == '--')) {
          fileItem.ranges.push({ lines, highlights, start })
        }
        if (fileEnd) {
          this.emit('item', fileItem)
          fileItem = null
        }
        lines = []
        highlights = []
        create = true
      }
    })
    rl.on('close', () => {
      if (fileItem) {
        if (lines.length) {
          fileItem.ranges.push({ lines, highlights, start, })
        }
        this.emit('item', fileItem)
      }
      lines = highlights = fileItem = null
      this.emit('end')
    })
  }

  public dispose(): void {
    if (this.process) {
      this.process.kill()
    }
  }
}

export default class Search {
  private task: Task
  constructor(private nvim: Neovim, private cmd = 'rg') {
  }

  public run(args: string[], cwd: string, refactorBuf: RefactorBuffer): Promise<void> {
    let { nvim, cmd } = this
    let { afterContext, beforeContext } = refactorBuf.config
    let argList = ['-A', afterContext.toString(), '-B', beforeContext.toString()].concat(defaultArgs, args)
    let p = getPathFromArgs(args)
    if (p) argList.pop()
    argList.push('--', p ? path.isAbsolute(p) ? p : `./${p.replace(/^\.\//, '')}` : './')
    this.task = new Task()
    this.task.start(cmd, argList, cwd)
    let mutex: Mutex = new Mutex()
    let files = 0
    let matches = 0
    let start = Date.now()
    // remaining items
    let fileItems: FileItem[] = []
    const addFileItems = async () => {
      if (fileItems.length == 0) return
      let items = fileItems.slice()
      fileItems = []
      const release = await mutex.acquire()
      try {
        await refactorBuf.addFileItems(items)
      } catch (e) {
        logger.error(e)
      }
      release()
    }
    return new Promise((resolve, reject) => {
      let interval = setInterval(addFileItems, 300)
      this.task.on('item', async (fileItem: FileItem) => {
        files++
        matches = matches + fileItem.ranges.reduce((p, r) => p + r.highlights.length, 0)
        fileItems.push(fileItem)
      })
      this.task.on('error', message => {
        clearInterval(interval)
        window.showMessage(`Error on command "${cmd}": ${message}`, 'error')
        this.task = null
        reject(new Error(message))
      })
      this.task.on('end', async () => {
        clearInterval(interval)
        try {
          await addFileItems()
          const release = await mutex.acquire()
          release()
          this.task.removeAllListeners()
          this.task = null
          let buf = refactorBuf.buffer
          if (buf) {
            nvim.pauseNotification()
            if (files == 0) {
              buf.setLines(['No match found'], { start: 1, end: 2, strictIndexing: false }, true)
              // eslint-disable-next-line @typescript-eslint/no-floating-promises
              buf.addHighlight({ line: 1, srcId: -1, colEnd: -1, colStart: 0, hlGroup: 'Error' })
              buf.setOption('modified', false, true)
            } else {
              let highligher = new Highlighter()
              highligher.addText('Files', 'MoreMsg')
              highligher.addText(': ')
              highligher.addText(`${files} `, 'Number')
              highligher.addText('Matches', 'MoreMsg')
              highligher.addText(': ')
              highligher.addText(`${matches} `, 'Number')
              highligher.addText('Duration', 'MoreMsg')
              highligher.addText(': ')
              highligher.addText(`${Date.now() - start}ms`, 'Number')
              highligher.render(buf, 1, 2)
            }
            buf.setOption('modified', false, true)
            nvim.resumeNotification(false, true)
          }
        } catch (e) {
          reject(e)
          return
        }
        resolve()
      })
    })
  }

  public abort(): void {
    this.task?.dispose()
  }
}

// rg requires `-- [path]` at the end
export function getPathFromArgs(args: string[]): string | undefined {
  if (args.length < 2) return undefined
  let len = args.length
  if (args[len - 1].startsWith('-')) return undefined
  if (args[len - 2].startsWith('-')) return undefined
  return args[len - 1]
}
