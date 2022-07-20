'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import { EventEmitter } from 'events'
import { Disposable } from 'vscode-languageserver-protocol'
import { OutputChannel } from '../types'
import { frames } from './status'
const logger = require('../util/logger')('model-installBuffer')

export enum State {
  Waiting,
  Failed,
  Progressing,
  Success,
}

export default class InstallBuffer extends EventEmitter implements Disposable {
  private statMap: Map<string, State> = new Map()
  private messagesMap: Map<string, string[]> = new Map()
  private names: string[] = []
  // eslint-disable-next-line no-undef
  private interval: NodeJS.Timer
  public bufnr: number

  constructor(
    private isUpdate = false,
    private isSync = false,
    private channel: OutputChannel | undefined = undefined) {
    super()
  }

  public setExtensions(names: string[]): void {
    this.statMap.clear()
    this.names = names
    for (let name of names) {
      this.statMap.set(name, State.Waiting)
    }
  }

  public addMessage(name: string, msg: string, isProgress = false): void {
    if (isProgress && this.channel) return
    let lines = this.messagesMap.get(name) || []
    this.messagesMap.set(name, lines.concat(msg.trim().split(/\r?\n/)))
    if (this.channel) this.channel.appendLine(`[${name}] ${msg}`)
  }

  public startProgress(names: string[]): void {
    for (let name of names) {
      this.statMap.set(name, State.Progressing)
    }
  }

  public finishProgress(name: string, succeed = true): void {
    if (this.channel) {
      if (succeed) {
        this.channel.appendLine(`[${name}] install succeed!`)
      } else {
        this.channel.appendLine(`[${name}] install failed!`)
      }
    }
    this.statMap.set(name, succeed ? State.Success : State.Failed)
  }

  public get remains(): number {
    let count = 0
    for (let name of this.names) {
      let stat = this.statMap.get(name)
      if (![State.Success, State.Failed].includes(stat)) {
        count = count + 1
      }
    }
    return count
  }

  private getLines(): string[] {
    let lines: string[] = []
    for (let name of this.names) {
      let state = this.statMap.get(name)
      let processText = '*'
      switch (state) {
        case State.Progressing: {
          let d = new Date()
          let idx = Math.floor(d.getMilliseconds() / 100)
          processText = frames[idx]
          break
        }
        case State.Failed:
          processText = '✗'
          break
        case State.Success:
          processText = '✓'
          break
      }
      let msgs = this.messagesMap.get(name) || []
      lines.push(`- ${processText} ${name} ${msgs.length ? msgs[msgs.length - 1] : ''}`)
    }
    return lines
  }

  public getMessages(line: number): string[] {
    if (line <= 1) return []
    let name = this.names[line - 2]
    if (!name) return []
    return this.messagesMap.get(name)
  }

  // draw frame
  private draw(nvim: Neovim, buffer: Buffer): void {
    let { remains } = this
    let first = remains == 0 ? `${this.isUpdate ? 'Update' : 'Install'} finished` : `Installing, ${remains} remaining...`
    let lines = [first, '', ...this.getLines()]
    buffer.setLines(lines, { start: 0, end: -1, strictIndexing: false }, true)
    if (remains == 0 && this.interval) {
      clearInterval(this.interval)
      this.interval = null
    }
    if (process.env.VIM_NODE_RPC) {
      nvim.command('redraw', true)
    }
  }

  public highlight(nvim: Neovim): void {
    nvim.call('matchadd', ['CocListFgCyan', '^\\-\\s\\zs\\*'], true)
    nvim.call('matchadd', ['CocListFgGreen', '^\\-\\s\\zs✓'], true)
    nvim.call('matchadd', ['CocListFgRed', '^\\-\\s\\zs✗'], true)
    nvim.call('matchadd', ['CocListFgYellow', '^-.\\{3\\}\\zs\\S\\+'], true)
  }

  public async show(nvim: Neovim): Promise<void> {
    let { isSync } = this
    if (this.channel) return
    nvim.pauseNotification()
    nvim.command(isSync ? 'enew' : 'vs +enew', true)
    nvim.call('bufnr', ['%'], true)
    nvim.command('setl buftype=nofile bufhidden=wipe noswapfile nobuflisted wrap undolevels=-1', true)
    if (!isSync) {
      nvim.command('nnoremap <silent><nowait><buffer> q :q<CR>', true)
    }
    this.highlight(nvim)
    let res = await nvim.resumeNotification()
    this.bufnr = res[0][1] as number
    let buffer = nvim.createBuffer(this.bufnr)
    this.interval = setInterval(() => {
      this.draw(nvim, buffer)
    }, 100)
  }

  public dispose(): void {
    if (this.interval) {
      clearInterval(this.interval)
    }
  }
}
