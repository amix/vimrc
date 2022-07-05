'use strict'
import { Neovim } from '@chemzqm/neovim'
import { OutputChannel } from '../types'
const logger = require('../util/logger')('outpubChannel')

export default class BufferChannel implements OutputChannel {
  private lines: string[] = ['']
  private _disposed = false
  public created = false
  constructor(public name: string, private nvim: Neovim, private onDispose?: () => void) {
    if (!/^[\w\s-.]+$/.test(name)) throw new Error(`Invalid channel name "${name}", only word characters and white space allowed.`)
  }

  public get content(): string {
    return this.lines.join('\n')
  }

  private _append(value: string): void {
    let { nvim } = this
    let idx = this.lines.length - 1
    let newlines = value.split(/\r?\n/)
    let lastline = this.lines[idx] + newlines[0]
    this.lines[idx] = lastline
    let append = newlines.slice(1)
    this.lines = this.lines.concat(append)
    if (!this.created) return
    nvim.pauseNotification()
    nvim.call('setbufline', [this.bufname, '$', lastline], true)
    if (append.length) {
      nvim.call('appendbufline', [this.bufname, '$', append], true)
    }
    nvim.resumeNotification(false, true)
  }

  public append(value: string): void {
    if (!this.validate()) return
    this._append(value)
  }

  public appendLine(value: string): void {
    if (!this.validate()) return
    this._append(value + '\n')
  }

  public clear(keep?: number): void {
    if (!this.validate()) return
    let { nvim } = this
    this.lines = keep ? this.lines.slice(-keep) : []
    if (!this.created) return
    nvim.pauseNotification()
    nvim.call('deletebufline', [this.bufname, 1, '$'], true)
    if (this.lines.length) {
      nvim.call('appendbufline', [this.bufname, '$', this.lines], true)
    }
    nvim.resumeNotification(true, true)
  }

  public hide(): void {
    this.created = false
    this.nvim.command(`exe 'silent! bd! '.fnameescape('${this.bufname}')`, true)
  }

  private get bufname(): string {
    return `output:///${this.name}`
  }

  public show(preserveFocus?: boolean, cmd = 'vs'): void {
    let { nvim } = this
    nvim.pauseNotification()
    nvim.command(`exe '${cmd} '.fnameescape('${this.bufname}')`, true)
    if (preserveFocus) {
      nvim.command('wincmd p', true)
    }
    nvim.resumeNotification(true, true)
    this.created = true
  }

  private validate(): boolean {
    return !this._disposed
  }

  public dispose(): void {
    if (this.onDispose) this.onDispose()
    this._disposed = true
    this.hide()
    this.lines = []
  }
}
