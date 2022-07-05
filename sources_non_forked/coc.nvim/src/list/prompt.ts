'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Emitter, Event } from 'vscode-languageserver-protocol'
import { ListMode, ListOptions, Matcher } from '../types'
import ListConfiguration from './configuration'
const logger = require('../util/logger')('list-prompt')

export default class Prompt {
  private cusorIndex = 0
  private _input = ''
  private _matcher: Matcher | ''
  private _mode: ListMode = 'insert'
  private interactive = false
  private requestInput = false

  private _onDidChangeInput = new Emitter<string>()
  public readonly onDidChangeInput: Event<string> = this._onDidChangeInput.event

  constructor(private nvim: Neovim, private config: ListConfiguration) {
  }

  public get input(): string {
    return this._input
  }

  public set input(str: string) {
    if (this._input == str) return
    this.cusorIndex = str.length
    this._input = str
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public get mode(): ListMode {
    return this._mode
  }

  public set mode(val: ListMode) {
    if (val == this._mode) return
    this._mode = val
    this.drawPrompt()
  }

  public set matcher(val: Matcher) {
    this._matcher = val
    this.drawPrompt()
  }

  public start(opts?: ListOptions): void {
    if (opts) {
      this.interactive = opts.interactive
      this.cusorIndex = opts.input.length
      this._input = opts.input
      this._mode = opts.mode
      this._matcher = opts.interactive ? '' : opts.matcher
    }
    this.nvim.call('coc#prompt#start_prompt', ['list'], true)
    this.drawPrompt()
  }

  public cancel(): void {
    let { nvim } = this
    nvim.call('coc#prompt#stop_prompt', ['list'], true)
  }

  public reset(): void {
    this._input = ''
    this.cusorIndex = 0
  }

  public drawPrompt(): void {
    let indicator = this.config.get<string>('indicator', '>')
    let { cusorIndex, interactive, input, _matcher } = this
    let cmds = ['echo ""']
    if (this.mode == 'insert') {
      if (interactive) {
        cmds.push(`echohl MoreMsg | echon 'INTERACTIVE ' | echohl None`)
      } else if (_matcher) {
        cmds.push(`echohl MoreMsg | echon '${_matcher.toUpperCase()} ' | echohl None`)
      }
      cmds.push(`echohl Special | echon '${indicator} ' | echohl None`)
      if (cusorIndex == input.length) {
        cmds.push(`echon '${input.replace(/'/g, "''")}'`)
        cmds.push(`echohl Cursor | echon ' ' | echohl None`)
      } else {
        let pre = input.slice(0, cusorIndex)
        if (pre) cmds.push(`echon '${pre.replace(/'/g, "''")}'`)
        cmds.push(`echohl Cursor | echon '${input[cusorIndex].replace(/'/, "''")}' | echohl None`)
        let post = input.slice(cusorIndex + 1)
        cmds.push(`echon '${post.replace(/'/g, "''")}'`)
      }
    } else {
      cmds.push(`echohl MoreMsg | echo "" | echohl None`)
    }
    cmds.push('redraw')
    let cmd = cmds.join('|')
    this.nvim.command(cmd, true)
  }

  public moveLeft(): void {
    if (this.cusorIndex == 0) return
    this.cusorIndex = this.cusorIndex - 1
    this.drawPrompt()
  }

  public moveRight(): void {
    if (this.cusorIndex == this._input.length) return
    this.cusorIndex = this.cusorIndex + 1
    this.drawPrompt()
  }

  public moveToEnd(): void {
    if (this.cusorIndex == this._input.length) return
    this.cusorIndex = this._input.length
    this.drawPrompt()
  }

  public moveToStart(): void {
    if (this.cusorIndex == 0) return
    this.cusorIndex = 0
    this.drawPrompt()
  }

  public onBackspace(): void {
    let { cusorIndex, input } = this
    if (cusorIndex == 0) return
    let pre = input.slice(0, cusorIndex)
    let post = input.slice(cusorIndex)
    this.cusorIndex = cusorIndex - 1
    this._input = `${pre.slice(0, pre.length - 1)}${post}`
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public removeNext(): void {
    let { cusorIndex, input } = this
    if (cusorIndex == input.length - 1) return
    let pre = input.slice(0, cusorIndex)
    let post = input.slice(cusorIndex + 1)
    this._input = `${pre}${post}`
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public removeWord(): void {
    let { cusorIndex, input } = this
    if (cusorIndex == 0) return
    let pre = input.slice(0, cusorIndex)
    let post = input.slice(cusorIndex)
    let remain = pre.replace(/[\w$]+([^\w$]+)?$/, '')
    this.cusorIndex = cusorIndex - (pre.length - remain.length)
    this._input = `${remain}${post}`
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public removeTail(): void {
    let { cusorIndex, input } = this
    if (cusorIndex == input.length) return
    let pre = input.slice(0, cusorIndex)
    this._input = pre
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public removeAhead(): void {
    let { cusorIndex, input } = this
    if (cusorIndex == 0) return
    let post = input.slice(cusorIndex)
    this.cusorIndex = 0
    this._input = post
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }

  public async acceptCharacter(ch: string): Promise<void> {
    if (this.requestInput) {
      this.requestInput = false
      if (/^[0-9a-z"%#*+/:\-.]$/.test(ch)) {
        let text = await this.nvim.call('getreg', ch) as string
        text = text.replace(/\n/g, ' ')
        this.addText(text)
      }
    } else {
      this.addText(ch)
    }
  }

  public insertRegister(): void {
    this.requestInput = true
  }

  public async paste(): Promise<void> {
    let text = await this.nvim.eval('@*') as string
    text = text.replace(/\n/g, '')
    if (!text) return
    this.addText(text)
  }

  public async eval(expression: string): Promise<void> {
    let text = await this.nvim.call('eval', [expression]) as string
    text = text.replace(/\n/g, '')
    this.addText(text)
  }

  private addText(text: string): void {
    let { cusorIndex, input } = this
    this.cusorIndex = cusorIndex + text.length
    let pre = input.slice(0, cusorIndex)
    let post = input.slice(cusorIndex)
    this._input = `${pre}${text}${post}`
    this.drawPrompt()
    this._onDidChangeInput.fire(this._input)
  }
}
