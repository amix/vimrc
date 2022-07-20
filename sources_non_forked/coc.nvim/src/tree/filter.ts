'use strict'
import events from '../events'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import { disposeAll } from '../util'
export const sessionKey = 'filter'

export default class Filter<T> {
  private _activated = false
  private text: string
  private history: string[] = []
  private disposables: Disposable[] = []
  private readonly _onDidUpdate = new Emitter<string>()
  private readonly _onDidExit = new Emitter<T | undefined>()
  private readonly _onDidKeyPress = new Emitter<string>()
  public readonly onDidKeyPress: Event<string> = this._onDidKeyPress.event
  public readonly onDidUpdate: Event<string> = this._onDidUpdate.event
  public readonly onDidExit: Event<T | undefined> = this._onDidExit.event
  constructor(private nvim: Neovim, keys: string[]) {
    this.text = ''
    events.on('InputChar', (session, character) => {
      if (session !== sessionKey || !this._activated) return
      if (!keys.includes(character)) {
        if (character.length == 1) {
          this.text = this.text + character
          this._onDidUpdate.fire(this.text)
          return
        }
        if (character == '<bs>' || character == '<C-h>') {
          this.text = this.text.slice(0, -1)
          this._onDidUpdate.fire(this.text)
          return
        }
        if (character == '<C-u>') {
          this.text = ''
          this._onDidUpdate.fire(this.text)
          return
        }
        if (character == '<C-n>') {
          let idx = this.history.indexOf(this.text)
          let text = this.history[idx + 1] || this.history[0]
          if (text) {
            this.text = text
            this._onDidUpdate.fire(this.text)
          }
          return
        }
        if (character == '<C-p>') {
          let idx = this.history.indexOf(this.text)
          let text = this.history[idx - 1] || this.history[this.history.length - 1]
          if (text) {
            this.text = text
            this._onDidUpdate.fire(this.text)
          }
        }
        if (character == '<esc>' || character == '<C-o>') {
          this.deactivate()
          return
        }
      }
      this._onDidKeyPress.fire(character)
    }, null, this.disposables)
  }

  public active(): void {
    if (this._activated) return
    this._activated = true
    this.text = ''
    this.nvim.call('coc#prompt#start_prompt', [sessionKey], true)
  }

  public deactivate(node?: T): void {
    if (!this._activated) return
    this.nvim.call('coc#prompt#stop_prompt', [sessionKey], true)
    this._activated = false
    let { text } = this
    this.text = ''
    this._onDidExit.fire(node)
    if (text && !this.history.includes(text)) {
      this.history.push(text)
    }
  }

  public get activated(): boolean {
    return this._activated
  }

  public dispose(): void {
    this.deactivate()
    this.history = []
    this._onDidKeyPress.dispose()
    this._onDidUpdate.dispose()
    this._onDidExit.dispose()
    disposeAll(this.disposables)
  }
}
