'use strict'
import workspace from '../workspace'
import window from '../window'
import { WorkspaceConfiguration } from '../types'
import { EventEmitter } from 'events'
import { Disposable } from 'vscode-languageserver-protocol'

export const validKeys = [
  '<esc>',
  '<space>',
  '<tab>',
  '<s-tab>',
  '<bs>',
  '<right>',
  '<left>',
  '<up>',
  '<down>',
  '<home>',
  '<end>',
  '<cr>',
  '<FocusGained>',
  '<FocusLost>',
  '<ScrollWheelUp>',
  '<ScrollWheelDown>',
  '<LeftMouse>',
  '<LeftDrag>',
  '<LeftRelease>',
  '<2-LeftMouse>',
  '<C-a>',
  '<C-b>',
  '<C-c>',
  '<C-d>',
  '<C-e>',
  '<C-f>',
  '<C-g>',
  '<C-h>',
  '<C-i>',
  '<C-j>',
  '<C-k>',
  '<C-l>',
  '<C-m>',
  '<C-n>',
  '<C-o>',
  '<C-p>',
  '<C-q>',
  '<C-r>',
  '<C-s>',
  '<C-t>',
  '<C-u>',
  '<C-v>',
  '<C-w>',
  '<C-x>',
  '<C-y>',
  '<C-z>',
  '<A-a>',
  '<A-b>',
  '<A-c>',
  '<A-d>',
  '<A-e>',
  '<A-f>',
  '<A-g>',
  '<A-h>',
  '<A-i>',
  '<A-j>',
  '<A-k>',
  '<A-l>',
  '<A-m>',
  '<A-n>',
  '<A-o>',
  '<A-p>',
  '<A-q>',
  '<A-r>',
  '<A-s>',
  '<A-t>',
  '<A-u>',
  '<A-v>',
  '<A-w>',
  '<A-x>',
  '<A-y>',
  '<A-z>',
]

export default class ListConfiguration extends EventEmitter {
  private configuration: WorkspaceConfiguration
  private disposable: Disposable
  constructor() {
    super()
    this.configuration = workspace.getConfiguration('list')
    this.disposable = workspace.onDidChangeConfiguration(e => {
      if (e.affectsConfiguration('list')) {
        this.configuration = workspace.getConfiguration('list')
        this.emit('change')
      }
    })
  }

  public get<T>(key: string, defaultValue?: T): T {
    return this.configuration.get<T>(key, defaultValue)
  }

  public get previousKey(): string {
    return this.fixKey(this.configuration.get<string>('previousKeymap', '<C-j>'))
  }

  public get nextKey(): string {
    return this.fixKey(this.configuration.get<string>('nextKeymap', '<C-k>'))
  }

  public dispose(): void {
    this.disposable.dispose()
    this.removeAllListeners()
  }

  public fixKey(key: string): string {
    if (validKeys.includes(key)) return key
    let find = validKeys.find(s => s.toLowerCase() == key.toLowerCase())
    if (find) return find
    window.showMessage(`Configured key "${key}" not supported.`, 'error')
    return null
  }
}
