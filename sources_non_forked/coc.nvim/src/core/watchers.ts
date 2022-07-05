'use strict'
import events from '../events'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import { Env } from '../types'
import { disposeAll } from '../util'
const logger = require('../util/logger')('core-watchers')

export default class Watchers implements Disposable {
  private nvim: Neovim
  private env: Env
  private watchedOptions: Set<string> = new Set()
  private disposables: Disposable[] = []
  private _onDidRuntimePathChange = new Emitter<string[]>()
  private readonly _onDidOptionChange = new Emitter<void>()

  public readonly onDidRuntimePathChange: Event<string[]> = this._onDidRuntimePathChange.event
  public readonly onDidOptionChange: Event<void> = this._onDidOptionChange.event

  public get options(): string[] {
    return Array.from(this.watchedOptions)
  }

  public attach(nvim: Neovim, env: Env): void {
    this.nvim = nvim
    this.env = env
    this.watchOption('runtimepath', (oldValue, newValue: string) => {
      let oldList: string[] = oldValue.split(',')
      let newList: string[] = newValue.split(',')
      let paths = newList.filter(x => !oldList.includes(x))
      if (paths.length > 0) {
        this._onDidRuntimePathChange.fire(paths)
      }
      this.env.runtimepath = newValue
    }, this.disposables)
  }

  /**
   * Watch for option change.
   */
  public watchOption(key: string, callback: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void {
    let watching = this.watchedOptions.has(key)
    if (!watching) {
      this.watchedOptions.add(key)
      this._onDidOptionChange.fire()
    }
    let disposable = events.on('OptionSet', async (changed: string, oldValue: any, newValue: any) => {
      if (changed == key && callback) {
        await Promise.resolve(callback(oldValue, newValue))
      }
    })
    if (disposables) {
      disposables.push(
        Disposable.create(() => {
          disposable.dispose()
          if (watching) return
          this.watchedOptions.delete(key)
          this._onDidOptionChange.fire()
        })
      )
    }
  }

  /**
   * Watch global variable, works on neovim only.
   */
  public watchGlobal(key: string, callback: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void {
    let { nvim } = this
    nvim.call('coc#_watch', key, true)
    let disposable = events.on('GlobalChange', async (changed: string, oldValue: any, newValue: any) => {
      if (changed == key) {
        await Promise.resolve(callback(oldValue, newValue))
      }
    })
    if (disposables) {
      disposables.push(
        Disposable.create(() => {
          disposable.dispose()
          nvim.call('coc#_unwatch', key, true)
        })
      )
    }
  }

  public dispose(): void {
    disposeAll(this.disposables)
    this._onDidOptionChange.dispose()
    this._onDidRuntimePathChange.dispose()
  }
}
