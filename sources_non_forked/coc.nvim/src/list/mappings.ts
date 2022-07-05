'use strict'
import { Neovim } from '@chemzqm/neovim'
import { ListMode } from '../types'
import window from '../window'
import ListConfiguration, { validKeys } from './configuration'
import { ListManager } from './manager'
const logger = require('../util/logger')('list-mappings')

export default class Mappings {
  private insertMappings: Map<string, () => void | Promise<void>> = new Map()
  private normalMappings: Map<string, () => void | Promise<void>> = new Map()
  private userInsertMappings: Map<string, string> = new Map()
  private userNormalMappings: Map<string, string> = new Map()
  private actions: Map<string, (expr?: string) => void | Promise<void>> = new Map()

  constructor(private manager: ListManager,
    private nvim: Neovim,
    private config: ListConfiguration) {
    let { prompt } = manager
    this.addAction('do:switch', async () => {
      await manager.switchMatcher()
    })
    this.addAction('do:selectall', async () => {
      await manager.session?.ui.selectAll()
    })
    this.addAction('do:help', async () => {
      await manager.session?.showHelp()
    })
    this.addAction('do:refresh', async () => {
      await manager.session?.reloadItems()
    })
    this.addAction('do:exit', async () => {
      await manager.cancel()
    })
    this.addAction('do:stop', () => {
      manager.stop()
    })
    this.addAction('do:cancel', async () => {
      await manager.cancel(false)
    })
    this.addAction('do:toggle', async () => {
      await manager.session?.ui.toggleSelection()
    })
    this.addAction('do:jumpback', () => {
      manager.session?.jumpBack()
    })
    this.addAction('do:previous', async () => {
      await manager.normal('k')
    })
    this.addAction('do:next', async () => {
      await manager.normal('j')
    })
    this.addAction('do:defaultaction', async () => {
      await manager.doAction()
    })
    this.addAction('do:chooseaction', async () => {
      await manager.chooseAction()
    })
    this.addAction('do:togglemode', () => {
      manager.toggleMode()
    })
    this.addAction('do:previewtoggle', async () => {
      await manager.togglePreview()
    })
    this.addAction('do:previewup', () => {
      this.scrollPreview('up')
    })
    this.addAction('do:previewdown', () => {
      this.scrollPreview('down')
    })
    this.addAction('do:command', async () => {
      await manager.cancel(false)
      await nvim.eval('feedkeys(":")')
    })
    this.addAction('prompt:previous', () => {
      manager.session?.history.previous()
    })
    this.addAction('prompt:next', () => {
      manager.session?.history.next()
    })
    this.addAction('prompt:start', () => {
      prompt.moveToStart()
    })
    this.addAction('prompt:end', () => {
      prompt.moveToEnd()
    })
    this.addAction('prompt:left', () => {
      prompt.moveLeft()
    })
    this.addAction('prompt:right', () => {
      prompt.moveRight()
    })
    this.addAction('prompt:deleteforward', () => {
      prompt.onBackspace()
    })
    this.addAction('prompt:deletebackward', () => {
      prompt.removeNext()
    })
    this.addAction('prompt:removetail', () => {
      prompt.removeTail()
    })
    this.addAction('prompt:removeahead', () => {
      prompt.removeAhead()
    })
    this.addAction('prompt:removeword', () => {
      prompt.removeWord()
    })
    this.addAction('prompt:insertregister', () => {
      prompt.insertRegister()
    })
    this.addAction('prompt:paste', async () => {
      await prompt.paste()
    })
    this.addAction('eval', async expr => {
      await prompt.eval(expr)
    })
    this.addAction('command', async expr => {
      await manager.command(expr)
    })
    this.addAction('action', async expr => {
      await manager.doAction(expr)
    })
    this.addAction('feedkeys', async expr => {
      await manager.feedkeys(expr)
    })
    this.addAction('normal', async expr => {
      await manager.normal(expr, false)
    })
    this.addAction('normal!', async expr => {
      await manager.normal(expr, true)
    })
    this.addAction('call', async expr => {
      await manager.call(expr)
    })
    this.addAction('expr', async expr => {
      let name = await manager.call(expr)
      if (name) await manager.doAction(name)
    })

    this.addKeyMapping('insert', '<C-s>', 'do:switch')
    this.addKeyMapping('insert', '<C-n>', 'prompt:next')
    this.addKeyMapping('insert', '<C-p>', 'prompt:previous')
    this.addKeyMapping('insert', '<C-v>', 'prompt:paste')
    this.addKeyMapping('insert', ['<C-m>', '<cr>'], 'do:defaultaction')
    this.addKeyMapping('insert', ['<tab>', '<C-i>', '\t'], 'do:chooseaction')
    this.addKeyMapping('insert', '<C-o>', 'do:togglemode')
    this.addKeyMapping('insert', '<C-c>', 'do:stop')
    this.addKeyMapping('insert', '<C-l>', 'do:refresh')
    this.addKeyMapping('insert', '<left>', 'prompt:left')
    this.addKeyMapping('insert', '<right>', 'prompt:right')
    this.addKeyMapping('insert', ['<end>', '<C-e>'], 'prompt:end')
    this.addKeyMapping('insert', ['<home>', '<C-a>'], 'prompt:start')
    this.addKeyMapping('insert', ['<C-h>', '<bs>', '<backspace>'], 'prompt:deleteforward')
    this.addKeyMapping('insert', '<C-w>', 'prompt:removeword')
    this.addKeyMapping('insert', '<C-u>', 'prompt:removeahead')
    this.addKeyMapping('insert', '<C-r>', 'prompt:insertregister')
    // normal
    this.addKeyMapping('normal', 't', 'action:tabe')
    this.addKeyMapping('normal', 's', 'action:split')
    this.addKeyMapping('normal', 'd', 'action:drop')
    this.addKeyMapping('normal', ['<cr>', '<C-m>', '\r'], 'do:defaultaction')
    this.addKeyMapping('normal', '<C-a>', 'do:selectall')
    this.addKeyMapping('normal', ' ', 'do:toggle')
    this.addKeyMapping('normal', 'p', 'do:previewtoggle')
    this.addKeyMapping('normal', ['<tab>', '\t', '<C-i>'], 'do:chooseaction')
    this.addKeyMapping('normal', '<C-c>', 'do:stop')
    this.addKeyMapping('normal', '<C-l>', 'do:refresh')
    this.addKeyMapping('normal', '<C-o>', 'do:jumpback')
    this.addKeyMapping('normal', '<C-e>', 'do:previewdown')
    this.addKeyMapping('normal', '<C-y>', 'do:previewup')
    this.addKeyMapping('normal', ['i', 'I', 'o', 'O', 'a', 'A'], 'do:togglemode')
    this.addKeyMapping('normal', '?', 'do:help')
    this.addKeyMapping('normal', ':', 'do:command')
    this.createMappings()
    config.on('change', () => {
      this.createMappings()
    })
  }

  private createMappings(): void {
    let insertMappings = this.config.get<any>('insertMappings', {})
    this.userInsertMappings = this.fixUserMappings(insertMappings, 'list.insertMappings')
    let normalMappings = this.config.get<any>('normalMappings', {})
    this.userNormalMappings = this.fixUserMappings(normalMappings, 'list.normalMappings')
  }

  public hasUserMapping(mode: ListMode, key: string): boolean {
    let map = mode == 'insert' ? this.userInsertMappings : this.userNormalMappings
    return map.has(key)
  }

  public isValidAction(action: string): boolean {
    if (this.actions.has(action)) return true
    let [key, expr] = action.split(':', 2)
    if (!expr || !this.actions.has(key)) return false
    return true
  }

  private fixUserMappings(mappings: { [key: string]: string }, entry: string): Map<string, string> {
    let res: Map<string, string> = new Map()
    for (let [key, value] of Object.entries(mappings)) {
      if (!this.isValidAction(value)) {
        window.showMessage(`Invalid configuration - unable to support action "${value}" in "${entry}"`, 'warning')
        continue
      }
      if (key.length == 1) {
        res.set(key, value)
      } else if (key.startsWith('<') && key.endsWith('>')) {
        if (key.toLowerCase() == '<space>') {
          res.set(' ', value)
        } else if (key.toLowerCase() == '<backspace>') {
          res.set('<bs>', value)
        } else if (validKeys.includes(key)) {
          res.set(key, value)
        } else {
          let find = false
          for (let i = 0; i < validKeys.length; i++) {
            if (validKeys[i].toLowerCase() == key.toLowerCase()) {
              find = true
              res.set(validKeys[i], value)
              break
            }
          }
          if (!find) window.showMessage(`Invalid configuration - unable to recognize "${key}" in "${entry}"`, 'warning')
        }
      } else {
        window.showMessage(`Invalid configuration - unable to recognize key "${key}" in "${entry}"`, 'warning')
      }
    }
    return res
  }

  public async doInsertKeymap(key: string): Promise<boolean> {
    let nextKey = this.config.nextKey
    let previousKey = this.config.previousKey
    if (key == nextKey) {
      this.manager?.session.ui.moveDown()
      return true
    }
    if (key == previousKey) {
      this.manager?.session.ui.moveUp()
      return true
    }
    let expr = this.userInsertMappings.get(key)
    if (expr) {
      let fn = this.getAction(expr)
      await Promise.resolve(fn())
      return true
    }
    if (this.insertMappings.has(key)) {
      let fn = this.insertMappings.get(key)
      await Promise.resolve(fn())
      return true
    }
    return false
  }

  public async doNormalKeymap(key: string): Promise<boolean> {
    let expr = this.userNormalMappings.get(key)
    if (expr) {
      let fn = this.getAction(expr)
      await Promise.resolve(fn())
      return true
    }
    if (this.normalMappings.has(key)) {
      let fn = this.normalMappings.get(key)
      await Promise.resolve(fn())
      return true
    }
    return false
  }

  private addKeyMapping(mode: ListMode, key: string | string[], action: string): void {
    let mappings = mode == 'insert' ? this.insertMappings : this.normalMappings
    let fn = this.getAction(action)
    if (Array.isArray(key)) {
      for (let k of key) {
        mappings.set(k, fn)
      }
    } else {
      mappings.set(key, fn)
    }
  }

  private addAction(key: string, fn: (expr?: string) => void | Promise<void>): void {
    this.actions.set(key, fn)
  }

  public getAction(action: string): () => void | Promise<void> {
    if (this.actions.has(action)) return () => {
      return this.doAction(action)
    }
    let [key, expr] = action.split(':', 2)
    if (!expr || !this.actions.has(key)) throw new Error(`Invalid action ${action}`)
    return () => {
      return this.doAction(key, expr)
    }
  }

  public async doAction(key: string, expr?: string): Promise<void> {
    let fn = this.actions.get(key)
    if (!fn) throw new Error(`Action ${key} doesn't exist`)
    await Promise.resolve(fn(expr))
  }

  private scrollPreview(dir: 'up' | 'down'): void {
    let { nvim } = this
    nvim.pauseNotification()
    nvim.call('coc#list#scroll_preview', [dir], true)
    nvim.command('redraw', true)
    nvim.resumeNotification(false, true)
  }
}
