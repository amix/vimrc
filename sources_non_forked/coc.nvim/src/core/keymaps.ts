'use strict'
import { Neovim } from '@chemzqm/neovim'
import { v1 as uuid } from 'uuid'
import { Disposable } from 'vscode-languageserver-protocol'
import { KeymapOption } from '../types'
import { getKeymapModifier, MapMode } from '../util'
import Documents from './documents'
const logger = require('../util/logger')('core-keymaps')

export default class Keymaps {
  private readonly keymaps: Map<string, [Function, boolean]> = new Map()
  private nvim: Neovim
  constructor(private documents: Documents) {
  }

  public attach(nvim: Neovim): void {
    this.nvim = nvim
  }

  public async doKeymap(key: string, defaultReturn = '', pressed?: string): Promise<string> {
    let keymap = this.keymaps.get(key)
    if (!keymap) {
      logger.error(`keymap for ${key} not found`)
      if (pressed) this.nvim.command(`silent! unmap <buffer> ${pressed.startsWith('{') && pressed.endsWith('}') ? `<${pressed.slice(1, -1)}>` : pressed}`, true)
      return defaultReturn
    }
    let [fn, repeat] = keymap
    let res = await Promise.resolve(fn())
    if (repeat) await this.nvim.command(`silent! call repeat#set("\\<Plug>(coc-${key})", -1)`)
    return res ?? defaultReturn
  }

  /**
   * Register <Plug>(coc-${key}) key mapping.
   */
  public registerKeymap(modes: MapMode[], key: string, fn: Function, opts: Partial<KeymapOption> = {}): Disposable {
    if (!key) throw new Error(`Invalid key ${key} of registerKeymap`)
    if (this.keymaps.has(key)) throw new Error(`${key} already exists.`)
    opts = Object.assign({ sync: true, cancel: true, silent: true, repeat: false }, opts)
    let { nvim } = this
    this.keymaps.set(key, [fn, !!opts.repeat])
    let method = opts.sync ? 'request' : 'notify'
    let silent = opts.silent ? '<silent>' : ''
    for (let m of modes) {
      if (m == 'i') {
        nvim.command(`inoremap ${silent}<expr> <Plug>(coc-${key}) coc#_insert_key('${method}', '${key}', ${opts.cancel ? 1 : 0})`, true)
      } else {
        let modify = getKeymapModifier(m)
        nvim.command(`${m}noremap ${silent} <Plug>(coc-${key}) :${modify}call coc#rpc#${method}('doKeymap', ['${key}'])<cr>`, true)
      }
    }
    return Disposable.create(() => {
      this.keymaps.delete(key)
      for (let m of modes) {
        nvim.command(`${m}unmap <Plug>(coc-${key})`, true)
      }
    })
  }

  public registerExprKeymap(mode: 'i' | 'n' | 'v' | 's' | 'x', key: string, fn: Function, buffer = false): Disposable {
    let id = `${mode}${global.Buffer.from(key).toString('base64')}${buffer ? '1' : '0'}`
    let { nvim } = this
    this.keymaps.set(id, [fn, false])
    if (mode == 'i') {
      nvim.command(`inoremap <silent><expr>${buffer ? '<nowait><buffer>' : ''} ${key} coc#_insert_key('request', '${id}')`, true)
    } else {
      nvim.command(`${mode}noremap <silent><expr>${buffer ? '<nowait><buffer>' : ''} ${key} coc#rpc#request('doKeymap', ['${id}'])`, true)
    }
    return Disposable.create(() => {
      this.keymaps.delete(id)
      nvim.command(`${mode}unmap ${buffer ? '<buffer>' : ''} ${key}`, true)
    })
  }

  public registerLocalKeymap(mode: 'n' | 'v' | 's' | 'x', key: string, fn: Function, notify = false): Disposable {
    let id = uuid()
    let { nvim } = this
    let bufnr = this.documents.bufnr
    this.keymaps.set(id, [fn, false])
    let method = notify ? 'notify' : 'request'
    let modify = getKeymapModifier(mode)
    // neoivm's bug '<' can't be used.
    let escaped = key.startsWith('<') && key.endsWith('>') ? `{${key.slice(1, -1)}}` : key
    if (this.nvim.hasFunction('nvim_buf_set_keymap') && !global.hasOwnProperty('__TEST__')) {
      nvim.call('nvim_buf_set_keymap', [0, mode, key, `:${modify}call coc#rpc#${method}('doKeymap', ['${id}', '', '${escaped}'])<CR>`, {
        silent: true,
        nowait: true
      }], true)
    } else {
      let cmd = `${mode}noremap <silent><nowait><buffer> ${key} :${modify}call coc#rpc#${method}('doKeymap', ['${id}', '', '${escaped}'])<CR>`
      nvim.command(cmd, true)
    }
    return Disposable.create(() => {
      this.keymaps.delete(id)
      nvim.call('coc#compat#buf_del_keymap', [bufnr, mode, key], true)
    })
  }
}
