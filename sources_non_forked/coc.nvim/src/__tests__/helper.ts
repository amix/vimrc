/* eslint-disable @typescript-eslint/no-unsafe-return */
/* eslint-disable @typescript-eslint/no-unsafe-call */
import { Buffer, Neovim, Window } from '@chemzqm/neovim'
import * as cp from 'child_process'
import { EventEmitter } from 'events'
import fs from 'fs'
import os from 'os'
import path from 'path'
import util from 'util'
import { v4 as uuid } from 'uuid'
import { Disposable } from 'vscode-languageserver-protocol'
import attach from '../attach'
import completion from '../completion'
import events from '../events'
import Document from '../model/document'
import Plugin from '../plugin'
import { OutputChannel, VimCompleteItem } from '../types'
import { terminate } from '../util/processes'
import workspace from '../workspace'

export interface CursorPosition {
  bufnum: number
  lnum: number
  col: number
}

const nullChannel: OutputChannel = {
  content: '',
  show: () => {},
  dispose: () => {},
  name: 'null',
  append: () => {},
  appendLine: () => {},
  clear: () => {},
  hide: () => {}
}

process.on('uncaughtException', err => {
  let msg = 'Uncaught exception: ' + err.stack
  console.error(msg)
})
export class Helper extends EventEmitter {
  public nvim: Neovim
  public proc: cp.ChildProcess
  public plugin: Plugin

  constructor() {
    super()
    this.setMaxListeners(99)
  }

  public setupNvim(): void {
    const vimrc = path.resolve(__dirname, 'vimrc')
    let proc = this.proc = cp.spawn(process.env.COC_TEST_NVIM ?? 'nvim', ['-u', vimrc, '-i', 'NONE', '--embed'], {
      cwd: __dirname
    })
    let plugin = attach({ proc })
    this.nvim = plugin.nvim
  }

  public setup(): Promise<void> {
    const vimrc = path.resolve(__dirname, 'vimrc')
    let proc = this.proc = cp.spawn('nvim', ['-u', vimrc, '-i', 'NONE', '--embed'], {
      cwd: __dirname
    })
    let plugin = this.plugin = attach({ proc })
    this.nvim = plugin.nvim
    this.nvim.uiAttach(160, 80, {}).catch(e => {
      console.error(e)
    })
    this.nvim.on('notification', (method, args) => {
      if (method == 'redraw') {
        for (let arg of args) {
          let event = arg[0]
          this.emit(event, arg.slice(1))
          if (event == 'put') {
            let arr = arg.slice(1).map(o => o[0])
            let line = arr.join('').trim()
            if (line.length > 3) {
              // console.log(line)
            }
          }
        }
      }
    })
    return new Promise(resolve => {
      plugin.once('ready', resolve)
    })
  }

  public async shutdown(): Promise<void> {
    if (this.plugin) this.plugin.dispose()
    this.nvim.removeAllListeners()
    this.nvim = null
    if (this.proc) {
      this.proc.unref()
      terminate(this.proc)
      this.proc = null
    }
    await this.wait(60)
  }

  public async waitPopup(): Promise<void> {
    let visible = await this.nvim.call('pumvisible')
    if (visible) return
    let res = await events.race(['MenuPopupChanged'], 5000)
    if (!res) throw new Error('wait pum timeout after 5s')
  }

  public async waitPreviewWindow(): Promise<void> {
    for (let i = 0; i < 40; i++) {
      await this.wait(50)
      let has = await this.nvim.call('coc#list#has_preview')
      if (has > 0) return
    }
    throw new Error('timeout after 2s')
  }

  public async waitPrompt(): Promise<void> {
    for (let i = 0; i < 40; i++) {
      await this.wait(50)
      let prompt = await this.nvim.call('coc#prompt#activated')
      if (prompt) return
    }
    throw new Error('Wait prompt timeout after 2s')
  }

  public async waitFloat(): Promise<number> {
    for (let i = 0; i < 50; i++) {
      await this.wait(20)
      let winid = await this.nvim.call('GetFloatWin')
      if (winid) return winid
    }
    throw new Error('timeout after 2s')
  }

  public async selectCompleteItem(idx: number): Promise<void> {
    await this.nvim.call('nvim_select_popupmenu_item', [idx, true, true, {}])
  }

  public async doAction(method: string, ...args: any[]): Promise<any> {
    return await this.plugin.cocAction(method, ...args)
  }

  public async synchronize(): Promise<void> {
    let doc = await workspace.document
    doc.forceSync()
  }

  public async reset(): Promise<void> {
    let mode = await this.nvim.mode
    if (mode.blocking && mode.mode == 'r') {
      await this.nvim.input('<cr>')
    } else if (mode.mode != 'n' || mode.blocking) {
      await this.nvim.call('feedkeys', [String.fromCharCode(27), 'in'])
    }
    completion.stop()
    workspace.reset()
    await this.nvim.command('silent! %bwipeout!')
    await this.nvim.command('setl nopreviewwindow')
    await this.wait(30)
    await workspace.document
  }

  public async pumvisible(): Promise<boolean> {
    let res = await this.nvim.call('pumvisible', []) as number
    return res == 1
  }

  public wait(ms = 30): Promise<void> {
    return new Promise(resolve => {
      setTimeout(() => {
        resolve()
      }, ms)
    })
  }

  public async visible(word: string, source?: string): Promise<boolean> {
    await this.waitPopup()
    let context = await this.nvim.getVar('coc#_context') as any
    let items = context.candidates
    if (!items) return false
    let item = items.find(o => o.word == word)
    if (!item || !item.user_data) return false
    try {
      let arr = item.user_data.split(':', 2)
      if (source && arr[0] !== source) {
        return false
      }
    } catch (e) {
      return false
    }
    return true
  }

  public async notVisible(word: string): Promise<boolean> {
    let items = await this.getItems()
    return items.findIndex(o => o.word == word) == -1
  }

  public async getItems(): Promise<VimCompleteItem[]> {
    let visible = await this.pumvisible()
    if (!visible) return []
    let context = await this.nvim.getVar('coc#_context') as any
    let items = context.candidates
    return items || []
  }

  public async edit(file?: string): Promise<Buffer> {
    if (!file || !path.isAbsolute(file)) {
      file = path.join(__dirname, file ? file : `${uuid()}`)
    }
    let escaped = await this.nvim.call('fnameescape', file) as string
    await this.nvim.command(`edit ${escaped}`)
    let doc = await workspace.document
    return doc.buffer
  }

  public async createDocument(name?: string): Promise<Document> {
    let buf = await this.edit(name)
    let doc = workspace.getDocument(buf.id)
    if (!doc) return await workspace.document
    return doc
  }

  public async listInput(input: string): Promise<void> {
    await events.fire('InputChar', ['list', input, 0])
  }

  public async getMarkers(bufnr: number, ns: number): Promise<[number, number, number][]> {
    return await this.nvim.call('nvim_buf_get_extmarks', [bufnr, ns, 0, -1, {}]) as [number, number, number][]
  }

  public async getCmdline(): Promise<string> {
    let str = ''
    for (let i = 1, l = 70; i < l; i++) {
      let ch = await this.nvim.call('screenchar', [79, i])
      if (ch == -1) break
      str += String.fromCharCode(ch)
    }
    return str.trim()
  }

  public updateConfiguration(key: string, value: any): () => void {
    let { configurations } = workspace
    let curr = workspace.getConfiguration(key)
    configurations.updateUserConfig({ [key]: value })
    return () => {
      configurations.updateUserConfig({ [key]: curr })
    }
  }

  public async mockFunction(name: string, result: string | number | any): Promise<void> {
    let content = `
    function! ${name}(...)
      return ${typeof result == 'number' ? result : JSON.stringify(result)}
    endfunction`
    await this.nvim.exec(content)
  }

  public async items(): Promise<VimCompleteItem[]> {
    let context = await this.nvim.getVar('coc#_context')
    return context['candidates'] || []
  }

  public async screenLine(line: number): Promise<string> {
    let res = ''
    for (let i = 1; i <= 80; i++) {
      let ch = await this.nvim.call('screenchar', [line, i])
      res = res + String.fromCharCode(ch)
    }
    return res
  }

  public async getWinLines(winid: number): Promise<string[]> {
    return await this.nvim.eval(`getbufline(winbufnr(${winid}), 1, '$')`) as string[]
  }

  public async getFloat(): Promise<Window> {
    let wins = await this.nvim.windows
    let floatWin: Window
    for (let win of wins) {
      let f = await win.getVar('float')
      if (f) floatWin = win
    }
    return floatWin
  }

  public async getFloats(): Promise<Window[]> {
    let ids = await this.nvim.call('coc#float#get_float_win_list', [])
    if (!ids) return []
    return ids.map(id => this.nvim.createWindow(id))
  }

  public async getExtmarkers(bufnr: number, ns: number): Promise<[number, number, number, number, string][]> {
    let res = await this.nvim.call('nvim_buf_get_extmarks', [bufnr, ns, 0, -1, { details: true }]) as any
    return res.map(o => {
      return [o[1], o[2], o[3].end_row, o[3].end_col, o[3].hl_group]
    })
  }

  public async waitFor<T>(method: string, args: any[], value: T): Promise<void> {
    let find = false
    for (let i = 0; i < 40; i++) {
      await this.wait(50)
      let res = await this.nvim.call(method, args) as T
      if (res == value || (value instanceof RegExp && value.test(res.toString()))) {
        find = true
        break
      }
    }
    if (!find) {
      throw new Error(`waitFor ${value} timeout`)
    }
  }

  public async waitValue<T>(fn: () => T, value: T): Promise<void> {
    let find = false
    for (let i = 0; i < 40; i++) {
      await this.wait(50)
      let res = fn()
      if (res == value) {
        find = true
        break
      }
    }
    if (!find) {
      throw new Error(`waitValue ${value} timeout`)
    }
  }

  public createNullChannel(): OutputChannel {
    return nullChannel
  }
}

export function rmdir(dir: string): void {
  if (typeof fs['rm'] === 'function') {
    fs['rmSync'](dir, { recursive: true })
  } else {
    fs.rmdirSync(dir, { recursive: true })
  }
}

export async function createTmpFile(content: string, disposables?: Disposable[]): Promise<string> {
  let tmpFolder = path.join(os.tmpdir(), `coc-${process.pid}`)
  if (!fs.existsSync(tmpFolder)) {
    fs.mkdirSync(tmpFolder)
  }
  let fsPath = path.join(tmpFolder, uuid())
  await util.promisify(fs.writeFile)(fsPath, content, 'utf8')
  if (disposables) {
    disposables.push(Disposable.create(() => {
      if (fs.existsSync(fsPath)) fs.unlinkSync(fsPath)
    }))
  }
  return fsPath
}

export default new Helper()
