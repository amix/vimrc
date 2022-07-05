'use strict'
import { Neovim } from '@chemzqm/neovim'
import debounce from 'debounce'
import { CancellationTokenSource, Disposable } from 'vscode-languageserver-protocol'
import events from '../events'
import extensions from '../extensions'
import { IList, ListItem, ListOptions, ListTask, Matcher } from '../types'
import { disposeAll } from '../util'
import workspace from '../workspace'
import window from '../window'
import ListConfiguration from './configuration'
import Mappings from './mappings'
import Prompt from './prompt'
import ListSession from './session'
import CommandsList from './source/commands'
import DiagnosticsList from './source/diagnostics'
import ExtensionList from './source/extensions'
import FolderList from './source/folders'
import LinksList from './source/links'
import ListsList from './source/lists'
import LocationList from './source/location'
import OutlineList from './source/outline'
import ServicesList from './source/services'
import SourcesList from './source/sources'
import SymbolsList from './source/symbols'
import stripAnsi from 'strip-ansi'
const logger = require('../util/logger')('list-manager')

const mouseKeys = ['<LeftMouse>', '<LeftDrag>', '<LeftRelease>', '<2-LeftMouse>']

export class ListManager implements Disposable {
  public prompt: Prompt
  public config: ListConfiguration
  public mappings: Mappings
  private nvim: Neovim
  private plugTs = 0
  private sessionsMap: Map<string, ListSession> = new Map()
  private lastSession: ListSession | undefined
  private disposables: Disposable[] = []
  private listMap: Map<string, IList> = new Map()

  public init(nvim: Neovim): void {
    this.nvim = nvim
    this.config = new ListConfiguration()
    this.prompt = new Prompt(nvim, this.config)
    this.mappings = new Mappings(this, nvim, this.config)
    let signText = this.config.get<string>('selectedSignText', '*')
    nvim.command(`sign define CocSelected text=${signText} texthl=CocSelectedText linehl=CocSelectedLine`, true)
    events.on('InputChar', this.onInputChar, this, this.disposables)
    let debounced = debounce(async () => {
      let session = await this.getCurrentSession()
      if (session) this.prompt.drawPrompt()
    }, 100)
    events.on('FocusGained', debounced, null, this.disposables)
    events.on('WinEnter', winid => {
      let session = this.getSessionByWinid(winid)
      if (session) this.prompt.start(session.listOptions)
    }, null, this.disposables)
    let timer: NodeJS.Timer
    events.on('WinLeave', winid => {
      if (timer) clearTimeout(timer)
      let session = this.getSessionByWinid(winid)
      if (session) {
        setTimeout(() => {
          this.prompt.cancel()
        }, workspace.isVim ? 50 : 0)
      }
    }, null, this.disposables)
    this.disposables.push({
      dispose: () => {
        debounced.clear()
      }
    })
    // filter history on input
    this.prompt.onDidChangeInput(() => {
      let { session } = this
      if (!session) return
      session.onInputChange()
      session.history.filter()
    })
    this.registerList(new LinksList(nvim))
    this.registerList(new LocationList(nvim))
    this.registerList(new SymbolsList(nvim))
    this.registerList(new OutlineList(nvim))
    this.registerList(new CommandsList(nvim))
    this.registerList(new ExtensionList(nvim))
    this.registerList(new DiagnosticsList(nvim, this))
    this.registerList(new SourcesList(nvim))
    this.registerList(new ServicesList(nvim))
    this.registerList(new ListsList(nvim, this.listMap))
    this.registerList(new FolderList(nvim))
  }

  public async start(args: string[]): Promise<void> {
    let res = this.parseArgs(args)
    if (!res) return
    let { name } = res.list
    let curr = this.sessionsMap.get(name)
    if (curr) curr.dispose()
    this.prompt.start(res.options)
    let session = new ListSession(this.nvim, this.prompt, res.list, res.options, res.listArgs, this.config)
    this.sessionsMap.set(name, session)
    this.lastSession = session
    try {
      await session.start(args)
    } catch (e) {
      this.nvim.call('coc#prompt#stop_prompt', ['list'], true)
      let msg = e instanceof Error ? e.message : e.toString()
      window.showMessage(`Error on "CocList ${name}": ${msg}`, 'error')
      logger.error(e)
    }
  }

  private getSessionByWinid(winid: number): ListSession | null {
    for (let session of this.sessionsMap.values()) {
      if (session && session.winid == winid) {
        this.lastSession = session
        return session
      }
    }
    return null
  }

  private async getCurrentSession(): Promise<ListSession | null> {
    let { id } = await this.nvim.window
    for (let session of this.sessionsMap.values()) {
      if (session && session.winid == id) {
        this.lastSession = session
        return session
      }
    }
    return null
  }

  public async resume(name?: string): Promise<void> {
    if (!name) {
      await this.session?.resume()
    } else {
      let session = this.sessionsMap.get(name)
      if (!session) {
        window.showMessage(`Can't find exists ${name} list`)
        return
      }
      await session.resume()
    }
  }

  public async doAction(name?: string): Promise<void> {
    let lastSession = this.lastSession
    if (!lastSession) return
    await lastSession.doAction(name)
  }

  public async first(name?: string): Promise<void> {
    let s = this.getSession(name)
    if (s) await s.first()
  }

  public async last(name?: string): Promise<void> {
    let s = this.getSession(name)
    if (s) await s.last()
  }

  public async previous(name?: string): Promise<void> {
    let s = this.getSession(name)
    if (s) await s.previous()
  }

  public async next(name?: string): Promise<void> {
    let s = this.getSession(name)
    if (s) await s.next()
  }

  public getSession(name?: string): ListSession {
    if (!name) return this.session
    return this.sessionsMap.get(name)
  }

  public async cancel(close = true): Promise<void> {
    this.prompt.cancel()
    if (!close) return
    if (this.session) await this.session.hide()
  }

  /**
   * Clear all list sessions
   */
  public reset(): void {
    this.prompt.cancel()
    this.lastSession = undefined
    for (let session of this.sessionsMap.values()) {
      session.dispose()
    }
    this.sessionsMap.clear()
    this.nvim.call('coc#prompt#stop_prompt', ['list'], true)
  }

  public async switchMatcher(): Promise<void> {
    await this.session?.switchMatcher()
  }

  public async togglePreview(): Promise<void> {
    let { nvim } = this
    let winid = await nvim.call('coc#list#get_preview', [0])
    if (winid != -1) {
      await nvim.call('coc#window#close', [winid])
      await nvim.command('redraw')
    } else {
      await this.doAction('preview')
    }
  }

  public async chooseAction(): Promise<void> {
    let { lastSession } = this
    if (lastSession) await lastSession.chooseAction()
  }

  public parseArgs(args: string[]): { list: IList; options: ListOptions; listArgs: string[] } | null {
    let options: string[] = []
    let interactive = false
    let autoPreview = false
    let numberSelect = false
    let noQuit = false
    let first = false
    let reverse = false
    let name: string
    let input = ''
    let matcher: Matcher = 'fuzzy'
    let position = 'bottom'
    let listArgs: string[] = []
    let listOptions: string[] = []
    for (let arg of args) {
      if (!name && arg.startsWith('-')) {
        listOptions.push(arg)
      } else if (!name) {
        if (!/^\w+$/.test(arg)) {
          window.showMessage(`Invalid list option: "${arg}"`, 'error')
          return null
        }
        name = arg
      } else {
        listArgs.push(arg)
      }
    }
    name = name || 'lists'
    let config = workspace.getConfiguration(`list.source.${name}`)
    if (!listOptions.length && !listArgs.length) listOptions = config.get<string[]>('defaultOptions', [])
    if (!listArgs.length) listArgs = config.get<string[]>('defaultArgs', [])
    for (let opt of listOptions) {
      if (opt.startsWith('--input')) {
        input = opt.slice(8)
      } else if (opt == '--number-select' || opt == '-N') {
        numberSelect = true
      } else if (opt == '--auto-preview' || opt == '-A') {
        autoPreview = true
      } else if (opt == '--regex' || opt == '-R') {
        matcher = 'regex'
      } else if (opt == '--strict' || opt == '-S') {
        matcher = 'strict'
      } else if (opt == '--interactive' || opt == '-I') {
        interactive = true
      } else if (opt == '--top') {
        position = 'top'
      } else if (opt == '--tab') {
        position = 'tab'
      } else if (opt == '--ignore-case' || opt == '--normal' || opt == '--no-sort') {
        options.push(opt.slice(2))
      } else if (opt == '--first') {
        first = true
      } else if (opt == '--reverse') {
        reverse = true
      } else if (opt == '--no-quit') {
        noQuit = true
      } else {
        window.showMessage(`Invalid option "${opt}" of list`, 'error')
        return null
      }
    }
    let list = this.listMap.get(name)
    if (!list) {
      window.showMessage(`List ${name} not found`, 'error')
      return null
    }
    if (interactive && !list.interactive) {
      window.showMessage(`Interactive mode of "${name}" list not supported`, 'error')
      return null
    }
    return {
      list,
      listArgs,
      options: {
        numberSelect,
        autoPreview,
        reverse,
        noQuit,
        first,
        input,
        interactive,
        matcher,
        position,
        ignorecase: options.includes('ignore-case') ? true : false,
        mode: !options.includes('normal') ? 'insert' : 'normal',
        sort: !options.includes('no-sort') ? true : false
      },
    }
  }

  private async onInputChar(session: string, ch: string, charmod: number): Promise<void> {
    if (session != 'list') return
    let { mode } = this.prompt
    let now = Date.now()
    if (ch == '<plug>' || (this.plugTs && now - this.plugTs < 20)) {
      this.plugTs = now
      return
    }
    if (!ch) return
    if (ch == '<esc>') {
      await this.cancel()
      return
    }
    if (mode == 'insert') {
      await this.onInsertInput(ch, charmod)
    } else {
      await this.onNormalInput(ch, charmod)
    }
  }

  public async onInsertInput(ch: string, charmod?: number): Promise<void> {
    let { session } = this
    if (!session) return
    if (mouseKeys.includes(ch)) {
      await this.onMouseEvent(ch)
      return
    }
    let n = await session.doNumberSelect(ch)
    if (n) return
    let done = await this.mappings.doInsertKeymap(ch)
    if (done || charmod) return
    if (ch.startsWith('<') && ch.endsWith('>')) {
      await this.feedkeys(ch, false)
      return
    }
    for (let s of ch) {
      let code = s.codePointAt(0)
      if (code == 65533) return
      // exclude control character
      if (code < 32 || code >= 127 && code <= 159) return
      await this.prompt.acceptCharacter(s)
    }
  }

  public async onNormalInput(ch: string, _charmod?: number): Promise<void> {
    if (mouseKeys.includes(ch)) {
      await this.onMouseEvent(ch)
      return
    }
    let used = await this.mappings.doNormalKeymap(ch)
    if (!used) await this.feedkeys(ch)
  }

  private onMouseEvent(key): Promise<void> {
    if (this.session) return this.session.onMouseEvent(key)
  }

  public async feedkeys(key: string, remap = true): Promise<void> {
    let { nvim } = this
    key = key.startsWith('<') && key.endsWith('>') ? `\\${key}` : key
    await nvim.call('coc#prompt#stop_prompt', ['list'])
    await nvim.call('eval', [`feedkeys("${key}", "${remap ? 'i' : 'in'}")`])
    this.prompt.start()
  }

  public async command(command: string): Promise<void> {
    let { nvim } = this
    await nvim.call('coc#prompt#stop_prompt', ['list'])
    await nvim.command(command)
    this.prompt.start()
  }

  public async normal(command: string, bang = true): Promise<void> {
    let { nvim } = this
    await nvim.call('coc#prompt#stop_prompt', ['list'])
    await nvim.command(`normal${bang ? '!' : ''} ${command}`)
    this.prompt.start()
  }

  public async call(fname: string): Promise<any> {
    if (this.session) return await this.session.call(fname)
  }

  public get session(): ListSession | undefined {
    return this.lastSession
  }

  public registerList(list: IList): Disposable {
    let { name } = list
    let exists = this.listMap.get(name)
    if (this.listMap.has(name)) {
      if (exists) {
        if (typeof exists.dispose == 'function') {
          exists.dispose()
        }
        this.listMap.delete(name)
      }
      window.showMessage(`list "${name}" recreated.`)
    }
    this.listMap.set(name, list)
    extensions.addSchemeProperty(`list.source.${name}.defaultAction`, {
      type: 'string',
      default: null,
      description: `Default action of "${name}" list.`
    })
    extensions.addSchemeProperty(`list.source.${name}.defaultOptions`, {
      type: 'array',
      default: list.interactive ? ['--interactive'] : [],
      description: `Default list options of "${name}" list, only used when both list option and argument are empty.`,
      uniqueItems: true,
      items: {
        type: 'string',
        enum: ['--top', '--normal', '--no-sort', '--input', '--tab',
          '--strict', '--regex', '--ignore-case', '--number-select',
          '--interactive', '--auto-preview', '--first', '--no-quit']
      }
    })
    extensions.addSchemeProperty(`list.source.${name}.defaultArgs`, {
      type: 'array',
      default: [],
      description: `Default argument list of "${name}" list, only used when list argument is empty.`,
      uniqueItems: true,
      items: { type: 'string' }
    })
    return Disposable.create(() => {
      if (typeof list.dispose == 'function') {
        list.dispose()
      }
      this.listMap.delete(name)
    })
  }

  public get names(): string[] {
    return Array.from(this.listMap.keys())
  }

  public get descriptions(): { [name: string]: string } {
    let d = {}
    for (let name of this.listMap.keys()) {
      let list = this.listMap.get(name)
      d[name] = list.description
    }
    return d
  }

  /**
   * Get items of {name} list, not work with interactive list.
   *
   * @param {string} name
   * @returns {Promise<any>}
   */
  public async loadItems(name: string): Promise<any> {
    let args = [name]
    let res = this.parseArgs(args)
    if (!res) return
    let { list, options, listArgs } = res
    let source = new CancellationTokenSource()
    let token = source.token
    let arr = await this.nvim.eval('[win_getid(),bufnr("%")]')
    let items = await list.loadItems({
      options,
      args: listArgs,
      input: '',
      cwd: workspace.cwd,
      window: this.nvim.createWindow(arr[0]),
      buffer: this.nvim.createBuffer(arr[1]),
      listWindow: null
    }, token)
    if (!items || Array.isArray(items)) {
      return items
    }
    let task = items as ListTask
    let newItems = await new Promise<ListItem[]>((resolve, reject) => {
      let items = []
      task.on('data', item => {
        item.label = stripAnsi(item.label)
        items.push(item)
      })
      task.on('end', () => {
        resolve(items)
      })
      task.on('error', msg => {
        reject(msg)
      })
    })
    return newItems
  }

  public toggleMode(): void {
    let lastSession = this.lastSession
    if (lastSession) lastSession.toggleMode()
  }

  public get isActivated(): boolean {
    return this.session?.winid != null
  }

  public stop(): void {
    let lastSession = this.lastSession
    if (lastSession) lastSession.stop()
  }

  public dispose(): void {
    for (let session of this.sessionsMap.values()) {
      session.dispose()
    }
    this.sessionsMap.clear()
    if (this.config) {
      this.config.dispose()
    }
    this.lastSession = undefined
    disposeAll(this.disposables)
  }
}

export default new ListManager()
