'use strict'
import TerminalModel, { TerminalOptions } from '../model/terminal'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import { Neovim } from '@chemzqm/neovim'
import { disposeAll } from '../util'
import events from '../events'
const logger = require('../util/logger')('core-terminals')

export default class Terminals {
  private _terminals: Map<number, TerminalModel> = new Map()
  private disposables: Disposable[] = []
  private readonly _onDidOpenTerminal = new Emitter<TerminalModel>()
  private readonly _onDidCloseTerminal = new Emitter<TerminalModel>()
  public readonly onDidCloseTerminal: Event<TerminalModel> = this._onDidCloseTerminal.event
  public readonly onDidOpenTerminal: Event<TerminalModel> = this._onDidOpenTerminal.event

  constructor() {
    events.on('BufUnload', bufnr => {
      if (this._terminals.has(bufnr)) {
        logger.debug('terminal detach', bufnr)
        let terminal = this._terminals.get(bufnr)
        this._onDidCloseTerminal.fire(terminal)
        this._terminals.delete(bufnr)
      }
    }, null, this.disposables)
    events.on('TermExit', (bufnr, status) => {
      let terminal = this._terminals.get(bufnr)
      if (terminal) {
        terminal.onExit(status)
        terminal.dispose()
      }
    }, null, this.disposables)
  }

  public get terminals(): ReadonlyArray<TerminalModel> {
    return Array.from(this._terminals.values())
  }

  public async createTerminal(nvim: Neovim, opts: TerminalOptions): Promise<TerminalModel> {
    let cwd = opts.cwd
    let cmd = opts.shellPath
    let args = opts.shellArgs
    if (!cmd) cmd = await nvim.getOption('shell') as string
    if (!cwd) cwd = await nvim.call('getcwd') as string
    let terminal = new TerminalModel(cmd, args || [], nvim, opts.name, opts.strictEnv)
    await terminal.start(cwd, opts.env)
    this._terminals.set(terminal.bufnr, terminal)
    this._onDidOpenTerminal.fire(terminal)
    return terminal
  }

  public reset(): void {
    for (let terminal of this._terminals.values()) {
      terminal.dispose()
    }
    this._terminals.clear()
  }

  public dispose(): void {
    this._onDidOpenTerminal.dispose()
    this._onDidCloseTerminal.dispose()
    disposeAll(this.disposables)
    this.reset()
  }
}
