'use strict'
import { Neovim } from '@chemzqm/neovim'
const logger = require('../util/logger')('model-terminal')

export interface TerminalOptions {
  /**
   * A human-readable string which will be used to represent the terminal in the UI.
   */
  name?: string

  /**
   * A path to a custom shell executable to be used in the terminal.
   */
  shellPath?: string

  /**
   * Args for the custom shell executable, this does not work on Windows (see #8429)
   */
  shellArgs?: string[]

  /**
   * A path or URI for the current working directory to be used for the terminal.
   */
  cwd?: string

  /**
   * Object with environment variables that will be added to the VS Code process.
   */
  env?: { [key: string]: string | null }

  /**
   * Whether the terminal process environment should be exactly as provided in
   * `TerminalOptions.env`. When this is false (default), the environment will be based on the
   * window's environment and also apply configured platform settings like
   * `terminal.integrated.windows.env` on top. When this is true, the complete environment
   * must be provided as nothing will be inherited from the process or any configuration.
   */
  strictEnv?: boolean
}

export interface TerminalExitStatus {
  code: number | undefined
}

export default class TerminalModel {
  public bufnr: number
  private pid = 0
  public exitStatus: TerminalExitStatus | undefined

  constructor(private cmd: string,
    private args: string[],
    private nvim: Neovim,
    private _name?: string,
    private strictEnv?: boolean
  ) {
  }

  public async start(cwd?: string, env?: { [key: string]: string | null }): Promise<void> {
    let { nvim } = this
    let cmd = [this.cmd, ...this.args]
    let [bufnr, pid] = await nvim.call('coc#terminal#start', [cmd, cwd, env || {}, !!this.strictEnv])
    this.bufnr = bufnr
    this.pid = pid
  }

  public onExit(code: number | undefined): void {
    this.exitStatus = { code: code === -1 ? undefined : code }
  }

  public get name(): string {
    return this._name || this.cmd
  }

  public get processId(): Promise<number> {
    return Promise.resolve(this.pid)
  }

  public sendText(text: string, addNewLine = true): void {
    if (!this.bufnr) return
    this.nvim.call('coc#terminal#send', [this.bufnr, text, addNewLine], true)
  }

  public async show(preserveFocus?: boolean): Promise<boolean> {
    let { bufnr, nvim } = this
    if (!bufnr) return
    let [loaded, winid, curr] = await nvim.eval(`[bufloaded(${bufnr}),bufwinid(${bufnr}),win_getid()]`) as [number, number, number]
    if (!loaded) return false
    if (curr == winid) return true
    nvim.pauseNotification()
    if (winid == -1) {
      nvim.command(`below ${bufnr}sb`, true)
      nvim.command('resize 8', true)
      nvim.call('coc#util#do_autocmd', ['CocTerminalOpen'], true)
    } else {
      nvim.call('win_gotoid', [winid], true)
    }
    nvim.command('normal! G', true)
    if (preserveFocus) {
      nvim.command('wincmd p', true)
    }
    await nvim.resumeNotification()
    return true
  }

  public async hide(): Promise<void> {
    let { bufnr, nvim } = this
    if (!bufnr) return
    await nvim.eval(`coc#window#close(bufwinid(${bufnr}))`)
  }

  public dispose(): void {
    if (!this.exitStatus) {
      this.exitStatus = { code: undefined }
    }
    let { bufnr, nvim } = this
    if (!bufnr) return
    this.bufnr = undefined
    nvim.call('coc#terminal#close', [bufnr], true)
  }
}
