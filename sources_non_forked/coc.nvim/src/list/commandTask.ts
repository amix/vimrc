'use strict'
import { spawn } from 'child_process'
import { EventEmitter } from 'events'
import readline from 'readline'
import { Disposable } from 'vscode-languageserver-protocol'
import { ListItem, ListTask } from '../types'
import { disposeAll } from '../util'
import workspace from '../workspace'
const logger = require('../util/logger')('list-commandTask')

export interface CommandTaskOption {
  /**
   * Command to run.
   */
  cmd: string
  /**
   * Arguments of command.
   */
  args: string[]
  cwd?: string
  env?: NodeJS.ProcessEnv
  /**
   * Runs for each line, return undefined for invalid item.
   */
  onLine: (line: string) => ListItem | undefined
}

export default class CommandTask extends EventEmitter implements ListTask {
  private disposables: Disposable[] = []
  constructor(private opt: CommandTaskOption) {
    super()
    this.start()
  }

  private start(): void {
    let { cmd, args, cwd, onLine } = this.opt
    let proc = spawn(cmd, args, { cwd: cwd || workspace.cwd, windowsHide: true })
    this.disposables.push({
      dispose: () => {
        proc.kill()
      }
    })
    proc.on('error', e => {
      this.emit('error', e.message)
    })
    proc.stderr.on('data', chunk => {
      logger.error(`[${cmd} Error]`, chunk.toString('utf8'))
    })
    const rl = readline.createInterface(proc.stdout)
    rl.on('line', line => {
      let res = onLine(line)
      if (res) this.emit('data', res)
    })
    rl.on('close', () => {
      this.emit('end')
    })
  }

  public dispose(): void {
    disposeAll(this.disposables)
  }
}
