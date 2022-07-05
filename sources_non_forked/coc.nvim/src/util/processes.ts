'use strict'
/* ---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/
import * as cp from 'child_process'
import { join, dirname, resolve } from 'path'
import ChildProcess = cp.ChildProcess

declare const ESBUILD

const isWindows = process.platform === 'win32'
const isMacintosh = process.platform === 'darwin'
const isLinux = process.platform === 'linux'
const pluginRoot = typeof ESBUILD === 'undefined' ? resolve(__dirname, '../..') : dirname(__dirname)

export function terminate(process: ChildProcess, cwd?: string): boolean {
  if (process.killed) return
  if (isWindows) {
    try {
      // This we run in Atom execFileSync is available.
      // Ignore stderr since this is otherwise piped to parent.stderr
      // which might be already closed.
      let options: any = {
        stdio: ['pipe', 'pipe', 'ignore']
      }
      if (cwd) {
        options.cwd = cwd
      }
      cp.execFileSync(
        'taskkill',
        ['/T', '/F', '/PID', process.pid.toString()],
        options
      )
      return true
    } catch (err) {
      return false
    }
  } else if (isLinux || isMacintosh) {
    try {
      let filepath = join(pluginRoot, 'bin/terminateProcess.sh')
      let result = cp.spawnSync(filepath, [process.pid.toString()])
      return result.error ? false : true
    } catch (err) {
      return false
    }
  } else {
    process.kill('SIGKILL')
    return true
  }
}
