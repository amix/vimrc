'use strict'
import path from 'path'
import fs from 'fs'
import { executable, runCommand } from '../util'
import { statAsync } from '../util/fs'
import stripAnsi from 'strip-ansi'
const logger = require('../util/logger')('model-resolver')

export default class Resolver {
  private _npmFolder: string
  private _yarnFolder: string

  private get nodeFolder(): Promise<string> {
    if (!executable('npm')) return Promise.resolve('')
    if (this._npmFolder) return Promise.resolve(this._npmFolder)
    return runCommand('npm --loglevel silent root -g', {}, 3000).then(root => {
      this._npmFolder = stripAnsi(root).trim()
      return this._npmFolder
    })
  }

  private get yarnFolder(): Promise<string> {
    if (!executable('yarnpkg')) return Promise.resolve('')
    if (this._yarnFolder) return Promise.resolve(this._yarnFolder)
    return runCommand('yarnpkg global dir', {}, 3000).then(root => {
      let folder = path.join(stripAnsi(root).trim(), 'node_modules')
      let exists = fs.existsSync(folder)
      if (exists) this._yarnFolder = folder
      return exists ? folder : ''
    })
  }

  public async resolveModule(mod: string): Promise<string> {
    let nodeFolder = await this.nodeFolder
    let yarnFolder = await this.yarnFolder
    if (yarnFolder) {
      let s = await statAsync(path.join(yarnFolder, mod, 'package.json'))
      if (s && s.isFile()) return path.join(yarnFolder, mod)
    }
    if (nodeFolder) {
      let s = await statAsync(path.join(nodeFolder, mod, 'package.json'))
      if (s && s.isFile()) return path.join(nodeFolder, mod)
    }
    return null
  }
}
