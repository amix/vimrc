'use strict'
import { spawn } from 'child_process'
import { EventEmitter } from 'events'
import fs from 'fs-extra'
import { parse, ParseError } from 'jsonc-parser'
import os from 'os'
import path from 'path'
import readline from 'readline'
import semver from 'semver'
import { statAsync } from '../util/fs'
import { omit } from '../util/lodash'
import workspace from '../workspace'
import download from './download'
import fetch from './fetch'
const logger = require('../util/logger')('model-installer')
const HOME_DIR = global.__TEST__ ? os.tmpdir() : os.homedir()

export interface Info {
  'dist.tarball'?: string
  'engines.coc'?: string
  version?: string
  name?: string
}

export type Dependencies = Record<string, string>

export function registryUrl(scope = 'coc.nvim'): string {
  let res = 'https://registry.npmjs.org/'
  let filepath = path.join(HOME_DIR, '.npmrc')
  if (fs.existsSync(filepath)) {
    try {
      let content = fs.readFileSync(filepath, 'utf8')
      let obj = {}
      for (let line of content.split(/\r?\n/)) {
        if (line.indexOf('=') > -1) {
          let [_, key, val] = line.match(/^(.*?)=(.*)$/)
          obj[key] = val
        }
      }
      if (obj[`${scope}:registry`]) {
        res = obj[`${scope}:registry`]
      } else if (obj['registry']) {
        res = obj['registry']
      }
    } catch (e) {
      logger.error('Error on read .npmrc:', e)
    }
  }
  return res.endsWith('/') ? res : res + '/'
}

export function isNpmCommand(exePath: string): boolean {
  let name = path.basename(exePath)
  return name === 'npm' || name === 'npm.CMD'
}

export function isYarn(exePath: string) {
  let name = path.basename(exePath)
  return ['yarn', 'yarn.CMD', 'yarnpkg', 'yarnpkg.CMD'].includes(name)
}

export function getInstallArguments(exePath: string, url: string): string[] {
  let args = ['install', '--ignore-scripts', '--no-lockfile', '--production']
  if (url.startsWith('https://github.com')) {
    args = ['install']
  }
  if (isNpmCommand(exePath)) {
    args.push('--legacy-peer-deps')
    args.push('--no-global')
  }
  if (isYarn(exePath)) {
    args.push('--ignore-engines')
  }
  return args
}

// remove properties that should be devDependencies.
export function getDependencies(content: string): Dependencies {
  let dependencies: Dependencies
  try {
    let obj = JSON.parse(content)
    dependencies = obj.dependencies || {}
  } catch (e) {
    // noop
    dependencies = {}
  }
  return omit(dependencies, ['coc.nvim', 'esbuild', 'webpack', '@types/node'])
}

function isSymbolicLink(folder: string): boolean {
  if (fs.existsSync(folder)) {
    let stat = fs.lstatSync(folder)
    if (stat.isSymbolicLink()) {
      return true
    }
  }
  return false
}

export class Installer extends EventEmitter {
  private name: string
  private url: string
  private version: string
  constructor(
    private root: string,
    private npm: string,
    // could be url or name@version or name
    private def: string
  ) {
    super()
    if (!fs.existsSync(root)) fs.mkdirpSync(root)
    if (/^https?:/.test(def)) {
      this.url = def
    } else {
      let ms = def.match(/(.+)@([^/]+)$/)
      if (ms) {
        this.name = ms[1]
        this.version = ms[2]
      } else {
        this.name = def
      }
    }
  }

  public get info() {
    return { name: this.name, version: this.version }
  }

  public async install(): Promise<string> {
    this.log(`Using npm from: ${this.npm}`)
    let info = await this.getInfo()
    logger.info(`Fetched info of ${this.def}`, info)
    let { name } = info
    let required = info['engines.coc'] ? info['engines.coc'].replace(/^\^/, '>=') : ''
    if (required && !semver.satisfies(workspace.version, required)) {
      throw new Error(`${name} ${info.version} requires coc.nvim >= ${required}, please update coc.nvim.`)
    }
    await this.doInstall(info)
    return name
  }

  public async update(url?: string): Promise<string> {
    this.url = url
    let folder = path.join(this.root, this.name)
    if (isSymbolicLink(folder)) {
      this.log(`Skipped update for symbol link`)
      return
    }
    let version: string
    if (fs.existsSync(path.join(folder, 'package.json'))) {
      let content = await fs.readFile(path.join(folder, 'package.json'), 'utf8')
      version = JSON.parse(content).version
    }
    this.log(`Using npm from: ${this.npm}`)
    let info = await this.getInfo()
    if (version && info.version && semver.gte(version, info.version)) {
      this.log(`Current version ${version} is up to date.`)
      return
    }
    let required = info['engines.coc'] ? info['engines.coc'].replace(/^\^/, '>=') : ''
    if (required && !semver.satisfies(workspace.version, required)) {
      throw new Error(`${info.version} requires coc.nvim ${required}, please update coc.nvim.`)
    }
    await this.doInstall(info)
    let jsonFile = path.join(this.root, info.name, 'package.json')
    this.log(`Updated to v${info.version}`)
    return path.dirname(jsonFile)
  }

  public async doInstall(info: Info): Promise<boolean> {
    let folder = path.join(this.root, info.name)
    if (isSymbolicLink(folder)) return false
    let tmpFolder = await fs.mkdtemp(path.join(os.tmpdir(), `${info.name.replace('/', '-')}-`))
    let url = info['dist.tarball']
    this.log(`Downloading from ${url}`)
    await download(url, { dest: tmpFolder, onProgress: p => this.log(`Download progress ${p}%`, true), extract: 'untar' })
    this.log(`Extension download at ${tmpFolder}`)
    let content = await fs.readFile(path.join(tmpFolder, 'package.json'), 'utf8')
    let dependencies = getDependencies(content)
    if (Object.keys(dependencies).length) {
      let p = new Promise<void>((resolve, reject) => {
        let args = getInstallArguments(this.npm, url)
        this.log(`Installing dependencies by: ${this.npm} ${args.join(' ')}.`)
        const child = spawn(this.npm, args, {
          cwd: tmpFolder,
        })
        const rl = readline.createInterface({
          input: child.stdout
        })
        rl.on('line', line => {
          this.log(`[npm] ${line}`, true)
        })
        child.stderr.setEncoding('utf8')
        child.stdout.setEncoding('utf8')
        child.on('error', reject)
        let err = ''
        child.stderr.on('data', data => {
          err += data
        })
        child.on('exit', code => {
          if (code) {
            if (err) this.log(err)
            reject(new Error(`${this.npm} install exited with ${code}`))
            return
          }
          resolve()
        })
      })
      await p
    }
    let jsonFile = path.resolve(this.root, global.__TEST__ ? '' : '..', 'package.json')
    let errors: ParseError[] = []
    if (!fs.existsSync(jsonFile)) fs.writeFileSync(jsonFile, '{}')
    let obj = parse(fs.readFileSync(jsonFile, 'utf8'), errors, { allowTrailingComma: true })
    if (errors && errors.length > 0) {
      throw new Error(`Error on load ${jsonFile}`)
    }
    obj.dependencies = obj.dependencies || {}
    if (this.url) {
      obj.dependencies[info.name] = this.url
    } else {
      obj.dependencies[info.name] = '>=' + info.version
    }
    const sortedObj = { dependencies: {} }
    Object.keys(obj.dependencies).sort().forEach(k => {
      sortedObj.dependencies[k] = obj.dependencies[k]
    })
    let stat = await statAsync(folder)
    if (stat) {
      if (stat.isDirectory()) {
        fs.removeSync(folder)
      } else {
        fs.unlinkSync(folder)
      }
    }
    await fs.move(tmpFolder, folder, { overwrite: true })
    await fs.writeFile(jsonFile, JSON.stringify(sortedObj, null, 2), { encoding: 'utf8' })
    if (fs.existsSync(tmpFolder)) fs.rmdirSync(tmpFolder)
    this.log(`Update package.json at ${jsonFile}`)
    this.log(`Installed extension ${this.name}@${info.version} at ${folder}`)
    return true
  }

  public async getInfo(): Promise<Info> {
    if (this.url) return await this.getInfoFromUri()
    let registry = registryUrl()
    this.log(`Get info from ${registry}`)
    let buffer = await fetch(registry + this.name, { timeout: 10000, buffer: true })
    let res = JSON.parse(buffer.toString())
    if (!this.version) this.version = res['dist-tags']['latest']
    let obj = res['versions'][this.version]
    if (!obj) throw new Error(`${this.def} doesn't exists in ${registry}.`)
    let requiredVersion = obj['engines'] && obj['engines']['coc']
    if (!requiredVersion) {
      throw new Error(`${this.def} is not valid coc extension, "engines" field with coc property required.`)
    }
    return {
      'dist.tarball': obj['dist']['tarball'],
      'engines.coc': requiredVersion,
      version: obj['version'],
      name: res.name
    } as Info
  }

  public async getInfoFromUri(): Promise<Info> {
    let { url } = this
    if (!url.startsWith('https://github.com')) {
      throw new Error(`"${url}" is not supported, coc.nvim support github.com only`)
    }
    url = url.replace(/\/$/, '')
    let branch = 'master'
    if (url.includes('@')) {
      // https://github.com/sdras/vue-vscode-snippets@main
      let idx = url.indexOf('@')
      branch = url.substr(idx + 1)
      url = url.substring(0, idx)
    }
    let fileUrl = url.replace('github.com', 'raw.githubusercontent.com') + `/${branch}/package.json`
    this.log(`Get info from ${fileUrl}`)
    let content = await fetch(fileUrl, { timeout: 10000 })
    let obj = typeof content == 'string' ? JSON.parse(content) : content
    this.name = obj.name
    return {
      'dist.tarball': `${url}/archive/${branch}.tar.gz`,
      'engines.coc': obj['engines'] ? obj['engines']['coc'] : null,
      name: obj.name,
      version: obj.version
    }
  }

  private log(msg: string, isProgress = false): void {
    logger.info(msg)
    this.emit('message', msg, isProgress)
  }
}

export function createInstallerFactory(npm: string, root: string): (def: string) => Installer {
  return (def): Installer => new Installer(root, npm, def)
}
