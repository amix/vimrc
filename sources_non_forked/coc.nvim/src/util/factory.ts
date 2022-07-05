'use strict'
/* eslint-disable @typescript-eslint/no-unsafe-return */
import fs from 'fs'
import path from 'path'
import * as vm from 'vm'
import { defaults } from './lodash'
const createLogger = require('./logger')
const logger = createLogger('util-factoroy')

export interface ExtensionExport {
  activate: (context: unknown) => any
  deactivate: () => any | null
}
export interface ILogger {
  debug: (data: string, ...meta: any[]) => void
  log: (data: string, ...meta: any[]) => void
  error: (data: string, ...meta: any[]) => void
  warn: (data: string, ...meta: any[]) => void
  info: (data: string, ...meta: any[]) => void
}

export interface IModule {
  new(name: string): any
  _resolveFilename: (file: string, context: any) => string
  _extensions: {}
  _cache: { [file: string]: any }
  _compile: () => void
  wrap: (content: string) => string
  require: (file: string) => NodeModule
  _nodeModulePaths: (filename: string) => string[]
}

const Module: IModule = require('module')
const REMOVED_GLOBALS = [
  'reallyExit',
  'abort',
  'umask',
  'setuid',
  'setgid',
  'setgroups',
  '_fatalException',
  'exit',
  'kill',
]

function removedGlobalStub(name: string): Function {
  return () => {
    throw new Error(`process.${name}() is not allowed in extension sandbox`)
  }
}

// @see node/lib/internal/module.js
function makeRequireFunction(this: any): any {
  const req: any = (p: string) => {
    if (p === 'coc.nvim') {
      return require('../index')
    }
    return this.require(p)
  }
  req.resolve = (request: string) => Module._resolveFilename(request, this)
  req.main = process.mainModule
  // Enable support to add extra extension types
  req.extensions = Module._extensions
  req.cache = Module._cache
  return req
}

// @see node/lib/module.js
export function compileInSandbox(sandbox: ISandbox): Function {
  // eslint-disable-next-line
  return function(this: any, content: string, filename: string): any {
    const require = makeRequireFunction.call(this)
    const dirname = path.dirname(filename)
    // remove shebang
    // eslint-disable-next-line
    const newContent = content.replace(/^\#\!.*/, '')
    const wrapper = Module.wrap(newContent)
    const compiledWrapper = vm.runInContext(wrapper, sandbox, { filename })
    const args = [this.exports, require, this, filename, dirname]
    return compiledWrapper.apply(this.exports, args)
  }
}

export interface ISandbox {
  process: NodeJS.Process
  module: NodeModule
  require: (p: string) => any
  console: { [key in keyof Console]?: Function }
  Buffer: any
  Reflect: any
  // eslint-disable-next-line id-blacklist
  String: any
  Promise: any
}

export function createSandbox(filename: string, logger: ILogger): ISandbox {
  const module = new Module(filename)
  module.paths = Module._nodeModulePaths(filename)

  const sandbox = vm.createContext({
    module,
    Buffer,
    console: {
      debug: (...args: any[]) => {
        logger.debug.apply(logger, args)
      },
      log: (...args: any[]) => {
        logger.info.apply(logger, args)
      },
      error: (...args: any[]) => {
        logger.error.apply(logger, args)
      },
      info: (...args: any[]) => {
        logger.info.apply(logger, args)
      },
      warn: (...args: any[]) => {
        logger.warn.apply(logger, args)
      }
    }
  }) as ISandbox

  defaults(sandbox, global)
  sandbox.Reflect = Reflect

  sandbox.require = function sandboxRequire(p): any {
    const oldCompile = Module.prototype._compile
    // Not work on test environment!
    Module.prototype._compile = compileInSandbox(sandbox)
    const moduleExports = sandbox.module.require(p)
    Module.prototype._compile = oldCompile
    return moduleExports
  }

  // patch `require` in sandbox to run loaded module in sandbox context
  // if you need any of these, it might be worth discussing spawning separate processes
  sandbox.process = new (process as any).constructor()
  for (let key of Object.keys(process)) {
    sandbox.process[key] = process[key]
  }

  REMOVED_GLOBALS.forEach(name => {
    sandbox.process[name] = removedGlobalStub(name)
  })
  sandbox.process['chdir'] = () => {}

  // read-only umask
  sandbox.process.umask = (mask?: number) => {
    if (typeof mask !== 'undefined') {
      throw new Error('Cannot use process.umask() to change mask (read-only)')
    }
    return process.umask()
  }

  return sandbox
}

// inspiration drawn from Module
export function createExtension(id: string, filename: string, isEmpty = false): ExtensionExport {
  if (isEmpty || !fs.existsSync(filename)) return {
    activate: () => {},
    deactivate: null
  }
  const sandbox = createSandbox(filename, createLogger(`extension:${id}`))

  delete Module._cache[require.resolve(filename)]

  // attempt to import plugin
  // Require plugin to export activate & deactivate
  const defaultImport = sandbox.require(filename)
  const activate = (defaultImport && defaultImport.activate) || defaultImport

  if (typeof activate !== 'function') {
    return { activate: () => {}, deactivate: null }
  }
  return {
    activate,
    deactivate: typeof defaultImport.deactivate === 'function' ? defaultImport.deactivate : null
  }
}
