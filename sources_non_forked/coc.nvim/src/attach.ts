'use strict'
import { attach, Attach, NeovimClient } from '@chemzqm/neovim'
import log4js from 'log4js'
import events from './events'
import Plugin from './plugin'
import semver from 'semver'
import { objectLiteral } from './util/is'
import { URI } from 'vscode-uri'
import { version as VERSION } from '../package.json'

const logger = require('./util/logger')('attach')
const isTest = global.hasOwnProperty('__TEST__')
/**
 * Request actions that not need plugin ready
 */
const ACTIONS_NO_WAIT = ['installExtensions', 'updateExtensions']

export default (opts: Attach, requestApi = true): Plugin => {
  const nvim: NeovimClient = attach(opts, log4js.getLogger('node-client'), requestApi)
  if (!global.hasOwnProperty('__TEST__')) {
    nvim.call('coc#util#path_replace_patterns').then(prefixes => {
      if (objectLiteral(prefixes)) {
        const old_uri = URI.file
        URI.file = (path): URI => {
          path = path.replace(/\\/g, '/')
          Object.keys(prefixes).forEach(k => path = path.replace(new RegExp('^' + k), prefixes[k]))
          return old_uri(path)
        }
      }
    }).logError()
  }
  nvim.setVar('coc_process_pid', process.pid, true)
  const plugin = new Plugin(nvim)
  let clientReady = false
  let initialized = false
  nvim.on('notification', async (method, args) => {
    switch (method) {
      case 'VimEnter': {
        if (!initialized && clientReady) {
          initialized = true
          await plugin.init()
        }
        break
      }
      case 'Log': {
        logger.debug(...args)
        break
      }
      case 'TaskExit':
      case 'TaskStderr':
      case 'TaskStdout':
      case 'GlobalChange':
      case 'PromptInsert':
      case 'InputChar':
      case 'MenuInput':
      case 'OptionSet':
      case 'PromptKeyPress':
      case 'FloatBtnClick':
        logger.trace('Event: ', method, ...args)
        await events.fire(method, args)
        break
      case 'CocAutocmd':
        logger.trace('Notification autocmd:', ...args)
        await events.fire(args[0], args.slice(1))
        break
      case 'redraw':
        break
      default: {
        let exists = plugin.hasAction(method)
        if (!exists) {
          console.error(`action "${method}" does not exist`)
          return
        }
        try {
          if (!plugin.isReady) {
            logger.warn(`Plugin not ready when received "${method}"`, args)
          } else {
            logger.info('receive notification:', method, args)
          }
          await plugin.ready
          await plugin.cocAction(method, ...args)
        } catch (e) {
          nvim.echoError(`Error on notification "${method}": ${(e instanceof Error ? e.message : e)}`)
          logger.error(e)
        }
      }
    }
  })

  nvim.on('request', async (method: string, args, resp) => {
    if (method == 'redraw') {
      // ignore redraw from neovim
      resp.send()
      return
    }
    let timer = setTimeout(() => {
      logger.error('Request cost more than 3s', method, args)
    }, 3000)
    try {
      if (method == 'CocAutocmd') {
        logger.trace('Request autocmd:', ...args)
        await events.fire(args[0], args.slice(1))
        resp.send(undefined)
      } else {
        if (!plugin.isReady && !ACTIONS_NO_WAIT.includes(method)) {
          logger.warn(`Plugin not ready on request "${method}"`, args)
          resp.send('Plugin not ready', true)
          return
        }
        logger.info('Request action:', method, args)
        let res = await plugin.cocAction(method, ...args)
        resp.send(res)
      }
      clearTimeout(timer)
    } catch (e) {
      clearTimeout(timer)
      resp.send(e instanceof Error ? e.message : e.toString(), true)
      logger.error(`Request error:`, method, args, e)
    }
  })

  nvim.channelId.then(async channelId => {
    clientReady = true
    // Used for test client on vim side
    if (isTest) nvim.call('coc#rpc#set_channel', [channelId], true)
    let { major, minor, patch } = semver.parse(VERSION)
    nvim.setClientInfo('coc', { major, minor, patch }, 'remote', {}, {})
    let entered = await nvim.getVvar('vim_did_enter')
    if (entered && !initialized) {
      initialized = true
      await plugin.init()
    }
  }).catch(e => {
    console.error(`Channel create error: ${e.message}`)
  })
  return plugin
}
