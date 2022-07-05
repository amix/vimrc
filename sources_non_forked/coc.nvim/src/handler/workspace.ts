'use strict'
import { Neovim } from '@chemzqm/neovim'
import { URI } from 'vscode-uri'
import fs from 'fs'
import path from 'path'
import extensions from '../extensions'
import { HandlerDelegate, PatternType, WorkspaceConfiguration } from '../types'
import workspace from '../workspace'
import window from '../window'
import snippetManager from '../snippets/manager'
const logger = require('../util/logger')('handler-workspace')
declare const REVISION

interface RootPatterns {
  buffer: string[]
  server: string[]
  global: string[]
}

export default class WorkspaceHandler {
  constructor(
    private nvim: Neovim,
    private handler: HandlerDelegate
  ) {
  }

  public async openLog(): Promise<void> {
    let file = logger.logfile
    await workspace.jumpTo(URI.file(file).toString())
  }

  public async doAutocmd(id: number, args: any[]): Promise<void> {
    await workspace.autocmds.doAutocmd(id, args)
  }

  public async getConfiguration(key: string): Promise<WorkspaceConfiguration> {
    let document = await workspace.document
    return workspace.getConfiguration(key, document ? document.uri : undefined)
  }

  public getRootPatterns(bufnr: number): RootPatterns | null {
    let doc = workspace.getDocument(bufnr)
    if (!doc) return null
    return {
      buffer: workspace.workspaceFolderControl.getRootPatterns(doc, PatternType.Buffer),
      server: workspace.workspaceFolderControl.getRootPatterns(doc, PatternType.LanguageServer) || [],
      global: workspace.workspaceFolderControl.getRootPatterns(doc, PatternType.Global)
    }
  }

  public async ensureDocument(): Promise<boolean> {
    let doc = await workspace.document
    return doc && !doc.isCommandLine && doc.attached
  }

  public async doKeymap(key: string, defaultReturn = '', pressed?: string): Promise<string> {
    return await workspace.keymaps.doKeymap(key, defaultReturn, pressed)
  }

  public async snippetCheck(checkExpand: boolean, checkJump: boolean): Promise<boolean> {
    if (checkExpand && !extensions.has('coc-snippets')) {
      this.nvim.echoError('coc-snippets required for check expand status!')
      return false
    }
    if (checkJump) {
      let jumpable = snippetManager.jumpable()
      if (jumpable) return true
    }
    if (checkExpand) {
      let api = extensions.getExtensionApi('coc-snippets') as any
      if (api && api.hasOwnProperty('expandable')) {
        let expandable = await Promise.resolve(api.expandable())
        if (expandable) return true
      }
    }
    return false
  }

  public async showInfo(): Promise<void> {
    let lines: string[] = []
    let version = workspace.version + (typeof REVISION === 'string' ? '-' + REVISION : '')
    lines.push('## versions')
    lines.push('')
    let out = await this.nvim.call('execute', ['version']) as string
    let first = out.trim().split(/\r?\n/, 2)[0].replace(/\(.*\)/, '').trim()
    lines.push('vim version: ' + first + `${workspace.isVim ? ' ' + workspace.env.version : ''}`)
    lines.push('node version: ' + process.version)
    lines.push('coc.nvim version: ' + version)
    lines.push('coc.nvim directory: ' + path.dirname(__dirname))
    lines.push('term: ' + (process.env.TERM_PROGRAM || process.env.TERM))
    lines.push('platform: ' + process.platform)
    lines.push('')
    lines.push('## Log of coc.nvim')
    lines.push('')
    let file = logger.logfile
    if (fs.existsSync(file)) {
      let content = fs.readFileSync(file, { encoding: 'utf8' })
      lines.push(...content.split(/\r?\n/))
    }
    await this.nvim.command('vnew +setl\\ buftype=nofile\\ bufhidden=wipe\\ nobuflisted')
    let buf = await this.nvim.buffer
    await buf.setLines(lines, { start: 0, end: -1, strictIndexing: false })
  }
}
