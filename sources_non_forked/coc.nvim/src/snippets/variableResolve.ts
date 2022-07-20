'use strict'
import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { Variable, VariableResolver } from "./parser"
import WorkspaceFolderController from '../core/workspaceFolder'
import { v4 as uuid } from 'uuid'
import { URI } from 'vscode-uri'
const logger = require('../util/logger')('snippets-variable')

function padZero(n: number): string {
  return n < 10 ? '0' + n : n.toString()
}

export function parseComments(comments: string): { start?: string, end?: string, single?: string } {
  let start: string
  let end: string
  let single: string
  let parts = comments.split(',')
  for (let s of parts) {
    if (start && end && single) break
    if (!s.includes(':')) continue
    let [flag, str] = s.split(':')
    if (flag.includes('s')) {
      start = str
    } else if (flag.includes('e')) {
      end = str
    } else if (!single && flag == '') {
      single = str
    }
  }
  return { start, end, single }
}

/*
 * Get single line comment text
 */
export function parseCommentstring(commentstring: string): string | undefined {
  if (commentstring.endsWith('%s')) return commentstring.slice(0, -2).trim()
  return undefined
}

export class SnippetVariableResolver implements VariableResolver {
  private _variableToValue: { [key: string]: string } = {}

  constructor(private nvim: Neovim, private workspaceFolder: WorkspaceFolderController) {
    const currentDate = new Date()
    const fullyear = currentDate.getFullYear().toString()
    Object.assign(this._variableToValue, {
      CURRENT_YEAR: fullyear,
      CURRENT_YEAR_SHORT: fullyear.slice(-2),
      CURRENT_MONTH: padZero(currentDate.getMonth() + 1),
      CURRENT_DATE: padZero(currentDate.getDate()),
      CURRENT_HOUR: padZero(currentDate.getHours()),
      CURRENT_MINUTE: padZero(currentDate.getMinutes()),
      CURRENT_SECOND: padZero(currentDate.getSeconds()),
      CURRENT_DAY_NAME: currentDate.toLocaleString("en-US", { weekday: "long" }),
      CURRENT_DAY_NAME_SHORT: currentDate.toLocaleString("en-US", { weekday: "short" }),
      CURRENT_MONTH_NAME: currentDate.toLocaleString("en-US", { month: "long" }),
      CURRENT_MONTH_NAME_SHORT: currentDate.toLocaleString("en-US", { month: "short" }),
      TM_FILENAME: null,
      TM_FILENAME_BASE: null,
      TM_DIRECTORY: null,
      TM_FILEPATH: null,
      YANK: null,
      TM_LINE_INDEX: null,
      TM_LINE_NUMBER: null,
      TM_CURRENT_LINE: null,
      TM_CURRENT_WORD: null,
      TM_SELECTED_TEXT: null,
      VISUAL: null,
      CLIPBOARD: null,
      RELATIVE_FILEPATH: null,
      RANDOM: null,
      RANDOM_HEX: null,
      UUID: null,
      BLOCK_COMMENT_START: null,
      BLOCK_COMMENT_END: null,
      LINE_COMMENT: null,
      WORKSPACE_NAME: null,
      WORKSPACE_FOLDER: null
    })
  }

  private async resolveValue(name: string): Promise<string | undefined> {
    let { nvim } = this
    if (['TM_FILENAME', 'TM_FILENAME_BASE', 'TM_DIRECTORY', 'TM_FILEPATH'].includes(name)) {
      let filepath = await nvim.eval('expand("%:p")') as string
      if (name === 'TM_FILENAME') return path.basename(filepath)
      if (name === 'TM_FILENAME_BASE') return path.basename(filepath, path.extname(filepath))
      if (name === 'TM_DIRECTORY') return path.dirname(filepath)
      if (name === 'TM_FILEPATH') return filepath
    }
    if (name === 'YANK') {
      return await nvim.call('getreg', ['""']) as string
    }
    if (name === 'TM_LINE_INDEX') {
      let lnum = await nvim.call('line', ['.']) as number
      return (lnum - 1).toString()
    }
    if (name === 'TM_LINE_NUMBER') {
      let lnum = await nvim.call('line', ['.']) as number
      return lnum.toString()
    }
    if (name === 'TM_CURRENT_LINE') {
      return await nvim.call('getline', ['.']) as string
    }
    if (name === 'TM_CURRENT_WORD') {
      return await nvim.eval(`expand('<cword>')`) as string
    }
    if (name === 'TM_SELECTED_TEXT' || name == 'VISUAL') {
      return await nvim.eval(`get(g:,'coc_selected_text', v:null)`) as string
    }
    if (name === 'CLIPBOARD') {
      return await nvim.eval('@*') as string
    }
    if (name === 'RANDOM') {
      return Math.random().toString().slice(-6)
    }
    if (name === 'RANDOM_HEX') {
      return Math.random().toString(16).slice(-6)
    }
    if (name === 'UUID') {
      return uuid()
    }
    if (['RELATIVE_FILEPATH', 'WORKSPACE_NAME', 'WORKSPACE_FOLDER'].includes(name)) {
      let filepath = await nvim.eval('expand("%:p")') as string
      let folder = this.workspaceFolder.getWorkspaceFolder(URI.file(filepath))
      if (name === 'RELATIVE_FILEPATH') return this.workspaceFolder.getRelativePath(filepath)
      if (name === 'WORKSPACE_NAME') return folder.name
      if (name === 'WORKSPACE_FOLDER') return URI.parse(folder.uri).fsPath
    }
    if (name === 'LINE_COMMENT') {
      let commentstring = await nvim.eval('&commentstring') as string
      let s = parseCommentstring(commentstring)
      if (s) return s
      let comments = await nvim.eval('&comments') as string
      let { single } = parseComments(comments)
      return single ?? ''
    }
    if (['BLOCK_COMMENT_START', 'BLOCK_COMMENT_END'].includes(name)) {
      let comments = await nvim.eval('&comments') as string
      let { start, end } = parseComments(comments)
      if (name === 'BLOCK_COMMENT_START') return start ?? ''
      if (name === 'BLOCK_COMMENT_END') return end ?? ''
    }
  }

  public async resolve(variable: Variable): Promise<string> {
    const name = variable.name
    let resolved = this._variableToValue[name]
    if (resolved != null) return resolved.toString()
    // resolve known value
    if (this._variableToValue.hasOwnProperty(name)) {
      let value = await this.resolveValue(name)
      if (!value && variable.children.length) {
        return variable.toString()
      }
      return value == null ? '' : value.toString()
    }
    if (variable.children.length) return variable.toString()
    // VSCode behavior
    return name
  }
}
