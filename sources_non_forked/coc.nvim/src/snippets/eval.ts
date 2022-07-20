'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Range } from '@chemzqm/neovim/lib/types'
import { exec } from 'child_process'
import { promisify } from 'util'
export type EvalKind = 'vim' | 'python' | 'shell'
const logger = require('../util/logger')('snippets-eval')
const isVim = process.env.VIM_NODE_RPC == '1'

export interface UltiSnippetContext {
  /**
   * line on insert
   */
  line: string
  /**
   * Range to replace, start.line should equal end.line
   */
  range: Range
  /**
   * Context python code.
   */
  context?: string
  /**
   * Regex trigger (python code)
   */
  regex?: string
  /**
   * Avoid python code eval when is true.
   */
  noPython?: boolean
}

/**
 * Eval code for code placeholder.
 */
export async function evalCode(nvim: Neovim, kind: EvalKind, code: string, curr = ''): Promise<string> {
  if (kind == 'vim') {
    let res = await nvim.eval(code)
    return res.toString()
  }

  if (kind == 'shell') {
    let res = await promisify(exec)(code)
    return res.stdout.replace(/\s*$/, '') || res.stderr
  }

  let lines = [`snip._reset("${escapeString(curr)}")`]
  lines.push(...code.split(/\r?\n/).map(line => line.replace(/\t/g, '    ')))
  await executePythonCode(nvim, lines)
  return await nvim.call(`pyxeval`, 'str(snip.rv)') as string
}

export function prepareMatchCode(snip: UltiSnippetContext): string {
  let { range, regex, line } = snip
  let pyCodes: string[] = []
  if (regex && range != null) {
    let trigger = line.slice(range.start.character, range.end.character)
    pyCodes.push(`pattern = re.compile("${escapeString(regex)}")`)
    pyCodes.push(`match = pattern.search("${escapeString(trigger)}")`)
  } else {
    pyCodes.push(`match = None`)
  }
  return pyCodes.join('\n')
}

export function preparePythonCodes(snip: UltiSnippetContext): string[] {
  let { range, context, line } = snip
  let pyCodes: string[] = [
    'import re, os, vim, string, random',
    `path = vim.eval('expand("%:p")') or ""`,
    `fn = os.path.basename(path)`,
  ]
  if (context) {
    pyCodes.push(`snip = ContextSnippet()`)
    pyCodes.push(`context = ${context}`)
  } else {
    pyCodes.push(`context = True`)
  }
  let start = `(${range.start.line},${Buffer.byteLength(line.slice(0, range.start.character))})`
  let end = `(${range.start.line},${Buffer.byteLength(line.slice(0, range.end.character))})`
  let indent = line.match(/^\s*/)[0]
  pyCodes.push(`snip = SnippetUtil("${escapeString(indent)}", ${start}, ${end}, context)`)
  return pyCodes
}

export async function executePythonCode(nvim: Neovim, codes: string[]) {
  try {
    await nvim.command(`pyx ${addPythonTryCatch(codes.join('\n'))}`)
  } catch (e) {
    let err = new Error(e instanceof Error ? e.message : e.toString())
    err.stack = `Error on execute python code:\n${codes.join('\n')}\n` + (e instanceof Error ? e.stack : e)
    throw err
  }
}

export function getVariablesCode(values: { [index: number]: string }): string {
  let keys = Object.keys(values)
  let maxIndex = keys.length ? Math.max.apply(null, keys.map(v => Number(v))) : 0
  let vals = (new Array(maxIndex)).fill('""')
  for (let [idx, val] of Object.entries(values)) {
    vals[idx] = `"${escapeString(val)}"`
  }
  return `t = (${vals.join(',')},)`
}

/**
 * vim8 doesn't throw any python error with :py command
 * we have to use g:errmsg since v:errmsg can't be changed in python script.
 */
export function addPythonTryCatch(code: string, force = false): string {
  if (!isVim && force === false) return code
  let lines = [
    'import traceback, vim',
    `vim.vars['errmsg'] = ''`,
    'try:',
  ]
  lines.push(...code.split('\n').map(line => '    ' + line))
  lines.push('except Exception as e:')
  lines.push(`    vim.vars['errmsg'] = traceback.format_exc()`)
  return lines.join('\n')
}

function escapeString(input: string): string {
  return input
    .replace(/\\/g, '\\\\')
    .replace(/"/g, '\\"')
    .replace(/\t/g, '\\t')
    .replace(/\n/g, '\\n')
}

const stringStartRe = /\\A/
const conditionRe = /\(\?\(\w+\).+\|/
const commentRe = /\(\?#.*?\)/
const namedCaptureRe = /\(\?P<\w+>.*?\)/
const namedReferenceRe = /\(\?P=(\w+)\)/
const regex = new RegExp(`${commentRe.source}|${stringStartRe.source}|${namedCaptureRe.source}|${namedReferenceRe.source}`, 'g')

/**
 * Convert python regex to javascript regex,
 * throw error when unsupported pattern found
 */
export function convertRegex(str: string): string {
  if (str.indexOf('\\z') !== -1) {
    throw new Error('pattern \\z not supported')
  }
  if (str.indexOf('(?s)') !== -1) {
    throw new Error('pattern (?s) not supported')
  }
  if (str.indexOf('(?x)') !== -1) {
    throw new Error('pattern (?x) not supported')
  }
  if (str.indexOf('\n') !== -1) {
    throw new Error('pattern \\n not supported')
  }
  if (conditionRe.test(str)) {
    throw new Error('pattern (?id/name)yes-pattern|no-pattern not supported')
  }
  return str.replace(regex, (match, p1) => {
    if (match == '\\A') return '^'
    if (match.startsWith('(?#')) return ''
    if (match.startsWith('(?P<')) return '(?' + match.slice(3)
    if (match.startsWith('(?P=')) return `\\k<${p1}>`
    return ''
  })
}
