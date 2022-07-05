'use strict'
import events, { InsertChange } from '../events'
import { CompleteOption } from '../types'
import { byteSlice } from '../util/string'
const logger = require('../util/logger')('completion-util')

export async function waitInsertEvent(): Promise<string | undefined> {
  let res = await events.race(['InsertLeave', 'CursorMovedI', 'MenuPopupChanged', 'TextChangedI', 'InsertCharPre'], 300)
  return res?.name
}

export async function waitTextChangedI(): Promise<InsertChange | string | undefined> {
  let res = await events.race(['InsertCharPre', 'CursorMoved', 'InsertLeave', 'TextChangedI'], 100)
  if (!res || res.name !== 'TextChangedI') return res ? res.name : undefined
  return res.args[1] as InsertChange
}

export function shouldIndent(indentkeys = '', pretext: string): boolean {
  if (!indentkeys) return false
  for (let part of indentkeys.split(',')) {
    if (part.indexOf('=') > -1) {
      let [pre, post] = part.split('=')
      let word = post.startsWith('~') ? post.slice(1) : post
      if (pretext.length < word.length ||
        (pretext.length > word.length && !/^\s/.test(pretext.slice(-word.length - 1)))) {
        continue
      }
      let matched = post.startsWith('~') ? pretext.toLowerCase().endsWith(word) : pretext.endsWith(word)
      if (!matched) {
        continue
      }
      if (pre == '') {
        return true
      }
      if (pre == '0' && (pretext.length == word.length || /^\s*$/.test(pretext.slice(0, pretext.length - word.length)))) {
        return true
      }
    }
  }
  return false
}

export function shouldStop(bufnr: number, pretext: string, info: InsertChange, option: Pick<CompleteOption, 'bufnr' | 'linenr' | 'line' | 'colnr'>): boolean {
  let { pre } = info
  if (pre.length === 0 || pre[pre.length - 1] === ' ' || pre.length < pretext.length) return true
  if (option.bufnr != bufnr) return true
  let text = byteSlice(option.line, 0, option.colnr - 1)
  if (option.linenr != info.lnum || !pre.startsWith(text)) return true
  return false
}
