'use strict'
import { CancellationToken, Disposable } from 'vscode-languageserver-protocol'
import BufferSync from '../../model/bufferSync'
import { CompleteOption, CompleteResult, ISource } from '../../types'
import { waitImmediate } from '../../util'
import { fuzzyMatch, getCharCodes } from '../../util/fuzzy'
import KeywordsBuffer from '../keywords'
import Source from '../source'
const logger = require('../../util/logger')('sources-buffer')

export default class Buffer extends Source {
  constructor(private keywords: BufferSync<KeywordsBuffer>) {
    super({
      name: 'buffer',
      filepath: __filename
    })
  }

  public get ignoreGitignore(): boolean {
    return this.getConfig('ignoreGitignore', true)
  }

  private async getWords(bufnr: number, opt: CompleteOption, token: CancellationToken, words: Set<string>): Promise<boolean> {
    let { ignoreGitignore } = this
    let isIncomplete = false
    let first = opt.input[0]
    let min = opt.input.length
    let fuzzy = min > 1
    let code = first.charCodeAt(0)
    let ignoreCase = code >= 97 && code <= 122
    let needle = fuzzy ? getCharCodes(opt.input) : []
    let ts = Date.now()
    for (let item of this.keywords.items) {
      if (words.size == 100) break
      if (item.bufnr === bufnr || (ignoreGitignore && item.gitIgnored)) continue
      for (let w of item.words) {
        if (Date.now() - ts > 15) {
          await waitImmediate()
          if (token.isCancellationRequested) return undefined
          ts = Date.now()
        }
        if (w.length < min) continue
        let ch = ignoreCase ? w[0].toLowerCase() : w[0]
        if (fuzzy) {
          if (ch.charCodeAt(0) === code && fuzzyMatch(needle, w)) {
            words.add(w)
            if (words.size == 100) {
              isIncomplete = true
              break
            }
          }
        } else {
          if (ch.charCodeAt(0) === code) {
            words.add(w)
            if (words.size == 100) {
              isIncomplete = true
              break
            }
          }
        }
      }
    }
    return isIncomplete
  }

  public async doComplete(opt: CompleteOption, token: CancellationToken): Promise<CompleteResult> {
    let { bufnr, input } = opt
    if (input.length == 0) return null
    await waitImmediate()
    if (token.isCancellationRequested) return null
    let words: Set<string> = new Set()
    let isIncomplete = await this.getWords(bufnr, opt, token, words)
    return {
      isIncomplete,
      items: Array.from(words).map(word => ({
        word,
        menu: this.menu
      }))
    }
  }
}

export function regist(sourceMap: Map<string, ISource>, keywords: BufferSync<KeywordsBuffer>): Disposable {
  sourceMap.set('buffer', new Buffer(keywords))
  return Disposable.create(() => {
    sourceMap.delete('buffer')
  })
}
