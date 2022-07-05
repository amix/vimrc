'use strict'
import { CancellationToken, Disposable } from 'vscode-languageserver-protocol'
import BufferSync from '../../model/bufferSync'
import { CompleteOption, CompleteResult, ISource } from '../../types'
import { waitImmediate } from '../../util'
import { fuzzyMatch, getCharCodes } from '../../util/fuzzy'
import KeywordsBuffer from '../keywords'
import Source from '../source'
const logger = require('../../util/logger')('sources-around')

export default class Around extends Source {
  constructor(private keywords: BufferSync<KeywordsBuffer>) {
    super({
      name: 'around',
      filepath: __filename
    })
  }

  /**
   * Filter words that too short or doesn't match input
   */
  private async filterWords(words: Set<string>, opt: CompleteOption, token: CancellationToken, res: string[]): Promise<boolean> {
    let isIncomplete = false
    let { input } = opt
    let cword = opt.word
    let first = input[0]
    let fuzzy = input.length > 1
    let min = opt.input.length
    let code = first.charCodeAt(0)
    let ignoreCase = code >= 97 && code <= 122
    let needle = fuzzy ? getCharCodes(input) : []
    let checkInput = true
    let checkCword = true
    let ts = Date.now()
    for (let word of words) {
      let len = word.length
      if (len < min) continue
      if (checkInput && len == min && word === input) {
        checkInput = false
        continue
      }
      if (checkCword && len == cword.length && word === cword) {
        checkCword = false
        continue
      }
      if (Date.now() - ts > 15) {
        await waitImmediate()
        if (token.isCancellationRequested) return undefined
        ts = Date.now()
      }
      let ch = ignoreCase ? word[0].toLowerCase() : word[0]
      if (fuzzy) {
        if (ch.charCodeAt(0) === code && fuzzyMatch(needle, word)) {
          res.push(word)
          if (res.length == 100) {
            isIncomplete = true
            break
          }
        }
      } else {
        if (ch.charCodeAt(0) === code) {
          res.push(word)
          if (res.length == 100) {
            isIncomplete = true
            break
          }
        }
      }
    }
    return isIncomplete
  }

  public async doComplete(opt: CompleteOption, token: CancellationToken): Promise<CompleteResult> {
    let { bufnr, input } = opt
    if (input.length === 0) return null
    await waitImmediate()
    if (token.isCancellationRequested) return null
    let item = this.keywords.getItem(bufnr)
    let words = item?.words
    if (!words) return null
    let arr = []
    let isIncomplete = await this.filterWords(words, opt, token, arr)
    if (token.isCancellationRequested) return null
    return {
      isIncomplete,
      items: arr.map(word => ({
        word,
        menu: this.menu
      }))
    }
  }
}

export function regist(sourceMap: Map<string, ISource>, keywords: BufferSync<KeywordsBuffer>): Disposable {
  sourceMap.set('around', new Around(keywords))
  return Disposable.create(() => {
    sourceMap.delete('around')
  })
}
