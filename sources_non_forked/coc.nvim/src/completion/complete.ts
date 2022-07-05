'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationToken, CancellationTokenSource, Emitter, Event, Position } from 'vscode-languageserver-protocol'
import Document from '../model/document'
import { CompleteOption, CompleteResult, ExtendedCompleteItem, FloatConfig, ISource, VimCompleteItem } from '../types'
import { wait } from '../util'
import { getCharCodes } from '../util/fuzzy'
import { byteSlice, characterIndex, isWord } from '../util/string'
import { matchScore } from './match'
import MruLoader from './mru'
const isVim = process.env.VIM_NODE_RPC == '1'
const logger = require('../util/logger')('completion-complete')

export interface CompleteConfig {
  selection: 'none' | 'recentlyUsed' | 'recentlyUsedByPrefix'
  disableKind: boolean
  disableMenu: boolean
  disableMenuShortcut: boolean
  enablePreview: boolean
  enablePreselect: boolean
  labelMaxLength: number
  floatEnable: boolean
  autoTrigger: string
  previewIsKeyword: string
  triggerCompletionWait: number
  minTriggerInputLength: number
  triggerAfterInsertEnter: boolean
  acceptSuggestionOnCommitCharacter: boolean
  noselect: boolean
  keepCompleteopt: boolean
  maxItemCount: number
  timeout: number
  snippetIndicator: string
  fixInsertedWord: boolean
  localityBonus: boolean
  highPrioritySourceLimit: number
  lowPrioritySourceLimit: number
  removeDuplicateItems: boolean
  defaultSortMethod: string
  asciiCharactersOnly: boolean
  ignoreRegexps: string[]
  floatConfig: FloatConfig
}

export type Callback = () => void

export default class Complete {
  // identify this complete
  private results: Map<string, CompleteResult> = new Map()
  private _input = ''
  private _completing = false
  private localBonus: Map<string, number> = new Map()
  // source names that already filtered.
  private filtered: Set<string> = new Set()
  private tokenSource: CancellationTokenSource
  private timer: NodeJS.Timer
  private names: string[] = []
  private readonly _onDidRefresh = new Emitter<void>()
  public readonly onDidRefresh: Event<void> = this._onDidRefresh.event
  constructor(public option: CompleteOption,
    private document: Document,
    private config: CompleteConfig,
    private sources: ISource[],
    private mruLoader: MruLoader,
    private nvim: Neovim) {
    this.tokenSource = new CancellationTokenSource()
    sources.sort((a, b) => b.priority - a.priority)
    this.names = sources.map(o => o.name)
  }

  private fireRefresh(waitTime: number): void {
    if (this.timer) clearTimeout(this.timer)
    this.timer = setTimeout(() => {
      if (this.allFiltered) return
      this._onDidRefresh.fire()
    }, waitTime)
  }

  private get allFiltered(): boolean {
    let { filtered, results } = this
    if (filtered.size === 0) return false
    for (let key of results.keys()) {
      if (!filtered.has(key)) return false
    }
    return true
  }

  public get isCompleting(): boolean {
    return this._completing
  }

  public get input(): string {
    return this._input
  }

  public get isEmpty(): boolean {
    let empty = true
    for (let res of this.results.values()) {
      if (res.items.length > 0) {
        empty = false
        break
      }
    }
    return empty
  }

  public getIncompleteSources(): string[] {
    let names: string[] = []
    for (let [name, result] of this.results.entries()) {
      if (result.isIncomplete) {
        names.push(name)
      }
    }
    return names
  }

  public async doComplete(): Promise<boolean> {
    let token = this.tokenSource.token
    let res = await Promise.all([
      this.nvim.call('coc#util#synname', []),
      this.document.patchChange()
    ])
    this.option.synname = res[0]
    if (token.isCancellationRequested) return true
    let { triggerCompletionWait, localityBonus } = this.config
    await wait(Math.min(triggerCompletionWait ?? 0, 50))
    if (token.isCancellationRequested) return true
    let { colnr, linenr, col } = this.option
    if (localityBonus) {
      let line = linenr - 1
      this.localBonus = this.document.getLocalifyBonus(Position.create(line, col - 1), Position.create(line, colnr))
    }
    await this.completeSources(this.sources)
    return token.isCancellationRequested
  }

  private async completeSources(sources: ReadonlyArray<ISource>): Promise<void> {
    let { fixInsertedWord, timeout } = this.config
    let { results, tokenSource, } = this
    let col = this.option.col
    let isFilter = results.size > 0
    let followPart = !fixInsertedWord ? '' : this.getFollowPart()
    let names = sources.map(s => s.name)
    let total = names.length
    this._completing = true
    let token = tokenSource.token
    let timer: NodeJS.Timer
    let tp = new Promise<void>(resolve => {
      timer = setTimeout(() => {
        if (!tokenSource.token.isCancellationRequested) {
          names = names.filter(n => !finished.includes(n))
          tokenSource.cancel()
          logger.warn(`Complete timeout after ${timeout}ms`, names)
          this.nvim.setVar(`coc_timeout_sources`, names, true)
        }
        resolve()
      }, typeof timeout === 'number' ? timeout : 500)
    })
    const finished: string[] = []
    await Promise.race([
      tp,
      Promise.all(sources.map(s => this.completeSource(s, token, followPart).then(() => {
        finished.push(s.name)
        if (token.isCancellationRequested || isFilter) return
        let colChanged = this.option.col !== col
        if (colChanged) this.cancel()
        if (colChanged || finished.length === total) {
          this.fireRefresh(0)
        } else if (results.has(s.name)) {
          this.fireRefresh(16)
        }
      })))])
    clearTimeout(timer)
    this._completing = false
  }

  private async completeSource(source: ISource, token: CancellationToken, followPart: string): Promise<void> {
    // new option for each source
    let opt = Object.assign({}, this.option)
    let { snippetIndicator } = this.config
    let { name } = source
    try {
      if (typeof source.shouldComplete === 'function') {
        let shouldRun = await Promise.resolve(source.shouldComplete(opt))
        if (!shouldRun || token.isCancellationRequested) return
      }
      const priority = source.priority ?? 0
      const start = Date.now()
      await new Promise<void>((resolve, reject) => {
        Promise.resolve(source.doComplete(opt, token)).then(result => {
          let len = result ? result.items.length : 0
          if (token.isCancellationRequested) {
            resolve(undefined)
            return
          }
          logger.debug(`Source "${name}" finished with ${len} items ${Date.now() - start}ms`)
          if (len > 0) {
            result.priority = priority
            let hasFollow = followPart.length > 0
            result.items.forEach((item, idx) => {
              let word = item.word ?? ''
              let abbr = item.abbr ?? word
              item.word = word
              item.source = name
              item.priority = priority
              item.filterText = item.filterText ?? word
              if (hasFollow && word != followPart && word.endsWith(followPart)) {
                item.word = word.slice(0, - followPart.length)
              }
              if (item.isSnippet === true && !abbr.endsWith(snippetIndicator)) item.abbr = `${abbr}${snippetIndicator}`
              item.localBonus = this.localBonus.get(item.filterText) || 0
              item.user_data = `${name}:${idx}`
            })
            this.setResult(name, result)
          } else {
            this.results.delete(name)
          }
          resolve()
        }, err => {
          reject(err)
        })
      })
    } catch (err) {
      this.nvim.echoError(err)
      logger.error('Complete error:', source.name, err)
    }
  }

  public async completeInComplete(resumeInput: string, names: string[]): Promise<ExtendedCompleteItem[] | undefined> {
    let { document } = this
    this.cancel()
    this.tokenSource = new CancellationTokenSource()
    let token = this.tokenSource.token
    await document.patchChange(true)
    if (token.isCancellationRequested) return undefined
    let { input, colnr, linenr } = this.option
    let character = resumeInput[resumeInput.length - 1]
    Object.assign(this.option, {
      input: resumeInput,
      line: document.getline(linenr - 1),
      colnr: colnr + (resumeInput.length - input.length),
      triggerCharacter: !character || isWord(character) ? undefined : character,
      triggerForInComplete: true
    })
    let sources = this.sources.filter(s => names.includes(s.name))
    await this.completeSources(sources)
    if (token.isCancellationRequested) return undefined
    return this.filterItems(resumeInput)
  }

  public filterItems(input: string): ExtendedCompleteItem[] | undefined {
    let { results, names } = this
    this._input = input
    if (results.size == 0) return []
    let len = input.length
    let emptyInput = len == 0
    let { maxItemCount, selection, enablePreselect, defaultSortMethod, removeDuplicateItems } = this.config
    let arr: ExtendedCompleteItem[] = []
    let codes = getCharCodes(input)
    let words: Set<string> = new Set()
    let maxMru = -1
    let checkMru = selection !== 'none'
    for (let name of names) {
      let result = results.get(name)
      if (!result) continue
      let snippetSource = name === 'snippets'
      let items = result.items
      for (let idx = 0; idx < items.length; idx++) {
        let item = items[idx]
        let { word, filterText, dup } = item
        if (dup !== 1 && words.has(word)) continue
        if (filterText.length < len) continue
        if (removeDuplicateItems && item.isSnippet !== true && words.has(word)) continue
        if (!emptyInput) {
          let score = item.kind && filterText == input ? 64 : matchScore(filterText, codes)
          if (score === 0) continue
          if (snippetSource && word === input) {
            item.score = 99
          } else {
            item.score = score * (item.sourceScore || 1)
          }
        }
        if (checkMru) {
          let n = this.mruLoader.getScore(input, item)
          maxMru = Math.max(n, maxMru)
          item.recentScore = n
        }
        words.add(word)
        arr.push(item)
      }
    }
    arr.sort((a, b) => {
      let sa = a.sortText
      let sb = b.sortText
      if (a.score !== b.score) return b.score - a.score
      if (a.priority !== b.priority) return b.priority - a.priority
      if (a.localBonus !== b.localBonus) return b.localBonus - a.localBonus
      if (a.source === b.source && sa !== sb) return sa < sb ? -1 : 1
      // not sort with empty input
      if (input.length === 0) return 0
      switch (defaultSortMethod) {
        case 'none':
          return 0
        case 'alphabetical':
          return a.filterText.localeCompare(b.filterText)
        case 'length':
        default: // Fallback on length
          return a.filterText.length - b.filterText.length
      }
    })
    let sourceNames = results.keys()
    process.nextTick(() => {
      let { results } = this
      for (let name of sourceNames) {
        let result = results.get(name)
        if (result) result.items = arr.filter(o => o.source === name)
      }
    })
    if (maxMru !== -1) {
      let idx = arr.findIndex(o => o.recentScore === maxMru)
      if (enablePreselect && !isVim) {
        arr[idx].preselect = true
      } else {
        let removed = arr.splice(idx, 1)
        arr.unshift(removed[0])
      }
    }
    return this.limitCompleteItems(arr.slice(0, maxItemCount))
  }

  public async filterResults(input: string): Promise<ExtendedCompleteItem[] | undefined> {
    this.filtered = new Set(this.results.keys())
    if (input !== this.option.input) {
      let names = this.getIncompleteSources()
      if (names.length) {
        return await this.completeInComplete(input, names)
      }
    }
    return this.filterItems(input)
  }

  private limitCompleteItems(items: ExtendedCompleteItem[]): ExtendedCompleteItem[] {
    let { highPrioritySourceLimit, lowPrioritySourceLimit } = this.config
    if (!highPrioritySourceLimit && !lowPrioritySourceLimit) return items
    let counts: Map<string, number> = new Map()
    return items.filter(item => {
      let { priority, source } = item
      let isLow = priority < 90
      let curr = counts.get(source) || 0
      if ((lowPrioritySourceLimit && isLow && curr == lowPrioritySourceLimit)
        || (highPrioritySourceLimit && !isLow && curr == highPrioritySourceLimit)) {
        return false
      }
      counts.set(source, curr + 1)
      return true
    })
  }

  // handle startcol change
  private setResult(name: string, result: CompleteResult): void {
    let { results } = this
    let { line, colnr, col } = this.option
    if (typeof result.startcol === 'number' && result.startcol != col) {
      let { startcol } = result
      this.option.col = startcol
      this.option.input = byteSlice(line, startcol, colnr - 1)
      results.clear()
      results.set(name, result)
    } else {
      results.set(name, result)
    }
  }

  private cancel(): void {
    let { tokenSource, timer } = this
    if (timer) clearTimeout(timer)
    tokenSource.cancel()
    this._completing = false
  }

  public resolveCompletionItem(item: VimCompleteItem | undefined): ExtendedCompleteItem | null {
    if (typeof item.user_data !== 'string') return null
    try {
      let arr = item.user_data.split(':', 2)
      let res = this.results.get(arr[0])
      return res ? res.items.find(o => o.user_data == item.user_data) : null
    } catch (e) {
      return null
    }
  }

  private getFollowPart(): string {
    let { colnr, line } = this.option
    let idx = characterIndex(line, colnr - 1)
    if (idx == line.length) return ''
    let part = line.slice(idx - line.length)
    return part.match(/^\S?[\w-]*/)[0]
  }

  public dispose(): void {
    this.cancel()
    this._onDidRefresh.dispose()
    this.sources = []
    this.filtered.clear()
    this.results.clear()
  }
}
