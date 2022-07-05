'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationToken, CancellationTokenSource, Emitter, Event } from 'vscode-languageserver-protocol'
import { IList, ListContext, ListHighlights, ListItem, ListItemsEvent, ListItemWithHighlights, ListOptions, ListTask } from '../types'
import { parseAnsiHighlights } from '../util/ansiparse'
import { filter } from '../util/async'
import { patchLine } from '../util/diff'
import { hasMatch, positions, score } from '../util/fzy'
import { Mutex } from '../util/mutex'
import { getMatchResult } from '../util/score'
import { byteIndex, byteLength } from '../util/string'
import Prompt from './prompt'
const logger = require('../util/logger')('list-worker')
const controlCode = '\x1b'

export interface WorkerConfiguration {
  interactiveDebounceTime: number
  extendedSearchMode: boolean
}

export interface FilterOption {
  append?: boolean
  reload?: boolean
}

export type OnFilter = (arr: ListItemWithHighlights[], finished: boolean, sort?: boolean) => void

// perform loading task
export default class Worker {
  private _loading = false
  private _finished = false
  private mutex: Mutex = new Mutex()
  private filteredCount: number
  private totalItems: ListItem[] = []
  private tokenSource: CancellationTokenSource
  private filterTokenSource: CancellationTokenSource
  private _onDidChangeItems = new Emitter<ListItemsEvent>()
  private _onDidChangeLoading = new Emitter<boolean>()
  public readonly onDidChangeItems: Event<ListItemsEvent> = this._onDidChangeItems.event
  public readonly onDidChangeLoading: Event<boolean> = this._onDidChangeLoading.event

  constructor(
    private nvim: Neovim,
    private list: IList,
    private prompt: Prompt,
    private listOptions: ListOptions,
    private config: WorkerConfiguration
  ) {
  }

  private set loading(loading: boolean) {
    if (this._loading == loading) return
    this._loading = loading
    this._onDidChangeLoading.fire(loading)
  }

  public get isLoading(): boolean {
    return this._loading
  }

  public async loadItems(context: ListContext, reload = false): Promise<void> {
    this.cancelFilter()
    this.filteredCount = 0
    this._finished = false
    let { list, listOptions } = this
    this.loading = true
    let { interactive } = listOptions
    this.tokenSource = new CancellationTokenSource()
    let token = this.tokenSource.token
    let items = await list.loadItems(context, token)
    if (token.isCancellationRequested) return
    items = items ?? []
    if (Array.isArray(items)) {
      this.tokenSource = null
      this.totalItems = items
      this.loading = false
      this._finished = true
      let filtered: ListItemWithHighlights[]
      if (!interactive) {
        let tokenSource = this.filterTokenSource = new CancellationTokenSource()
        await this.mutex.use(async () => {
          let token = tokenSource.token
          if (token.isCancellationRequested) return
          await this.filterItems(items as ListItem[], { reload }, token)
        })
      } else {
        filtered = this.convertToHighlightItems(items)
        this._onDidChangeItems.fire({
          items: filtered,
          reload,
          finished: true
        })
      }
    } else {
      let task = items as ListTask
      let totalItems = this.totalItems = []
      let taken = 0
      let currInput = context.input
      let filtering = false
      this.filterTokenSource = new CancellationTokenSource()
      let _onData = async (finished?: boolean) => {
        filtering = true
        await this.mutex.use(async () => {
          let inputChanged = this.input != currInput
          if (inputChanged) {
            currInput = this.input
            taken = this.filteredCount ?? 0
          }
          if (taken >= totalItems.length) return
          let append = taken > 0
          let remain = totalItems.slice(taken)
          taken = totalItems.length
          if (!interactive) {
            let tokenSource = this.filterTokenSource
            if (tokenSource && !tokenSource.token.isCancellationRequested) {
              await this.filterItems(remain, { append, reload }, tokenSource.token)
            }
          } else {
            let items = this.convertToHighlightItems(remain)
            this._onDidChangeItems.fire({ items, append, reload, finished })
          }
        })
        filtering = false
      }
      let promise: Promise<void> = Promise.resolve()
      let interval = setInterval(() => {
        if (filtering) return
        promise = _onData()
      }, 50)
      task.on('data', item => {
        if (token.isCancellationRequested) return
        totalItems.push(item)
      })
      let onEnd = () => {
        if (task == null) return
        this.tokenSource = null
        task = null
        this.loading = false
        this._finished = true
        disposable.dispose()
        clearInterval(interval)
        promise.then(() => {
          if (token.isCancellationRequested) return
          if (totalItems.length == 0) {
            this._onDidChangeItems.fire({ items: [], append: false, reload, finished: true })
            return
          }
          return _onData(true)
        }).catch(e => {
          logger.error('Error on filter', e)
        })
      }
      let disposable = token.onCancellationRequested(() => {
        task?.dispose()
        onEnd()
      })
      task.on('error', async (error: Error | string) => {
        if (task == null) return
        task = null
        this.tokenSource = null
        this.loading = false
        disposable.dispose()
        clearInterval(interval)
        this.nvim.call('coc#prompt#stop_prompt', ['list'], true)
        this.nvim.echoError(`Task error: ${error.toString()}`)
        logger.error('Task error:', error)
      })
      task.on('end', onEnd)
    }
  }

  /*
   * Draw all items with filter if necessary
   */
  public async drawItems(): Promise<void> {
    let { totalItems } = this
    if (totalItems.length === 0) return
    this.cancelFilter()
    let tokenSource = this.filterTokenSource = new CancellationTokenSource()
    let token = tokenSource.token
    await this.mutex.use(async () => {
      if (token.isCancellationRequested) return
      let { totalItems } = this
      this.filteredCount = totalItems.length
      await this.filterItems(totalItems, {}, tokenSource.token)
    })
  }

  public cancelFilter(): void {
    if (this.filterTokenSource) {
      this.filterTokenSource.cancel()
      this.filterTokenSource = null
    }
  }

  public stop(): void {
    this.cancelFilter()
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource = null
    }
    this.loading = false
  }

  public get length(): number {
    return this.totalItems.length
  }

  private get input(): string {
    return this.prompt.input
  }

  /**
   * Add highlights for interactive list
   */
  private convertToHighlightItems(items: ListItem[]): ListItemWithHighlights[] {
    let input = this.input ?? ''
    let res = items.map(item => {
      this.convertItemLabel(item)
      let highlights = input.length > 0 ? getItemHighlights(input, item) : undefined
      return Object.assign({}, item, { highlights })
    })
    return res
  }

  private async filterItemsByInclude(inputs: string[], items: ListItem[], token: CancellationToken, onFilter: OnFilter): Promise<void> {
    let { ignorecase } = this.listOptions
    if (ignorecase) inputs = inputs.map(s => s.toLowerCase())
    await filter(items, item => {
      this.convertItemLabel(item)
      let spans: [number, number][] = []
      let filterLabel = getFilterLabel(item)
      let match = true
      for (let input of inputs) {
        let idx = ignorecase ? filterLabel.toLowerCase().indexOf(input) : filterLabel.indexOf(input)
        if (idx == -1) {
          match = false
          break
        }
        spans.push([byteIndex(filterLabel, idx), byteIndex(filterLabel, idx + byteLength(input))])
      }
      if (!match) return false
      return { highlights: { spans } }
    }, onFilter, token)
  }

  private async filterItemsByRegex(inputs: string[], items: ListItem[], token: CancellationToken, onFilter: OnFilter): Promise<void> {
    let { ignorecase } = this.listOptions
    let flags = ignorecase ? 'iu' : 'u'
    let regexes = inputs.reduce((p, c) => {
      try {
        p.push(new RegExp(c, flags))
      } catch (e) {}
      return p
    }, [])
    await filter(items, item => {
      this.convertItemLabel(item)
      let spans: [number, number][] = []
      let filterLabel = getFilterLabel(item)
      let match = true
      for (let regex of regexes) {
        let ms = filterLabel.match(regex)
        if (ms == null) {
          match = false
          break
        }
        spans.push([byteIndex(filterLabel, ms.index), byteIndex(filterLabel, ms.index + byteLength(ms[0]))])
      }
      if (!match) return false
      return { highlights: { spans } }
    }, onFilter, token)
  }

  private async filterItemsByFuzzyMatch(inputs: string[], items: ListItem[], token: CancellationToken, onFilter: OnFilter): Promise<void> {
    let { sort } = this.listOptions
    let idx = 0
    await filter(items, item => {
      this.convertItemLabel(item)
      let filterText = item.filterText || item.label
      let matchScore = 0
      let matches: number[] = []
      let filterLabel = getFilterLabel(item)
      let match = true
      for (let input of inputs) {
        if (!hasMatch(input, filterText)) {
          match = false
          break
        }
        matches.push(...positions(input, filterLabel))
        if (sort) matchScore += score(input, filterText)
      }
      idx = idx + 1
      if (!match) return false
      return {
        sortText: typeof item.sortText === 'string' ? item.sortText : String.fromCharCode(idx),
        score: matchScore,
        highlights: getHighlights(filterLabel, matches)
      }
    }, (items, done) => {
      onFilter(items, done, sort)
    }, token)
  }

  private async filterItems(arr: ListItem[], opts: FilterOption, token: CancellationToken): Promise<void> {
    let { input } = this
    if (input.length === 0) {
      let items = arr.map(item => {
        return this.convertItemLabel(item)
      })
      this._onDidChangeItems.fire({ items, finished: this._finished, ...opts })
      return
    }
    let inputs = this.config.extendedSearchMode ? parseInput(input) : [input]
    let called = false
    const onFilter = (items: ListItemWithHighlights[], finished: boolean, sort?: boolean) => {
      finished = finished && this._finished
      if (token.isCancellationRequested || (!finished && items.length == 0)) return
      if (sort) {
        items.sort((a, b) => {
          if (a.score != b.score) return b.score - a.score
          if (a.sortText > b.sortText) return 1
          return -1
        })
      }
      let append = opts.append === true || called
      called = true
      this._onDidChangeItems.fire({ items, append, reload: opts.reload, finished })
    }
    switch (this.listOptions.matcher) {
      case 'strict':
        await this.filterItemsByInclude(inputs, arr, token, onFilter)
        break
      case 'regex':
        await this.filterItemsByRegex(inputs, arr, token, onFilter)
        break
      default:
        await this.filterItemsByFuzzyMatch(inputs, arr, token, onFilter)
    }
  }

  // set correct label, add ansi highlights
  private convertItemLabel(item: ListItem): ListItem {
    let { label, converted } = item
    if (converted) return item
    if (label.includes('\n')) {
      label = item.label = label.replace(/\r?\n/g, ' ')
    }
    if (label.includes(controlCode)) {
      let { line, highlights } = parseAnsiHighlights(label)
      item.label = line
      if (!Array.isArray(item.ansiHighlights)) item.ansiHighlights = highlights
    }
    item.converted = true
    return item
  }

  public dispose(): void {
    this.stop()
  }
}

function getFilterLabel(item: ListItem): string {
  return item.filterText != null ? patchLine(item.filterText, item.label) : item.label
}

/**
 * `a\ b` => [`a b`]
 * `a b` =>  ['a', 'b']
 */
export function parseInput(input: string): string[] {
  let res: string[] = []
  let startIdx = 0
  let currIdx = 0
  let prev = ''
  for (; currIdx < input.length; currIdx++) {
    let ch = input[currIdx]
    if (ch.charCodeAt(0) === 32) {
      // find space
      if (prev && prev != '\\' && startIdx != currIdx) {
        res.push(input.slice(startIdx, currIdx))
        startIdx = currIdx + 1
      }
    } else {
    }
    prev = ch
  }
  if (startIdx != input.length) {
    res.push(input.slice(startIdx, input.length))
  }
  return res.map(s => s.replace(/\\\s/g, ' ').trim()).filter(s => s.length > 0)
}

export function getHighlights(text: string, matches?: number[]): ListHighlights {
  let spans: [number, number][] = []
  if (matches && matches.length) {
    let start = matches.shift()
    let next = matches.shift()
    let curr = start
    while (next) {
      if (next == curr + 1) {
        curr = next
        next = matches.shift()
        continue
      }
      spans.push([byteIndex(text, start), byteIndex(text, curr) + 1])
      start = next
      curr = start
      next = matches.shift()
    }
    spans.push([byteIndex(text, start), byteIndex(text, curr) + 1])
  }
  return { spans }
}

export function getItemHighlights(input: string, item: ListItem): ListHighlights {
  let filterLabel = getFilterLabel(item)
  let res = getMatchResult(filterLabel, input)
  if (!res?.score) return { spans: [] }
  return getHighlights(filterLabel, res.matches)
}
