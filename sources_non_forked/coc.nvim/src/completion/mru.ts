'use strict'
import Mru from '../model/mru'
import { ExtendedCompleteItem } from '../types'

export type Selection = 'none' | 'recentlyUsed' | 'recentlyUsedByPrefix'

export default class MruLoader {
  private mru: Mru
  private max = 0
  private items: Map<string, number> = new Map()
  private itemsNoPrefex: Map<string, number> = new Map()
  constructor(private selection: Selection) {
    this.mru = new Mru(`suggest${globalThis.__TEST__ ? process.pid : ''}.txt`, process.env.COC_DATA_HOME, 1000)
  }

  public async load(): Promise<void> {
    let { selection } = this
    if (selection == 'none') return
    let lines = await this.mru.load()
    let total = lines.length
    for (let i = total - 1; i >= 0; i--) {
      let line = lines[i]
      if (!line.includes('|')) continue
      let [_prefix, label, source, kind] = line.split('|')
      if (!source) continue
      this.items.set(line, total - 1 - i)
      this.itemsNoPrefex.set(`${label}|${source}|${kind || ''}`, total - 1 - i)
    }
    this.max = total - 1
  }

  public getScore(input: string, item: ExtendedCompleteItem): number {
    let key = toItemKey(item)
    if (input.length == 0) return this.itemsNoPrefex.get(key) ?? -1
    if (this.selection === 'recentlyUsedByPrefix') key = `${input}|${key}`
    let map = this.selection === 'recentlyUsed' ? this.itemsNoPrefex : this.items
    return map.get(key) ?? -1
  }

  public add(prefix: string, item: ExtendedCompleteItem): void {
    if (this.selection == 'none') return
    let key = toItemKey(item)
    let line = `${prefix}|key`
    this.items.set(line, this.max)
    this.itemsNoPrefex.set(key, this.max)
    this.max += 1
    void this.mru.add(line)
  }
}

function toItemKey(item: ExtendedCompleteItem): string {
  let label = item.filterText
  let source = item.source
  let kind = item.kind ?? ''
  return `${label}|${source}|${kind}`
}
