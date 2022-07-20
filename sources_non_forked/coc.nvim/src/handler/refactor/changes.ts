'use strict'
import { equals } from '../../util/object'

export interface LineInfo {
  filepath: string
  // start lnum in refactor buffer, 1 indexed
  lnum: number
  start: number
  lines: string[]
}

export type DeleteChange = Map<number, LineInfo>

export default class Changes {
  private stack: DeleteChange[] = []

  public add(infos: LineInfo[]): void {
    let map: Map<number, LineInfo> = new Map()
    for (let info of infos) {
      map.set(info.lnum, info)
    }
    this.stack.push(map)
  }

  public checkInsert(lnums: number[]): LineInfo[] | undefined {
    if (!this.stack.length) return undefined
    let last = this.stack[this.stack.length - 1]
    let arr = Array.from(last.keys()).sort((a, b) => a - b)
    if (!equals(arr, lnums)) return undefined
    this.stack.pop()
    return Array.from(last.values())
  }
}
