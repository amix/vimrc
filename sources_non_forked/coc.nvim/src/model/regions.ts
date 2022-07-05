'use strict'
/**
 * Remember used regions
 */
export default class Regions {
  /**
   * ranges that never overlaps.
   */
  private ranges: [number, number][] = []

  public get current(): ReadonlyArray<number> {
    let res: number[] = []
    this.ranges.sort((a, b) => a[0] - b[0])
    this.ranges.forEach(o => {
      res.push(o[0], o[1])
    })
    return res
  }

  public clear(): void {
    this.ranges = []
  }

  public add(start: number, end: number): void {
    if (start > end) {
      [start, end] = [end, start]
    }
    let { ranges } = this
    if (ranges.length == 0) {
      ranges.push([start, end])
    } else {
      // 1, 2, 3
      ranges.sort((a, b) => a[0] - b[0])
      let s: number
      let e: number
      let removedIndexes: number[] = []
      for (let i = 0; i < ranges.length; i++) {
        let r = ranges[i]
        if (r[1] < start || r[0] > end) continue
        removedIndexes.push(i)
        if (s == null) s = Math.min(start, r[0])
        e = Math.max(end, r[1])
      }
      let newRanges = removedIndexes.length ? ranges.filter((_, i) => !removedIndexes.includes(i)) : ranges
      this.ranges = newRanges
      if (s != null && e != null) {
        this.ranges.push([s, e])
      } else {
        this.ranges.push([start, end])
      }
    }
  }

  public has(start: number, end: number): boolean {
    let idx = this.ranges.findIndex(o => o[0] <= start && o[1] >= end)
    return idx !== -1
  }

  public static mergeSpans(ranges: [number, number][]): [number, number][] {
    let res: [number, number][] = []
    for (let r of ranges) {
      let idx = res.findIndex(o => !(r[1] < o[0] || r[0] > o[1]))
      if (idx == -1) {
        res.push(r)
      } else {
        let o = res[idx]
        res[idx] = [Math.min(r[0], o[0]), Math.max(r[1], o[1])]
      }
    }
    return res
  }
}
