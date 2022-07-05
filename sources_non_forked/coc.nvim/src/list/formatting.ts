'use strict'
import { ListItem } from '../types'
import path from 'path'

export type PathFormatting = "full" | "short" | "filename" | "hidden"

export interface UnformattedListItem extends Omit<ListItem, 'label'> {
  label: string[]
}

export function formatListItems(align: boolean, list: UnformattedListItem[]): ListItem[] {
  if (list.length === 0) {
    return []
  }

  let processedList: ListItem[] = []
  if (align) {
    const maxWidths = Array(Math.min(...list.map(item => item.label.length))).fill(0)
    for (let item of list) {
      for (let i = 0; i < maxWidths.length; i++) {
        maxWidths[i] = Math.max(maxWidths[i], item.label[i].length)
      }
    }
    processedList = list
      .map(item => ({
        ...item,
        label: item.label
          .map((element, idx) => element.padEnd(maxWidths[idx]))
          .join("\t")
      }))
  } else {
    processedList = list.map(item => ({ ...item, label: item.label.join("\t") }))
  }
  return processedList
}

export function formatPath(format: PathFormatting, pathToFormat: string): string {
  if (format === "hidden") {
    return ""
  } else if (format === "full") {
    return pathToFormat
  } else if (format === "short") {
    const segments = pathToFormat.split(path.sep)
    if (segments.length < 2) {
      return pathToFormat
    }
    const shortenedInit = segments
      .slice(0, segments.length - 2)
      .filter(seg => seg.length > 0)
      .map(seg => seg[0])
    return [...shortenedInit, segments[segments.length - 1]].join(path.sep)
  } else {
    const segments = pathToFormat.split(path.sep)
    return segments[segments.length - 1] ?? ""
  }
}
