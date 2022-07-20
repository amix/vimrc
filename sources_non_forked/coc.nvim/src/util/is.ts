'use strict'
import { VimCompleteItem } from '../types'
/* eslint-disable id-blacklist */
const hasOwnProperty = Object.prototype.hasOwnProperty

export function vimCompleteItem(value: any): value is VimCompleteItem {
  return value && typeof value.word === 'string' && value.user_data !== ''
}

export function boolean(value: any): value is boolean {
  return typeof value === 'boolean'
}

export function string(value: any): value is string {
  return typeof value === 'string'
}

export function number(value: any): value is number {
  return typeof value === 'number'
}

export function array(array: any): array is any[] {
  return Array.isArray(array)
}

export function func(value: any): value is Function {
  return typeof value == 'function'
}

export function objectLiteral(obj: any): obj is object {
  return (
    obj != null &&
    typeof obj === 'object' &&
    !Array.isArray(obj) &&
    !(obj instanceof RegExp) &&
    !(obj instanceof Date)
  )
}

export function emptyObject(obj: any): boolean {
  if (!objectLiteral(obj)) {
    return false
  }

  for (let key in obj) {
    if (hasOwnProperty.call(obj, key)) {
      return false
    }
  }

  return true
}

export function typedArray<T>(
  value: any,
  check: (value: any) => boolean
): value is T[] {
  return Array.isArray(value) && (value as any).every(check)
}
