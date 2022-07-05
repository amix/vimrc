'use strict'
import style from 'ansi-styles'

export function gray(str: string): string {
  return `${style.gray.open}${str}${style.gray.close}`
}

export function magenta(str: string): string {
  return `${style.magenta.open}${str}${style.magenta.close}`
}

export function bold(str: string): string {
  return `${style.bold.open}${str}${style.bold.close}`
}

export function underline(str: string): string {
  return `${style.underline.open}${str}${style.underline.close}`
}

export function strikethrough(str: string): string {
  return `${style.strikethrough.open}${str}${style.strikethrough.close}`
}

export function italic(str: string): string {
  return `${style.italic.open}${str}${style.italic.close}`
}

export function yellow(str: string): string {
  return `${style.yellow.open}${str}${style.yellow.close}`
}

export function green(str: string): string {
  return `${style.green.open}${str}${style.green.close}`
}

export function blue(str: string): string {
  return `${style.blue.open}${str}${style.blue.close}`
}
