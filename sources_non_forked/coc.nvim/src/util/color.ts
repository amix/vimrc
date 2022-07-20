'use strict'
import { Color } from 'vscode-languageserver-protocol'

function pad(str: string): string {
  return str.length == 1 ? `0${str}` : str
}

export function toHexString(color: Color): string {
  let c = toHexColor(color)
  return `${pad(c.red.toString(16))}${pad(c.green.toString(16))}${pad(c.blue.toString(16))}`
}

export function toHexColor(color: Color): { red: number; green: number; blue: number } {
  let { red, green, blue } = color
  return {
    red: Math.round(red * 255),
    green: Math.round(green * 255),
    blue: Math.round(blue * 255)
  }
}

export function isDark(color: Color): boolean {
  // http://www.w3.org/TR/WCAG20/#relativeluminancedef
  let rgb = [color.red, color.green, color.blue]
  let lum = []
  for (let i = 0; i < rgb.length; i++) {
    let chan = rgb[i]
    lum[i] = (chan <= 0.03928) ? chan / 12.92 : Math.pow(((chan + 0.055) / 1.055), 2.4)
  }
  let luma = 0.2126 * lum[0] + 0.7152 * lum[1] + 0.0722 * lum[2]
  return luma <= 0.5
}

