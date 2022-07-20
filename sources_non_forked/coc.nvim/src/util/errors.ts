'use strict'

export function illegalArgument(name?: string): Error {
  if (name) {
    return new Error(`Illegal argument: ${name}`)
  } else {
    return new Error('Illegal argument')
  }
}

export function directoryNotExists(dir: string): Error {
  return new Error(`Directory ${dir} not exists`)
}

export function fileExists(filepath: string) {
  return new Error(`File ${filepath} already exists`)
}

export function fileNotExists(filepath: string) {
  return new Error(`File ${filepath} not exists`)
}

export function shouldNotAsync(method: string) {
  return new Error(`${method} should not be called in an asynchronize manner`)
}

export function badScheme(scheme: string) {
  return new Error(`Change of ${scheme} not supported`)
}
