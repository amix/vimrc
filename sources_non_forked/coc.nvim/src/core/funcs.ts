'use strict'
import { Neovim } from '@chemzqm/neovim'
import minimatch from 'minimatch'
import os from 'os'
import path from 'path'
import semver from 'semver'
import { DocumentFilter, DocumentSelector } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import which from 'which'
import Configurations from '../configuration'
import Resolver from '../model/resolver'
import * as fs from '../util/fs'
import * as platform from '../util/platform'
let NAME_SPACE = 2000
const resolver = new Resolver()

const namespaceMap: Map<string, number> = new Map()

interface PartialEnv {
  isVim: boolean
  version: string
}

/**
 * Like vim's has(), but for version check only.
 * Check patch on neovim and check nvim on vim would return false.
 *
 * For example:
 * - has('nvim-0.6.0')
 * - has('patch-7.4.248')
 */
export function has(env: PartialEnv, feature: string): boolean {
  if (!feature.startsWith('nvim-') && !feature.startsWith('patch-')) {
    throw new Error('Feature param could only starts with nvim and patch')
  }
  if (!env.isVim && feature.startsWith('patch-')) {
    return false
  }
  if (env.isVim && feature.startsWith('nvim-')) {
    return false
  }
  if (env.isVim) {
    let [_, major, minor, patch] = env.version.match(/^(\d)(\d{2})(\d+)$/)
    let version = `${major}.${parseInt(minor, 10)}.${parseInt(patch, 10)}`
    return semver.gte(version, feature.slice(6))
  }
  return semver.gte(env.version, feature.slice(5))
}

/*
 * Create namespace id.
 *
 * @deprecated
 */
export function createNameSpace(name = ''): number {
  if (namespaceMap.has(name)) return namespaceMap.get(name)
  NAME_SPACE = NAME_SPACE + 1
  namespaceMap.set(name, NAME_SPACE)
  return NAME_SPACE
}

/**
 * Resolve watchman path.
 */
export function getWatchmanPath(configurations: Configurations): string | null {
  const preferences = configurations.getConfiguration('coc.preferences')
  let watchmanPath = preferences.get<string>('watchmanPath', 'watchman')
  try {
    return which.sync(watchmanPath)
  } catch (e) {
    return null
  }
}

export async function findUp(nvim: Neovim, cwd: string, filename: string | string[]): Promise<string | null> {
  let filepath = await nvim.call('expand', '%:p') as string
  filepath = path.normalize(filepath)
  let isFile = filepath && path.isAbsolute(filepath)
  if (isFile && !fs.isParentFolder(cwd, filepath, true)) {
    // can't use cwd
    return fs.findUp(filename, path.dirname(filepath))
  }
  let res = fs.findUp(filename, cwd)
  if (res && res != os.homedir()) return res
  if (isFile) return fs.findUp(filename, path.dirname(filepath))
  return null
}

export function resolveModule(name: string): Promise<string> {
  return resolver.resolveModule(name)
}

export function score(selector: DocumentSelector | DocumentFilter | string, uri: string, languageId: string): number {
  if (Array.isArray(selector)) {
    // array -> take max individual value
    let ret = 0
    for (const filter of selector) {
      const value = score(filter, uri, languageId)
      if (value === 10) {
        return value // already at the highest
      }
      if (value > ret) {
        ret = value
      }
    }
    return ret
  } else if (typeof selector === 'string') {
    // short-hand notion, desugars to
    // 'fooLang' -> { language: 'fooLang'}
    // '*' -> { language: '*' }
    if (selector === '*') {
      return 5
    } else if (selector === languageId) {
      return 10
    } else {
      return 0
    }
  } else if (selector) {
    let u = URI.parse(uri)
    // filter -> select accordingly, use defaults for scheme
    const { language, pattern, scheme } = selector
    let ret = 0
    if (scheme) {
      if (scheme === u.scheme) {
        ret = 5
      } else if (scheme === '*') {
        ret = 3
      } else {
        return 0
      }
    }

    if (language) {
      if (language === languageId) {
        ret = 10
      } else if (language === '*') {
        ret = Math.max(ret, 5)
      } else {
        return 0
      }
    }

    if (pattern) {
      let caseInsensitive = platform.isWindows || platform.isMacintosh
      let p = caseInsensitive ? pattern.toLowerCase() : pattern
      let f = caseInsensitive ? u.fsPath.toLowerCase() : u.fsPath
      if (p === f || minimatch(f, p, { dot: true })) {
        ret = 5
      } else {
        return 0
      }
    }

    return ret
  } else {
    return 0
  }
}
