'use strict'
import path from 'path'
import minimatch from 'minimatch'
import { URI } from 'vscode-uri'
import languages from '../../languages'
import { ListContext, ListItem } from '../../types'
import workspace from '../../workspace'
import LocationList from './location'
import { getSymbolKind } from '../../util/convert'
import { isParentFolder } from '../../util/fs'
import { score } from '../../util/fzy'
import { CancellationToken, CancellationTokenSource } from 'vscode-languageserver-protocol'
import { formatListItems, UnformattedListItem } from '../formatting'
const logger = require('../../util/logger')('list-symbols')

export default class Symbols extends LocationList {
  public readonly interactive = true
  public readonly description = 'search workspace symbols'
  public readonly detail = 'Symbols list is provided by server, it works on interactive mode only.'
  private cwd: string
  public name = 'symbols'
  public options = [{
    name: '-k, -kind KIND',
    description: 'Filter symbols by kind.',
    hasValue: true
  }]

  public async loadItems(context: ListContext, token: CancellationToken): Promise<ListItem[]> {
    let { input } = context
    this.cwd = context.cwd
    let args = this.parseArguments(context.args)
    let filterKind = args.kind ? (args.kind as string).toLowerCase() : ''
    if (!context.options.interactive) {
      throw new Error('Symbols only works on interactive mode')
    }
    let symbols = await languages.getWorkspaceSymbols(input, token)
    if (!symbols) {
      throw new Error('No workspace symbols provider registered')
    }
    let config = this.getConfig()
    let excludes = config.get<string[]>('excludes', [])
    let items: UnformattedListItem[] = []
    for (let s of symbols) {
      let kind = getSymbolKind(s.kind)
      if (filterKind && kind.toLowerCase() != filterKind) {
        continue
      }
      let file = URI.parse(s.location.uri).fsPath
      if (isParentFolder(workspace.cwd, file)) {
        file = path.relative(workspace.cwd, file)
      }
      if (excludes.some(p => minimatch(file, p))) {
        continue
      }
      items.push({
        label: [s.name, `[${kind}]`, file],
        filterText: `${s.name}`,
        location: s.location,
        data: { original: s, kind: s.kind, file, score: score(input, s.name) }
      })
    }
    items.sort((a, b) => {
      if (a.data.score != b.data.score) {
        return b.data.score - a.data.score
      }
      if (a.data.kind != b.data.kind) {
        return a.data.kind - b.data.kind
      }
      return a.data.file.length - b.data.file.length
    })
    return formatListItems(this.alignColumns, items)
  }

  public async resolveItem(item: ListItem): Promise<ListItem> {
    let s = item.data.original
    if (!s) return null
    let tokenSource = new CancellationTokenSource()
    let resolved = await languages.resolveWorkspaceSymbol(s, tokenSource.token)
    if (!resolved) return null
    let kind = getSymbolKind(resolved.kind)
    let file = URI.parse(resolved.location.uri).fsPath
    if (isParentFolder(this.cwd, file)) {
      file = path.relative(this.cwd, file)
    }
    return {
      label: `${s.name}\t[${kind}]\t${file}`,
      filterText: `${s.name}`,
      location: s.location
    }
  }

  public doHighlight(): void {
    let { nvim } = this
    nvim.pauseNotification()
    nvim.command('syntax match CocSymbolsName /\\v^\\s*\\S+/ contained containedin=CocSymbolsLine', true)
    nvim.command('syntax match CocSymbolsKind /\\[\\w\\+\\]\\s*\\t/ contained containedin=CocSymbolsLine', true)
    nvim.command('syntax match CocSymbolsFile /\\S\\+$/ contained containedin=CocSymbolsLine', true)
    nvim.command('highlight default link CocSymbolsName Normal', true)
    nvim.command('highlight default link CocSymbolsKind Typedef', true)
    nvim.command('highlight default link CocSymbolsFile Comment', true)
    nvim.resumeNotification(false, true)
  }
}
