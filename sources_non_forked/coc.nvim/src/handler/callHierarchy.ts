'use strict'
import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, CancellationToken, CancellationTokenSource, Disposable, Emitter, Position, Range, SymbolTag } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import commands from '../commands'
import events from '../events'
import languages from '../languages'
import { TreeDataProvider, TreeItem, TreeItemCollapsibleState } from '../tree/index'
import BasicTreeView from '../tree/TreeView'
import { ConfigurationChangeEvent, HandlerDelegate } from '../types'
import { disposeAll } from '../util'
import { omit } from '../util/lodash'
import workspace from '../workspace'
const logger = require('../util/logger')('Handler-callHierarchy')

interface CallHierarchyDataItem extends CallHierarchyItem {
  ranges?: Range[]
  sourceUri?: string
  children?: CallHierarchyItem[]
}

interface CallHierarchyConfig {
  splitCommand: string
  openCommand: string
  enableTooltip: boolean
}

interface CallHierarchyProvider extends TreeDataProvider<CallHierarchyDataItem> {
  kind: 'incoming' | 'outgoing'
  dispose: () => void
}

function isCallHierarchyItem(item: any): item is CallHierarchyItem {
  if (item && item.name && item.kind && Range.is(item.range) && item.uri) return true
  return false
}

export default class CallHierarchyHandler {
  private config: CallHierarchyConfig
  private disposables: Disposable[] = []
  public static commandId = 'callHierarchy.reveal'
  public static rangesHighlight = 'CocSelectedRange'
  private highlightWinids: Set<number> = new Set()
  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
    this.loadConfiguration()
    workspace.onDidChangeConfiguration(this.loadConfiguration, this, this.disposables)
    this.disposables.push(commands.registerCommand(CallHierarchyHandler.commandId, async (winid: number, item: CallHierarchyDataItem, openCommand?: string) => {
      let { nvim } = this
      await nvim.call('win_gotoid', [winid])
      await workspace.jumpTo(item.uri, item.selectionRange.start, openCommand)
      let win = await nvim.window
      win.clearMatchGroup(CallHierarchyHandler.rangesHighlight)
      win.highlightRanges(CallHierarchyHandler.rangesHighlight, [item.selectionRange], 10, true)
      if (!item.ranges?.length) return
      if (item.sourceUri) {
        let doc = workspace.getDocument(item.sourceUri)
        if (!doc) return
        let winid = await nvim.call('coc#compat#buf_win_id', [doc.bufnr])
        if (winid == -1) return
        if (winid != win.id) {
          win = nvim.createWindow(winid)
          win.clearMatchGroup(CallHierarchyHandler.rangesHighlight)
        }
      }
      win.highlightRanges(CallHierarchyHandler.rangesHighlight, item.ranges, 100, true)
      this.highlightWinids.add(win.id)
    }, null, true))
    events.on('BufWinEnter', (_, winid) => {
      if (this.highlightWinids.has(winid)) {
        this.highlightWinids.delete(winid)
        let win = nvim.createWindow(winid)
        win.clearMatchGroup(CallHierarchyHandler.rangesHighlight)
      }
    }, null, this.disposables)
  }

  private loadConfiguration(e?: ConfigurationChangeEvent): void {
    if (!e || e.affectsConfiguration('callHierarchy')) {
      let c = workspace.getConfiguration('callHierarchy')
      this.config = {
        splitCommand: c.get<string>('splitCommand'),
        openCommand: c.get<string>('openCommand'),
        enableTooltip: c.get<boolean>('enableTooltip')
      }
    }
  }

  private createProvider(doc: TextDocument, winid: number, position: Position, kind: 'incoming' | 'outgoing'): CallHierarchyProvider {
    let _onDidChangeTreeData = new Emitter<void | CallHierarchyDataItem>()
    let source: CancellationTokenSource | undefined
    let rootItems: CallHierarchyDataItem[] | undefined
    const cancel = () => {
      if (source) {
        source.cancel()
        source.dispose()
        source = null
      }
    }
    const findParent = (curr: CallHierarchyDataItem, element: CallHierarchyDataItem): CallHierarchyDataItem | undefined => {
      let children = curr.children
      if (!Array.isArray(children)) return undefined
      let find = children.find(o => o == element)
      if (find) return curr
      for (let item of children) {
        let res = findParent(item, element)
        if (res) return res
      }
    }
    let provider: CallHierarchyProvider = {
      kind,
      onDidChangeTreeData: _onDidChangeTreeData.event,
      getTreeItem: element => {
        let item = new TreeItem(element.name, element.children ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.Collapsed)
        if (this.config.enableTooltip) {
          item.tooltip = path.relative(workspace.cwd, URI.parse(element.uri).fsPath)
        }
        item.description = element.detail
        item.deprecated = element.tags?.includes(SymbolTag.Deprecated)
        item.icon = this.handler.getIcon(element.kind)
        item.command = {
          command: CallHierarchyHandler.commandId,
          title: 'open location',
          arguments: [winid, element, this.config.openCommand]
        }
        return item
      },
      getChildren: async element => {
        cancel()
        source = new CancellationTokenSource()
        let { token } = source
        if (!element) {
          if (!rootItems) {
            rootItems = await this.prepare(doc, position, token) as CallHierarchyDataItem[]
            if (!rootItems?.length) return
          }
          for (let o of rootItems) {
            let children = await this.getChildren(doc, o, provider.kind, token)
            if (token.isCancellationRequested) break
            if (Array.isArray(children)) o.children = children
          }
          return rootItems
        }
        if (element.children) return element.children
        let items = await this.getChildren(doc, element, provider.kind, token)
        source = null
        if (token.isCancellationRequested) return []
        element.children = items
        return items
      },
      resolveActions: () => {
        return [{
          title: 'Open in new tab',
          handler: async element => {
            await commands.executeCommand(CallHierarchyHandler.commandId, winid, element, 'tabe')
          }
        }, {
          title: 'Show Incoming Calls',
          handler: element => {
            rootItems = [omit(element, ['children', 'ranges', 'sourceUri'])]
            provider.kind = 'incoming'
            _onDidChangeTreeData.fire(undefined)
          }
        }, {
          title: 'Show Outgoing Calls',
          handler: element => {
            rootItems = [omit(element, ['children', 'ranges', 'sourceUri'])]
            provider.kind = 'outgoing'
            _onDidChangeTreeData.fire(undefined)
          }
        }, {
          title: 'Dismiss',
          handler: async element => {
            let parentElement: CallHierarchyDataItem | undefined
            for (let curr of rootItems) {
              parentElement = findParent(curr, element)
              if (parentElement) break
            }
            if (!parentElement) return
            let idx = parentElement.children.findIndex(o => o === element)
            parentElement.children.splice(idx, 1)
            _onDidChangeTreeData.fire(parentElement)
          }
        }]
      },
      dispose: () => {
        cancel()
        _onDidChangeTreeData.dispose()
        rootItems = undefined
        _onDidChangeTreeData = undefined
      }
    }
    return provider
  }

  private async getChildren(doc: TextDocument, item: CallHierarchyItem, kind: 'incoming' | 'outgoing', token: CancellationToken): Promise<CallHierarchyDataItem[]> {
    let items: CallHierarchyDataItem[] = []
    if (kind == 'incoming') {
      let res = await languages.provideIncomingCalls(doc, item, token)
      if (res) items = res.map(o => Object.assign(o.from, { ranges: o.fromRanges }))
    } else {
      let res = await languages.provideOutgoingCalls(doc, item, token)
      if (res) items = res.map(o => Object.assign(o.to, { ranges: o.fromRanges, sourceUri: item.uri }))
    }
    return items
  }

  private async prepare(doc: TextDocument, position: Position, token: CancellationToken): Promise<CallHierarchyItem[] | undefined> {
    this.handler.checkProvier('callHierarchy', doc)
    const res = await languages.prepareCallHierarchy(doc, position, token)
    return isCallHierarchyItem(res) ? [res] : res
  }

  private async getCallHierarchyItems(item: CallHierarchyItem | undefined, kind: 'outgoing'): Promise<CallHierarchyOutgoingCall[]>
  private async getCallHierarchyItems(item: CallHierarchyItem | undefined, kind: 'incoming'): Promise<CallHierarchyIncomingCall[]>
  private async getCallHierarchyItems(item: CallHierarchyItem | undefined, kind: 'incoming' | 'outgoing'): Promise<any> {
    const { doc, position } = await this.handler.getCurrentState()
    const source = new CancellationTokenSource()
    if (!item) {
      await doc.synchronize()
      let res = await this.prepare(doc.textDocument, position, source.token)
      item = res ? res[0] : undefined
      if (!res) return undefined
    }
    let method = kind == 'incoming' ? 'provideIncomingCalls' : 'provideOutgoingCalls'
    return await languages[method](doc.textDocument, item, source.token)
  }

  public async getIncoming(item?: CallHierarchyItem): Promise<CallHierarchyIncomingCall[] | undefined> {
    return await this.getCallHierarchyItems(item, 'incoming')
  }

  public async getOutgoing(item?: CallHierarchyItem): Promise<CallHierarchyOutgoingCall[] | undefined> {
    return await this.getCallHierarchyItems(item, 'outgoing')
  }

  public async showCallHierarchyTree(kind: 'incoming' | 'outgoing'): Promise<void> {
    const { doc, position, winid } = await this.handler.getCurrentState()
    await doc.synchronize()
    let provider = this.createProvider(doc.textDocument, winid, position, kind)
    let treeView = new BasicTreeView('calls', { treeDataProvider: provider })
    treeView.title = `${kind.toUpperCase()} CALLS`
    provider.onDidChangeTreeData(e => {
      if (!e) treeView.title = `${provider.kind.toUpperCase()} CALLS`
    })
    treeView.onDidChangeVisibility(e => {
      if (!e.visible) provider.dispose()
    })
    this.disposables.push(treeView)
    await treeView.show(this.config.splitCommand)
  }

  public dispose(): void {
    this.highlightWinids.clear()
    disposeAll(this.disposables)
  }
}
