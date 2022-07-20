'use strict'
import { v4 as uuid } from 'uuid'
import { CancellationToken, MarkupContent, Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import commandsManager from '../commands'
import { ProviderResult } from '../provider'
import { disposeAll } from '../util'
import { TreeDataProvider, TreeItemAction } from './index'
import { TreeItem, TreeItemCollapsibleState, TreeItemIcon, TreeItemLabel } from './TreeItem'

export interface TreeNode {
  label: string
  key?: string
  tooltip?: string | MarkupContent
  description?: string
  deprecated?: boolean
  icon?: TreeItemIcon
  children?: this[]
}

export interface ProviderOptions<T> {
  provideData: () => ProviderResult<T[]>
  expandLevel?: number
  onDispose?: () => void
  handleClick?: (item: T) => ProviderResult<void>
  resolveIcon?: (item: T) => TreeItemIcon | undefined
  resolveItem?: (item: TreeItem, element: T, token: CancellationToken) => ProviderResult<TreeItem>
  resolveActions?(item: TreeItem, element: T): ProviderResult<TreeItemAction<T>[]>
}

function isIcon(obj: any): obj is TreeItemIcon {
  if (!obj) return false
  return typeof obj.text === 'string' && typeof obj.hlGroup === 'string'
}

/**
 * Check label and key, children not checked.
 */
function sameTreeNode<T extends TreeNode>(one: T, two: T): boolean {
  if (one.label === two.label
    && one.deprecated === two.deprecated
    && one.key === two.key) {
    return true
  }
  return false
}

/**
 * Check changes of nodes array, children not checked.
 */
function sameTreeNodes<T extends TreeNode>(one: T[], two: T[]): boolean {
  if (one.length !== two.length) return false
  return one.every((v, idx) => sameTreeNode(v, two[idx]))
}

/**
 * Tree data provider for resolved tree with children.
 * Use update() to update data.
 */
export default class BasicDataProvider<T extends TreeNode> implements TreeDataProvider<T> {
  private disposables: Disposable[] = []
  private invokeCommand: string
  private data: T[] | undefined
  // only fired for change of exists TreeNode
  private _onDidChangeTreeData = new Emitter<void | T>()
  public onDidChangeTreeData: Event<void | T> = this._onDidChangeTreeData.event
  public resolveActions: (item: TreeItem, element: T) => ProviderResult<TreeItemAction<T>[]>
  // data is shared with TreeView
  constructor(private opts: ProviderOptions<T>) {
    this.invokeCommand = `_invoke_${uuid()}`
    this.disposables.push(commandsManager.registerCommand(this.invokeCommand, async (node: T) => {
      if (typeof opts.handleClick === 'function') {
        await opts.handleClick(node)
      } else {
        console.error('Handler not found')
      }
    }, null, true))
    if (typeof opts.resolveActions === 'function') {
      this.resolveActions = opts.resolveActions.bind(this)
    }
  }

  private iterate(node: T, parentNode: T | undefined, level: number, fn: (node: T, parentNode: T | undefined, level: number) => void | boolean): void | boolean {
    let res = fn(node, parentNode, level)
    if (res === false) return false
    if (Array.isArray(node.children)) {
      for (let element of node.children) {
        let res = this.iterate(element, node, level + 1, fn)
        if (res === false) return false
      }
    }
    return res
  }

  /**
   * Change old array to new nodes in place, keep old reference when possible.
   */
  private updateNodes(old: T[], data: T[], parentNode: T | undefined, fireEvent = true): void {
    let sameNodes = sameTreeNodes(old, data)
    const applyNode = (previous: T, curr: T, fireEvent: boolean): void => {
      let changed = false
      for (let key of Object.keys(curr)) {
        if (['children', 'key'].includes(key)) continue
        previous[key] = curr[key]
      }
      if (previous.children?.length && !curr.children?.length) {
        // removed children
        delete previous.children
        changed = true
      }
      if (!previous.children?.length && curr.children?.length) {
        // new children
        previous.children = curr.children
        changed = true
      }
      if (changed) {
        if (fireEvent) this._onDidChangeTreeData.fire(previous)
        return
      }
      if (previous.children?.length && curr.children?.length) {
        this.updateNodes(previous.children, curr.children, previous, fireEvent)
      }
    }
    if (sameNodes) {
      for (let i = 0; i < old.length; i++) {
        applyNode(old[i], data[i], fireEvent)
      }
    } else {
      let oldNodes = old.splice(0, old.length)
      let used: Set<number> = new Set()
      for (let i = 0; i < data.length; i++) {
        let curr = data[i]
        let findIndex: number
        if (curr.key) {
          findIndex = oldNodes.findIndex((o, i) => !used.has(i) && o.key == curr.key)
        } else {
          findIndex = oldNodes.findIndex((o, i) => !used.has(i) && o.label == curr.label)
        }
        if (findIndex === -1) {
          old[i] = curr
        } else {
          used.add(findIndex)
          let previous = oldNodes[findIndex]
          applyNode(previous, curr, false)
          old[i] = previous
        }
      }
      if (fireEvent) {
        this._onDidChangeTreeData.fire(parentNode)
      }
    }
  }

  /**
   * Update with new data, fires change event when necessary.
   */
  public update(data: T[], reset?: boolean): ReadonlyArray<T> {
    if (!this.data) return
    if (reset) {
      this.data = data || []
      this._onDidChangeTreeData.fire(undefined)
    } else {
      this.updateNodes(this.data, data || [], undefined)
    }
    return this.data
  }

  public getTreeItem(node: T): TreeItem {
    let label: string | TreeItemLabel = node.label
    let { expandLevel } = this.opts
    let item: TreeItem
    if (!node.children?.length) {
      item = new TreeItem(label)
    } else {
      if (expandLevel && expandLevel > 0) {
        let level = this.getLevel(node)
        let state = level && level <= expandLevel ? TreeItemCollapsibleState.Expanded : TreeItemCollapsibleState.Collapsed
        item = new TreeItem(label, state)
      } else {
        item = new TreeItem(label, TreeItemCollapsibleState.Collapsed)
      }
    }
    item.description = node.description
    if (node.deprecated) item.deprecated = true
    if (node.tooltip) item.tooltip = node.tooltip
    if (isIcon(node.icon)) {
      item.icon = node.icon
    } else if (typeof this.opts.resolveIcon === 'function') {
      let res = this.opts.resolveIcon(node)
      if (res) item.icon = res
    }
    return item
  }

  public async getChildren(element?: T): Promise<T[]> {
    if (element) return element.children || []
    if (this.data) return this.data
    let data = await Promise.resolve(this.opts.provideData())
    if (!Array.isArray(data)) throw new Error(`Unable to fetch data`)
    this.data = data
    return data
  }

  /**
   * Use reference check
   */
  public getParent(element: T): T | undefined {
    if (!this.data) return undefined
    let find: T
    for (let item of this.data) {
      let res = this.iterate(item, null, 0, (node, parentNode) => {
        if (node === element) {
          find = parentNode
          return false
        }
      })
      if (res === false) break
    }
    return find
  }

  private getLevel(element: T): number {
    if (!this.data) return undefined
    let level = 0
    for (let item of this.data) {
      let res = this.iterate(item, null, 1, (node, _parentNode, l) => {
        if (node === element) {
          level = l
          return false
        }
      })
      if (res === false) break
    }
    return level
  }

  /**
   * Resolve command and tooltip
   */
  public async resolveTreeItem(item: TreeItem, element: T, token: CancellationToken): Promise<TreeItem> {
    if (typeof this.opts.resolveItem === 'function') {
      let res = await Promise.resolve(this.opts.resolveItem(item, element, token))
      if (res) Object.assign(item, res)
    }
    if (!item.command) {
      item.command = {
        title: `invoke ${element.label}`,
        command: this.invokeCommand,
        arguments: [element]
      }
    }
    return item
  }

  public dispose(): void {
    this.data = []
    this._onDidChangeTreeData.dispose()
    if (typeof this.opts.onDispose === 'function') {
      this.opts.onDispose()
    }
    disposeAll(this.disposables)
  }
}
