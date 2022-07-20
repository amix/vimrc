'use strict'
import { Command, MarkupContent } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import path from 'path'

export interface TreeItemLabel {
  label: string
  highlights?: [number, number][]
}

// eslint-disable-next-line no-redeclare
export namespace TreeItemLabel {
  export function is(obj: any): obj is TreeItemLabel {
    return typeof obj.label == 'string'
  }
}

export interface TreeItemIcon {
  text: string
  hlGroup: string
}

/**
 * Collapsible state of the tree item
 */
export enum TreeItemCollapsibleState {
  /**
   * Determines an item can be neither collapsed nor expanded. Implies it has no children.
   */
  None = 0,
  /**
   * Determines an item is collapsed
   */
  Collapsed = 1,
  /**
   * Determines an item is expanded
   */
  Expanded = 2
}

export class TreeItem {
  public label: string | TreeItemLabel
  public id?: string
  public description?: string
  public icon?: TreeItemIcon
  public resourceUri?: URI
  public command?: Command
  public tooltip?: string | MarkupContent
  public deprecated?: boolean

  constructor(label: string | TreeItemLabel, collapsibleState?: TreeItemCollapsibleState)
  // eslint-disable-next-line @typescript-eslint/unified-signatures
  constructor(resourceUri: URI, collapsibleState?: TreeItemCollapsibleState)
  constructor(label: string | TreeItemLabel | URI, public collapsibleState: TreeItemCollapsibleState = TreeItemCollapsibleState.None) {
    if (URI.isUri(label)) {
      this.resourceUri = label
      this.label = path.basename(label.path)
      this.id = label.toString()
    } else {
      this.label = label
    }
  }
}
