'use strict'
import { Disposable, Event, CancellationToken } from 'vscode-languageserver-protocol'
import { ProviderResult } from '../provider'
import { TreeItem, TreeItemIcon, TreeItemCollapsibleState } from './TreeItem'

export { TreeItem, TreeItemIcon, TreeItemCollapsibleState }

export interface TreeItemAction<T> {
  /**
   * Label text in menu.
   */
  title: string
  handler: (item: T) => ProviderResult<void>
}

export interface LineState {
  /**
   * Line count used by message
   */
  messageCount: number
  /**
   * Line count used by title
   */
  titleCount: number
}

export interface TreeViewKeys {
  invoke: string
  toggle: string
  actions: string
  collapseAll: string
  toggleSelection: string
  close: string
  activeFilter: string
  selectNext: string
  selectPrevious: string
}

export interface TreeItemData {
  item: TreeItem
  resolved: boolean
}

/**
 * Options for creating a {@link TreeView}
 */
export interface TreeViewOptions<T> {
  /**
   * bufhidden option for TreeView, default to 'wipe'
   */
  bufhidden?: 'hide' | 'unload' | 'delete' | 'wipe'
  /**
   * Increase width to avoid wrapped lines.
   */
  autoWidth?: boolean
  /**
   * Fixed width for window, default to true
   */
  winfixwidth?: boolean
  /**
   * Enable filter feature, default to false
   */
  enableFilter?: boolean
  /**
   * Disable indent of leaves without children, default to false
   */
  disableLeafIndent?: boolean
  /**
   * A data provider that provides tree data.
   */
  treeDataProvider: TreeDataProvider<T>
  /**
   * Whether the tree supports multi-select. When the tree supports multi-select and a command is executed from the tree,
   * the first argument to the command is the tree item that the command was executed on and the second argument is an
   * array containing all selected tree items.
   */
  canSelectMany?: boolean
}

/**
 * The event that is fired when an element in the {@link TreeView} is expanded or collapsed
 */
export interface TreeViewExpansionEvent<T> {
  /**
   * Element that is expanded or collapsed.
   */
  readonly element: T
}

/**
 * The event that is fired when there is a change in {@link TreeView.selection tree view's selection}
 */
export interface TreeViewSelectionChangeEvent<T> {
  readonly selection: T[]
}

/**
 * The event that is fired when there is a change in {@link TreeView.visible tree view's visibility}
 */
export interface TreeViewVisibilityChangeEvent {
  readonly visible: boolean
}

/**
 * Represents a Tree view
 */
export interface TreeView<T> extends Disposable {

  /**
   * Event that is fired when an element is expanded
   */
  readonly onDidExpandElement: Event<TreeViewExpansionEvent<T>>

  /**
   * Event that is fired when an element is collapsed
   */
  readonly onDidCollapseElement: Event<TreeViewExpansionEvent<T>>

  /**
   * Currently selected elements.
   */
  readonly selection: T[]

  /**
   * Event that is fired when the {@link TreeView.selection selection} has changed
   */
  readonly onDidChangeSelection: Event<TreeViewSelectionChangeEvent<T>>

  /**
   * `true` if the {@link TreeView tree view} is visible otherwise `false`.
   *
   * **NOTE:** is `true` when TreeView visible on other tab.
   */
  readonly visible: boolean

  /**
   * Window id used by TreeView.
   */
  readonly windowId: number | undefined

  /**
   * Event that is fired when {@link TreeView.visible visibility} has changed
   */
  readonly onDidChangeVisibility: Event<TreeViewVisibilityChangeEvent>

  /**
   * An optional human-readable message that will be rendered in the view.
   * Setting the message to null, undefined, or empty string will remove the message from the view.
   */
  message?: string

  /**
   * The tree view title is initially taken from viewId of TreeView
   * Changes to the title property will be properly reflected in the UI in the title of the view.
   */
  title?: string

  /**
   * An optional human-readable description which is rendered less prominently in the title of the view.
   * Setting the title description to null, undefined, or empty string will remove the description from the view.
   */
  description?: string

  /**
   * Reveals the given element in the tree view.
   * If the tree view is not visible then the tree view is shown and element is revealed.
   *
   * By default revealed element is selected.
   * In order to not to select, set the option `select` to `false`.
   * In order to focus, set the option `focus` to `true`.
   * In order to expand the revealed element, set the option `expand` to `true`. To expand recursively set `expand` to the number of levels to expand.
   * **NOTE:** You can expand only to 3 levels maximum.
   *
   * **NOTE:** The {@link TreeDataProvider} that the `TreeView` {@link window.createTreeView is registered with} with must implement {@link TreeDataProvider.getParent getParent} method to access this API.
   */
  reveal(element: T, options?: { select?: boolean, focus?: boolean, expand?: boolean | number }): Thenable<void>

  /**
   * Create tree view in new window, does nothing when it's visible.
   *
   * **NOTE:**
   * **NOTE:** TreeView with same viewId in current tab would be disposed.
   *
   * @param splitCommand The command to open TreeView window, default to 'belowright 30vs'
   */
  show(splitCommand?: string): Promise<boolean>
}

/**
 * A data provider that provides tree data
 */
export interface TreeDataProvider<T> {
  /**
   * An optional event to signal that an element or root has changed.
   * This will trigger the view to update the changed element/root and its children recursively (if shown).
   * To signal that root has changed, do not pass any argument or pass `undefined` or `null`.
   */
  onDidChangeTreeData?: Event<T | undefined | null | void>

  /**
   * Get {@link TreeItem} representation of the `element`
   *
   * @param element The element for which {@link TreeItem} representation is asked for.
   * @return {@link TreeItem} representation of the element
   */
  getTreeItem(element: T): TreeItem | Thenable<TreeItem>

  /**
   * Get the children of `element` or root if no element is passed.
   *
   * @param element The element from which the provider gets children. Can be `undefined`.
   * @return Children of `element` or root if no element is passed.
   */
  getChildren(element?: T): ProviderResult<T[]>

  /**
   * Optional method to return the parent of `element`.
   * Return `null` or `undefined` if `element` is a child of root.
   *
   * **NOTE:** This method should be implemented in order to access {@link TreeView.reveal reveal} API.
   *
   * @param element The element for which the parent has to be returned.
   * @return Parent of `element`.
   */
  getParent?(element: T): ProviderResult<T>

  /**
   * Called on hover to resolve the {@link TreeItem.tooltip TreeItem} property if it is undefined.
   * Called on tree item click/open to resolve the {@link TreeItem.command TreeItem} property if it is undefined.
   * Only properties that were undefined can be resolved in `resolveTreeItem`.
   * Functionality may be expanded later to include being called to resolve other missing
   * properties on selection and/or on open.
   *
   * Will only ever be called once per TreeItem.
   *
   * onDidChangeTreeData should not be triggered from within resolveTreeItem.
   *
   * *Note* that this function is called when tree items are already showing in the UI.
   * Because of that, no property that changes the presentation (label, description, etc.)
   * can be changed.
   *
   * @param item Undefined properties of `item` should be set then `item` should be returned.
   * @param element The object associated with the TreeItem.
   * @param token A cancellation token.
   * @return The resolved tree item or a thenable that resolves to such. It is OK to return the given
   * `item`. When no result is returned, the given `item` will be used.
   */
  resolveTreeItem?(item: TreeItem, element: T, token: CancellationToken): ProviderResult<TreeItem>

  /**
   * Called with current element to resolve actions.
   * Called when user press 'actions' key.
   *
   * @param item Resolved item.
   * @param element The object under cursor.
   */
  resolveActions?(item: TreeItem, element: T): ProviderResult<TreeItemAction<T>[]>
}
