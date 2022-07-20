'use strict'
// vim: set sw=2 ts=2 sts=2 et foldmarker={{,}} foldmethod=marker foldlevel=0 nofen:
import { Buffer, Neovim, Window } from '@chemzqm/neovim'
import { CancellationToken, CodeAction, CodeActionKind, CreateFile, CreateFileOptions, DeleteFile, DeleteFileOptions, Disposable, DocumentSelector, Event, FormattingOptions, Location, Position, Range, RenameFile, RenameFileOptions, SymbolKind, TextDocumentEdit, TextDocumentSaveReason, TextEdit, WorkspaceEdit, WorkspaceFolder } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import Configurations from './configuration'
import Document from './model/document'
import { ProviderResult, TextDocumentContentProvider } from './provider'

declare global {
  namespace NodeJS {
    interface Global {
      __TEST__?: boolean
    }
  }
}
export type Optional<T extends object, K extends keyof T = keyof T> = Omit<
  T,
  K
> &
  Partial<Pick<T, K>>

export interface Thenable<T> {
  then<TResult>(onfulfilled?: (value: T) => TResult | Thenable<TResult>, onrejected?: (reason: any) => TResult | Thenable<TResult>): Thenable<TResult>
  // eslint-disable-next-line @typescript-eslint/unified-signatures
  then<TResult>(onfulfilled?: (value: T) => TResult | Thenable<TResult>, onrejected?: (reason: any) => void): Thenable<TResult>
}

export type ProviderName = 'rename' | 'onTypeEdit' | 'documentLink' | 'documentColor'
  | 'foldingRange' | 'format' | 'codeAction' | 'workspaceSymbols' | 'formatRange' | 'formatOnType'
  | 'hover' | 'signature' | 'documentSymbol' | 'documentHighlight' | 'definition'
  | 'declaration' | 'typeDefinition' | 'reference' | 'implementation'
  | 'codeLens' | 'selectionRange' | 'callHierarchy' | 'semanticTokens' | 'linkedEditing'

export type LocalMode = 'n' | 'v' | 's' | 'x'

export interface CurrentState {
  doc: Document
  winid: number
  position: Position
  // :h mode()
  mode: string
}

export interface BufferOption {
  readonly bufnr: number
  readonly eol: number
  readonly size: number
  readonly winid: number
  readonly lines: null | string[]
  readonly variables: { [key: string]: any }
  readonly bufname: string
  readonly fullpath: string
  readonly buftype: string
  readonly filetype: string
  readonly iskeyword: string
  readonly changedtick: number
  readonly previewwindow: number
  readonly indentkeys: string
}

export interface HandlerDelegate {
  checkProvier: (id: ProviderName, document: TextDocument) => void
  withRequestToken: <T> (name: string, fn: (token: CancellationToken) => Thenable<T>, checkEmpty?: boolean) => Promise<T>
  getCurrentState: () => Promise<CurrentState>
  addDisposable: (disposable: Disposable) => void
  getIcon(kind: SymbolKind): { text: string, hlGroup: string }
  getCodeActions(doc: Document, range?: Range, only?: CodeActionKind[]): Promise<ExtendedCodeAction[]>
  applyCodeAction(action: ExtendedCodeAction): Promise<void>
}

/*
 * With providerId so it can be resolved.
 */
export interface ExtendedCodeAction extends CodeAction {
  providerId: string
}

export interface FileSystemWatcher extends Disposable {
  ignoreCreateEvents: boolean
  ignoreChangeEvents: boolean
  ignoreDeleteEvents: boolean
  onDidCreate: Event<URI>
  onDidChange: Event<URI>
  onDidDelete: Event<URI>
}

export interface FloatConfig {
  border?: boolean
  rounded?: boolean
  highlight?: string
  title?: string
  borderhighlight?: string
  close?: boolean
  maxHeight?: number
  maxWidth?: number
  winblend?: number
  focusable?: boolean
  shadow?: boolean
}

export interface HighlightItemOption {
  /**
   * default to true
   */
  combine?: boolean
  /**
   * default to false
   */
  start_incl?: boolean
  /**
   * default to false
   */
  end_incl?: boolean
}

/**
 * Represent a highlight that not cross lines
 * all zero based.
 */
export interface HighlightItem extends HighlightItemOption {
  lnum: number
  hlGroup: string
  /**
   * 0 based start column.
   */
  colStart: number
  /**
   * 0 based end column.
   */
  colEnd: number
}

export interface BufferSyncItem {
  /**
   * Called on buffer unload.
   */
  dispose: () => void
  /**
   * Called on buffer change.
   */
  onChange?(e: DidChangeTextDocumentParams): void
}

export interface Env {
  completeOpt: string
  runtimepath: string
  readonly guicursor: string
  readonly tabCount: number
  readonly mode: string
  readonly apiversion: number
  readonly floating: boolean
  readonly sign: boolean
  readonly extensionRoot: string
  readonly globalExtensions: string[]
  readonly workspaceFolders: string[]
  readonly config: any
  readonly pid: number
  readonly columns: number
  readonly lines: number
  readonly pumevent: boolean
  readonly cmdheight: number
  readonly filetypeMap: { [index: string]: string }
  readonly isVim: boolean
  readonly isCygwin: boolean
  readonly isMacvim: boolean
  readonly isiTerm: boolean
  readonly version: string
  readonly locationlist: boolean
  readonly progpath: string
  readonly dialog: boolean
  readonly textprop: boolean
  readonly updateHighlight: boolean
  readonly vimCommands: CommandConfig[]
  readonly semanticHighlights: string[]
}

export interface CommandConfig {
  id: string
  cmd: string
  title?: string
}

export interface EditerState {
  document: TextDocument
  position: Position
}

/**
 * An output channel is a container for readonly textual information.
 *
 * To get an instance of an `OutputChannel` use
 * [createOutputChannel](#window.createOutputChannel).
 */
export interface OutputChannel {

  /**
   * The human-readable name of this output channel.
   */
  readonly name: string

  readonly content: string
  /**
   * Append the given value to the channel.
   *
   * @param value A string, falsy values will not be printed.
   */
  append(value: string): void

  /**
   * Append the given value and a line feed character
   * to the channel.
   *
   * @param value A string, falsy values will be printed.
   */
  appendLine(value: string): void

  /**
   * Removes output from the channel. Latest `keep` lines will be remained.
   */
  clear(keep?: number): void

  /**
   * Reveal this channel in the UI.
   *
   * @param preserveFocus When `true` the channel will not take focus.
   */
  show(preserveFocus?: boolean): void

  /**
   * Hide this channel from the UI.
   */
  hide(): void

  /**
   * Dispose and free associated resources.
   */
  dispose(): void
}

export interface KeymapOption {
  sync: boolean
  cancel: boolean
  silent: boolean
  repeat: boolean
}

export interface Autocmd {
  pattern?: string
  event: string | string[]
  arglist?: string[]
  request?: boolean
  thisArg?: any
  callback: Function
}

export interface UltiSnippetOption {
  regex?: string
  context?: string
  noPython?: boolean
}

export interface IWorkspace {
  readonly nvim: Neovim
  readonly cwd: string
  readonly root: string
  readonly isVim: boolean
  readonly isNvim: boolean
  readonly filetypes: Set<string>
  readonly languageIds: Set<string>
  readonly pluginRoot: string
  readonly completeOpt: string
  readonly channelNames: string[]
  readonly documents: Document[]
  readonly configurations: Configurations
  textDocuments: TextDocument[]
  onDidOpenTextDocument: Event<TextDocument & { bufnr: number }>
  onDidCloseTextDocument: Event<TextDocument & { bufnr: number }>
  onDidChangeTextDocument: Event<DidChangeTextDocumentParams>
  onWillSaveTextDocument: Event<TextDocumentWillSaveEvent>
  onDidSaveTextDocument: Event<TextDocument>
  onDidChangeConfiguration: Event<ConfigurationChangeEvent>
  findUp(filename: string | string[]): Promise<string | null>
  getDocument(uri: number | string): Document
  getFormatOptions(uri?: string): Promise<FormattingOptions>
  getConfigFile(target: ConfigurationTarget): string
  applyEdit(edit: WorkspaceEdit): Promise<boolean>
  createFileSystemWatcher(globPattern: string, ignoreCreate?: boolean, ignoreChange?: boolean, ignoreDelete?: boolean): FileSystemWatcher
  getConfiguration(section?: string, _resource?: string): WorkspaceConfiguration
  registerTextDocumentContentProvider(scheme: string, provider: TextDocumentContentProvider): Disposable
  getWorkspaceFolder(uri: string): WorkspaceFolder | undefined
  getQuickfixItem(loc: Location, text?: string, type?: string): Promise<QuickfixItem>
  getQuickfixList(locations: Location[]): Promise<ReadonlyArray<QuickfixItem>>
  getLine(uri: string, line: number): Promise<string>
  readFile(uri: string): Promise<string>
  jumpTo(uri: string, position: Position): Promise<void>
  createFile(filepath: string, opts?: CreateFileOptions): Promise<void>
  renameFile(oldPath: string, newPath: string, opts?: RenameFileOptions): Promise<void>
  deleteFile(filepath: string, opts?: DeleteFileOptions): Promise<void>
  openResource(uri: string): Promise<void>
  resolveModule(name: string): Promise<string>
  match(selector: DocumentSelector, document: TextDocument): number
  runCommand(cmd: string, cwd?: string, timeout?: number): Promise<string>
  dispose(): void
}

// window {{
export type MsgTypes = 'error' | 'warning' | 'more'
export type HighlightItemResult = [string, number, number, number, number?]
export type HighlightItemDef = [string, number, number, number, number?, number?, number?]

export interface HighlightDiff {
  remove: number[]
  removeMarkers: number[]
  add: HighlightItemDef[]
}

export interface StatusItemOption {
  progress?: boolean
}

export interface ScreenPosition {
  row: number
  col: number
}

export interface OpenTerminalOption {
  /**
   * Cwd of terminal, default to result of |getcwd()|
   */
  cwd?: string
  /**
   * Close terminal on job finish, default to true.
   */
  autoclose?: boolean
  /**
   * Keep focus current window, default to false.
   */
  keepfocus?: boolean
  /**
   * Position of terminal window, default to 'right'.
   */
  position?: 'bottom' | 'right'
}

export interface TerminalResult {
  bufnr: number
  success: boolean
  content?: string
}
/**
 * Value-object describing where and how progress should show.
 */
export interface ProgressOptions {

  /**
   * A human-readable string which will be used to describe the
   * operation.
   */
  title?: string

  /**
   * Controls if a cancel button should show to allow the user to
   * cancel the long running operation.
   */
  cancellable?: boolean
  /**
   * Extension or language-client id
   */
  source?: string
}

/**
 * Represents an action that is shown with an information, warning, or
 * error message.
 *
 * @see [showInformationMessage](#window.showInformationMessage)
 * @see [showWarningMessage](#window.showWarningMessage)
 * @see [showErrorMessage](#window.showErrorMessage)
 */
export interface MessageItem {

  /**
   * A short title like 'Retry', 'Open Log' etc.
   */
  title: string

  /**
   * A hint for modal dialogs that the item should be triggered
   * when the user cancels the dialog (e.g. by pressing the ESC
   * key).
   *
   * Note: this option is ignored for non-modal messages.
   * Note: not used by coc.nvim for now.
   */
  isCloseAffordance?: boolean
}

export type MenuOption = {
  title?: string,
  content?: string
  /**
   * Create and highlight shortcut characters.
   */
  shortcuts?: boolean
  /**
   * Position of menu picker, default to 'cursor'
   */
  position?: 'cursor' | 'center'
  /**
   * Border highlight that override user configuration.
   */
  borderhighlight?: string
} | string
// }}

// vim {{
export interface LocationListItem {
  bufnr: number
  lnum: number
  end_lnum: number
  col: number
  end_col: number
  text: string
  type: string
}

export interface QuickfixItem {
  uri?: string
  module?: string
  range?: Range
  text?: string
  type?: string
  filename?: string
  bufnr?: number
  lnum?: number
  end_lnum?: number
  col?: number
  end_col?: number
  valid?: boolean
  nr?: number
}

/**
 * Options to configure the behavior of the quick pick UI.
 */
export interface QuickPickOptions {

  /**
   * An optional string that represents the title of the quick pick.
   */
  title?: string

  /**
   * An optional flag to include the description when filtering the picks.
   */
  matchOnDescription?: boolean

  /**
   * An optional flag to make the picker accept multiple selections, if true the result is an array of picks.
   */
  canPickMany?: boolean
}

/**
 * Represents an item that can be selected from
 * a list of items.
 */
export interface QuickPickItem {
  /**
   * A human-readable string which is rendered prominent
   */
  label: string
  /**
   * A human-readable string which is rendered less prominent in the same line
   */
  description?: string

  /**
   * Optional flag indicating if this item is picked initially.
   */
  picked?: boolean
}
// }}

// Enums{{
export enum PatternType {
  Buffer,
  LanguageServer,
  Global,
}

export enum SourceType {
  Native,
  Remote,
  Service,
}

export enum MessageLevel {
  More,
  Warning,
  Error
}

export enum ConfigurationTarget {
  Global,
  User,
  Workspace
}

export enum ServiceStat {
  Initial,
  Starting,
  StartFailed,
  Running,
  Stopping,
  Stopped,
}

export enum FileType {
  /**
   * The file type is unknown.
   */
  Unknown = 0,
  /**
   * A regular file.
   */
  File = 1,
  /**
   * A directory.
   */
  Directory = 2,
  /**
   * A symbolic link to a file.
   */
  SymbolicLink = 64
}
// }}

// TextDocument {{
/**
 * An event that is fired when a [document](#TextDocument) will be saved.
 *
 * To make modifications to the document before it is being saved, call the
 * [`waitUntil`](#TextDocumentWillSaveEvent.waitUntil)-function with a thenable
 * that resolves to an array of [text edits](#TextEdit).
 */
export interface TextDocumentWillSaveEvent {

  /**
   * The document that will be saved.
   */
  document: TextDocument

  /**
   * The reason why save was triggered.
   */
  reason: TextDocumentSaveReason

  /**
   * Allows to pause the event loop and to apply [pre-save-edits](#TextEdit).
   * Edits of subsequent calls to this function will be applied in order. The
   * edits will be *ignored* if concurrent modifications of the document happened.
   *
   * *Note:* This function can only be called during event dispatch and not
   * in an asynchronous manner:
   *
   * @param thenable A thenable that resolves to [pre-save-edits](#TextEdit).
   */
  waitUntil(thenable: Thenable<TextEdit[] | any>): void
}

export type DocumentChange = TextDocumentEdit | CreateFile | RenameFile | DeleteFile

export interface LinesChange {
  uri: string
  lnum: number
  oldLines: ReadonlyArray<string>
  newLines: ReadonlyArray<string>
}

/**
 * An event describing a change to a text document.
 */
export interface TextDocumentContentChange {
  /**
   * The range of the document that changed.
   */
  range: Range
  /**
   * The optional length of the range that got replaced.
   *
   * @deprecated use range instead.
   */
  rangeLength?: number
  /**
   * The new text for the provided range.
   */
  text: string
}

export interface DidChangeTextDocumentParams {
  /**
   * The document that did change. The version number points
   * to the version after all provided content changes have
   * been applied.
   */
  readonly textDocument: {
    version: number
    uri: string
  }
  /**
   * The actual content changes. The content changes describe single state changes
   * to the document. So if there are two content changes c1 (at array index 0) and
   * c2 (at array index 1) for a document in state S then c1 moves the document from
   * S to S' and c2 from S' to S''. So c1 is computed on the state S and c2 is computed
   * on the state S'.
   */
  readonly contentChanges: ReadonlyArray<TextDocumentContentChange>
  /**
   * Buffer number of document.
   */
  readonly bufnr: number
  /**
   * Original content before change
   */
  readonly original: string
  /**
   * Changed lines
   */
  readonly originalLines: ReadonlyArray<string>
}
// }}

// Completion {{
export interface Documentation {
  filetype: string
  content: string
  highlights?: HighlightItem[]
  active?: [number, number]
}

export interface VimCompleteItem {
  word: string
  abbr?: string
  menu?: string
  info?: string
  kind?: string
  icase?: number
  equal?: number
  dup?: number
  empty?: number
  user_data?: string
}

export interface CompleteDoneItem {
  readonly word: string
  readonly user_data?: string
  // if there's ClosePum event just received.
  close?: boolean
  // already cancelled by completion.stop()
  closed?: boolean
}

export interface ExtendedCompleteItem extends VimCompleteItem {
  score?: number
  sortText?: string
  sourceScore?: number
  filterText?: string
  isSnippet?: boolean
  source?: string
  matchScore?: number
  priority?: number
  preselect?: boolean
  signature?: string
  localBonus?: number
  index?: number
  // used for preview
  documentation?: Documentation[]
  detailShown?: number
  resolved?: boolean
  // saved line for apply TextEdit
  line?: string
  recentScore?: number
}

export interface CompleteResult {
  items: ExtendedCompleteItem[]
  isIncomplete?: boolean
  startcol?: number
  priority?: number
}

// option on complete & should_complete
// what need change? line, col, input, colnr, changedtick
// word = '', triggerForInComplete = false
export interface CompleteOption {
  readonly bufnr: number
  readonly line: string
  col: number
  input: string
  filetype: string
  readonly filepath: string
  readonly word: string
  // cursor position
  colnr: number
  synname?: string
  readonly linenr: number
  readonly source?: string
  readonly blacklist: string[]
  readonly disabled: ReadonlyArray<string>
  readonly changedtick: number
  readonly indentkeys: string
  readonly triggerCharacter?: string
  triggerForInComplete?: boolean
}

export interface SourceStat {
  name: string
  priority: number
  triggerCharacters: string[]
  type: string
  shortcut: string
  filepath: string
  disabled: boolean
  filetypes: string[]
}

export type SourceConfig = Omit<Partial<ISource>, 'shortcut' | 'priority' | 'triggerOnly' | 'triggerCharacters' | 'triggerPatterns' | 'enable' | 'filetypes' | 'disableSyntaxes'>

export interface ISource {
  name: string
  enable?: boolean
  shortcut?: string
  priority?: number
  sourceType?: SourceType
  optionalFns?: string[]
  triggerCharacters?: string[]
  triggerOnly?: boolean
  triggerPatterns?: RegExp[]
  disableSyntaxes?: string[]
  isSnippet?: boolean
  filetypes?: string[]
  documentSelector?: DocumentSelector
  filepath?: string
  firstMatch?: boolean
  refresh?(): Promise<void>
  toggle?(): void
  onEnter?(bufnr: number): void
  shouldComplete?(opt: CompleteOption): Promise<boolean>
  doComplete(opt: CompleteOption, token: CancellationToken): ProviderResult<CompleteResult | null>
  onCompleteResolve?(item: ExtendedCompleteItem, token: CancellationToken): ProviderResult<void> | void
  onCompleteDone?(item: ExtendedCompleteItem, opt: CompleteOption): ProviderResult<void>
  shouldCommit?(item: ExtendedCompleteItem, character: string): boolean
}
// }}

// Configuration {{
/**
 * An event describing the change in Configuration
 */
export interface ConfigurationChangeEvent {

  /**
   * Returns `true` if the given section for the given resource (if provided) is affected.
   *
   * @param section Configuration name, supports _dotted_ names.
   * @param resource A resource URI.
   * @return `true` if the given section for the given resource (if provided) is affected.
   */
  affectsConfiguration(section: string, resource?: string): boolean
}

export interface WorkspaceConfiguration {
  /**
   * Return a value from this configuration.
   *
   * @param section Configuration name, supports _dotted_ names.
   * @return The value `section` denotes or `undefined`.
   */
  get<T>(section: string): T | undefined

  /**
   * Return a value from this configuration.
   *
   * @param section Configuration name, supports _dotted_ names.
   * @param defaultValue A value should be returned when no value could be found, is `undefined`.
   * @return The value `section` denotes or the default.
   */
  get<T>(section: string, defaultValue: T): T

  /**
   * Check if this configuration has a certain value.
   *
   * @param section Configuration name, supports _dotted_ names.
   * @return `true` if the section doesn't resolve to `undefined`.
   */
  has(section: string): boolean

  /**
   * Retrieve all information about a configuration setting. A configuration value
   * often consists of a *default* value, a global or installation-wide value,
   * a workspace-specific value
   *
   * *Note:* The configuration name must denote a leaf in the configuration tree
   * (`editor.fontSize` vs `editor`) otherwise no result is returned.
   *
   * @param section Configuration name, supports _dotted_ names.
   * @return Information about a configuration setting or `undefined`.
   */
  inspect<T>(section: string): ConfigurationInspect<T> | undefined
  /**
   * Update a configuration value. The updated configuration values are persisted.
   *
   *
   * @param section Configuration name, supports _dotted_ names.
   * @param value The new value.
   * @param isUser if true, always update user configuration
   */
  update(section: string, value: any, isUser?: boolean): void

  /**
   * Readable dictionary that backs this configuration.
   */
  readonly [key: string]: any
}

export interface ErrorItem {
  location: Location
  message: string
}

export interface ConfigurationInspect<T> {
  key: string
  defaultValue?: T
  globalValue?: T
  workspaceValue?: T
}

export interface IConfigurationOverrides {
  overrideIdentifier?: string | null
  resource?: URI | null
}

export interface ConfigurationShape {
  /**
   * Resolve possible workspace config from resource.
   */
  getWorkspaceConfig?(resource?: string): URI | undefined
  $updateConfigurationOption(target: ConfigurationTarget, key: string, value: any, overrides?: IConfigurationOverrides): void
  $removeConfigurationOption(target: ConfigurationTarget, key: string, overrides?: IConfigurationOverrides): void
}

export interface IConfigurationModel {
  contents: any
}

export interface IConfigurationData {
  defaults: IConfigurationModel
  user: IConfigurationModel
  workspace: IConfigurationModel
}
// }}

// File operation {{
/**
 * An event that is fired when files are going to be renamed.
 *
 * To make modifications to the workspace before the files are renamed,
 * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
 * thenable that resolves to a [workspace edit](#WorkspaceEdit).
 */
export interface FileWillRenameEvent {

  /**
   * The files that are going to be renamed.
   */
  readonly files: ReadonlyArray<{ oldUri: URI, newUri: URI }>

  /**
   * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
   *
   * *Note:* This function can only be called during event dispatch and not
   * in an asynchronous manner:
   *
   * ```ts
   * workspace.onWillCreateFiles(event => {
   * 	// async, will *throw* an error
   * 	setTimeout(() => event.waitUntil(promise));
   *
   * 	// sync, OK
   * 	event.waitUntil(promise);
   * })
   * ```
   *
   * @param thenable A thenable that delays saving.
   */
  waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
}

/**
 * An event that is fired after files are renamed.
 */
export interface FileRenameEvent {

  /**
   * The files that got renamed.
   */
  readonly files: ReadonlyArray<{ oldUri: URI, newUri: URI }>
}

/**
 * An event that is fired when files are going to be created.
 *
 * To make modifications to the workspace before the files are created,
 * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
 * thenable that resolves to a [workspace edit](#WorkspaceEdit).
 */
export interface FileWillCreateEvent {

  /**
   * A cancellation token.
   */
  readonly token: CancellationToken

  /**
   * The files that are going to be created.
   */
  readonly files: ReadonlyArray<URI>

  /**
   * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
   *
   * *Note:* This function can only be called during event dispatch and not
   * in an asynchronous manner:
   *
   * ```ts
   * workspace.onWillCreateFiles(event => {
   *     // async, will *throw* an error
   *     setTimeout(() => event.waitUntil(promise));
   *
   *     // sync, OK
   *     event.waitUntil(promise);
   * })
   * ```
   *
   * @param thenable A thenable that delays saving.
   */
  waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
}

/**
 * An event that is fired after files are created.
 */
export interface FileCreateEvent {

  /**
   * The files that got created.
   */
  readonly files: ReadonlyArray<URI>
}

/**
 * An event that is fired when files are going to be deleted.
 *
 * To make modifications to the workspace before the files are deleted,
 * call the [`waitUntil](#FileWillCreateEvent.waitUntil)-function with a
 * thenable that resolves to a [workspace edit](#WorkspaceEdit).
 */
export interface FileWillDeleteEvent {

  /**
   * The files that are going to be deleted.
   */
  readonly files: ReadonlyArray<URI>

  /**
   * Allows to pause the event and to apply a [workspace edit](#WorkspaceEdit).
   *
   * *Note:* This function can only be called during event dispatch and not
   * in an asynchronous manner:
   *
   * ```ts
   * workspace.onWillCreateFiles(event => {
   *     // async, will *throw* an error
   *     setTimeout(() => event.waitUntil(promise));
   *
   *     // sync, OK
   *     event.waitUntil(promise);
   * })
   * ```
   *
   * @param thenable A thenable that delays saving.
   */
  waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
}

/**
 * An event that is fired after files are deleted.
 */
export interface FileDeleteEvent {

  /**
   * The files that got deleted.
   */
  readonly files: ReadonlyArray<URI>
}
// }}

// List {{
export interface LocationWithLine {
  uri: string
  line: string
  text?: string
}

export interface ListItem {
  label: string
  filterText?: string
  preselect?: boolean
  location?: Location | LocationWithLine | string
  data?: any
  ansiHighlights?: AnsiHighlight[]
  resolved?: boolean
  /**
   * A string that should be used when comparing this item
   * with other items, only used for fuzzy filter.
   */
  sortText?: string
  converted?: boolean
}

export interface ListHighlights {
  // column indexes
  spans: [number, number][]
  hlGroup?: string
}

export interface ListItemWithHighlights extends ListItem {
  score?: number
  highlights?: ListHighlights
}

export interface AnsiHighlight {
  span: [number, number]
  hlGroup: string
}

export interface ListItemsEvent {
  items: ListItem[]
  finished: boolean
  append?: boolean
  reload?: boolean
}

export type ListMode = 'normal' | 'insert'

export type Matcher = 'strict' | 'fuzzy' | 'regex'

export interface ListOptions {
  position: string
  reverse: boolean
  input: string
  ignorecase: boolean
  interactive: boolean
  sort: boolean
  mode: ListMode
  matcher: Matcher
  autoPreview: boolean
  numberSelect: boolean
  noQuit: boolean
  first: boolean
}

export interface ListContext {
  args: string[]
  input: string
  cwd: string
  options: ListOptions
  window: Window
  buffer: Buffer
  listWindow: Window
}

export interface ListAction {
  name: string
  persist?: boolean
  reload?: boolean
  parallel?: boolean
  multiple?: boolean
  tabPersist?: boolean
  execute: (item: ListItem | ListItem[], context: ListContext) => ProviderResult<void>
}

export interface ListTask {
  on(event: 'data', callback: (item: ListItem) => void): void
  on(event: 'end', callback: () => void): void
  on(event: 'error', callback: (msg: string | Error) => void): void
  dispose(): void
}

export interface ListArgument {
  key?: string
  hasValue?: boolean
  name: string
  description: string
}

export interface IList {
  /**
   * Unique name of list.
   */
  name: string
  /**
   * Action list.
   */
  actions: ListAction[]
  /**
   * Default action name.
   */
  defaultAction: string
  /**
   * Load list items.
   */
  loadItems(context: ListContext, token: CancellationToken): Promise<ListItem[] | ListTask | null | undefined>
  /**
   * Resolve list item.
   */
  resolveItem?(item: ListItem): Promise<ListItem | null>
  /**
   * Should be true when interactive is supported.
   */
  interactive?: boolean
  /**
   * Description of list.
   */
  description?: string
  /**
   * Detail description, shown in help.
   */
  detail?: string
  /**
   * Options supported by list.
   */
  options?: ListArgument[]
  /**
   * Highlight buffer by vim's syntax commands.
   */
  doHighlight?(): void
  dispose?(): void
}
// }}
