'use strict'
import { NeovimClient as Neovim } from '@chemzqm/neovim'
import fs from 'fs-extra'
import os from 'os'
import path from 'path'
import { CancellationToken, CreateFileOptions, DeleteFileOptions, Disposable, DocumentSelector, Event, FormattingOptions, Location, LocationLink, Position, RenameFileOptions, WorkspaceEdit, WorkspaceFolder, WorkspaceFoldersChangeEvent } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import { version as VERSION } from '../package.json'
import Configurations from './configuration'
import ConfigurationShape from './configuration/shape'
import Autocmds from './core/autocmds'
import channels from './core/channels'
import ContentProvider from './core/contentProvider'
import Documents from './core/documents'
import Files, { GlobPattern } from './core/files'
import { FileSystemWatcher, FileSystemWatcherManager } from './core/fileSystemWatcher'
import { createNameSpace, findUp, getWatchmanPath, has, resolveModule, score } from './core/funcs'
import Keymaps from './core/keymaps'
import Locations from './core/locations'
import * as ui from './core/ui'
import Watchers from './core/watchers'
import Editors from './core/editors'
import WorkspaceFolderController from './core/workspaceFolder'
import events from './events'
import BufferSync, { SyncItem } from './model/bufferSync'
import DB from './model/db'
import type Document from './model/document'
import Mru from './model/mru'
import Task from './model/task'
import { LinesTextDocument } from './model/textdocument'
import { TextDocumentContentProvider } from './provider'
import { Autocmd, ConfigurationChangeEvent, ConfigurationTarget, DidChangeTextDocumentParams, EditerState, Env, FileCreateEvent, FileDeleteEvent, FileRenameEvent, FileWillCreateEvent, FileWillDeleteEvent, FileWillRenameEvent, IWorkspace, KeymapOption, LocalMode, QuickfixItem, TextDocumentWillSaveEvent, WorkspaceConfiguration } from './types'
import { CONFIG_FILE_NAME, MapMode, runCommand } from './util/index'

const APIVERSION = 30
const logger = require('./util/logger')('workspace')
const methods = [
  'showMessage', 'runTerminalCommand', 'openTerminal', 'showQuickpick',
  'menuPick', 'openLocalConfig', 'showPrompt', 'createStatusBarItem', 'createOutputChannel',
  'showOutputChannel', 'requestInput', 'echoLines', 'getCursorPosition', 'moveTo',
  'getOffset', 'getSelectedRange', 'selectRange', 'createTerminal',
]

export class Workspace implements IWorkspace {
  public readonly onDidChangeConfiguration: Event<ConfigurationChangeEvent>
  public readonly onDidOpenTextDocument: Event<LinesTextDocument & { bufnr: number }>
  public readonly onDidCloseTextDocument: Event<LinesTextDocument & { bufnr: number }>
  public readonly onDidChangeTextDocument: Event<DidChangeTextDocumentParams>
  public readonly onDidSaveTextDocument: Event<LinesTextDocument>
  public readonly onWillSaveTextDocument: Event<TextDocumentWillSaveEvent>
  public readonly onDidChangeWorkspaceFolders: Event<WorkspaceFoldersChangeEvent>
  public readonly onDidRuntimePathChange: Event<string[]>
  public readonly onDidCreateFiles: Event<FileCreateEvent>
  public readonly onDidRenameFiles: Event<FileRenameEvent>
  public readonly onDidDeleteFiles: Event<FileDeleteEvent>
  public readonly onWillCreateFiles: Event<FileWillCreateEvent>
  public readonly onWillRenameFiles: Event<FileWillRenameEvent>
  public readonly onWillDeleteFiles: Event<FileWillDeleteEvent>
  public readonly nvim: Neovim
  public readonly version: string
  public readonly configurations: Configurations
  public readonly workspaceFolderControl: WorkspaceFolderController
  public readonly documentsManager: Documents
  public readonly contentProvider: ContentProvider
  public readonly autocmds: Autocmds
  public readonly watchers: Watchers
  public readonly keymaps: Keymaps
  public readonly locations: Locations
  public readonly files: Files
  public readonly fileSystemWatchers: FileSystemWatcherManager
  public readonly editors: Editors

  private _env: Env

  constructor() {
    this.version = VERSION
    let home = path.normalize(process.env.COC_VIMCONFIG) || path.join(os.homedir(), '.vim')
    let userConfigFile = path.join(home, CONFIG_FILE_NAME)
    this.configurations = new Configurations(userConfigFile, new ConfigurationShape(this))
    this.workspaceFolderControl = new WorkspaceFolderController(this.configurations)
    let documents = this.documentsManager = new Documents(this.configurations, this.workspaceFolderControl)
    this.contentProvider = new ContentProvider(documents)
    this.watchers = new Watchers()
    this.autocmds = new Autocmds(this.contentProvider, this.watchers)
    this.keymaps = new Keymaps(documents)
    this.locations = new Locations(this.configurations, documents, this.contentProvider)
    this.files = new Files(documents, this.configurations, this.workspaceFolderControl, this.keymaps)
    this.editors = new Editors(documents)
    this.onDidRuntimePathChange = this.watchers.onDidRuntimePathChange
    this.onDidChangeWorkspaceFolders = this.workspaceFolderControl.onDidChangeWorkspaceFolders
    this.onDidChangeConfiguration = this.configurations.onDidChange
    this.onDidOpenTextDocument = documents.onDidOpenTextDocument
    this.onDidChangeTextDocument = documents.onDidChangeDocument
    this.onDidCloseTextDocument = documents.onDidCloseDocument
    this.onDidSaveTextDocument = documents.onDidSaveTextDocument
    this.onWillSaveTextDocument = documents.onWillSaveTextDocument
    this.onDidCreateFiles = this.files.onDidCreateFiles
    this.onDidRenameFiles = this.files.onDidRenameFiles
    this.onDidDeleteFiles = this.files.onDidDeleteFiles
    this.onWillCreateFiles = this.files.onWillCreateFiles
    this.onWillRenameFiles = this.files.onWillRenameFiles
    this.onWillDeleteFiles = this.files.onWillDeleteFiles
    let watchmanPath = global.__TEST__ ? null : this.getWatchmanPath()
    this.fileSystemWatchers = new FileSystemWatcherManager(this.workspaceFolderControl, watchmanPath)
  }

  public async init(window: any): Promise<void> {
    let { nvim } = this
    for (let method of methods) {
      Object.defineProperty(this, method, {
        get: () => {
          return (...args: any[]) => {
            let stack = '\n' + Error().stack.split('\n').slice(2, 4).join('\n')
            logger.warn(`workspace.${method} is deprecated, please use window.${method} instead.`, stack)
            return window[method].apply(window, args)
          }
        }
      })
    }
    for (let name of ['onDidOpenTerminal', 'onDidCloseTerminal']) {
      Object.defineProperty(this, name, {
        get: () => {
          let stack = '\n' + Error().stack.split('\n').slice(2, 4).join('\n')
          logger.warn(`workspace.${name} is deprecated, please use window.${name} instead.`, stack)
          return window[name]
        }
      })
    }
    let env = this._env = await nvim.call('coc#util#vim_info') as Env
    window.init(env)
    if (this._env.apiversion != APIVERSION) {
      nvim.echoError(`API version ${this._env.apiversion} is not ${APIVERSION}, please build coc.nvim by 'yarn install' after pull source code.`)
    }
    this.workspaceFolderControl.setWorkspaceFolders(this._env.workspaceFolders)
    this.configurations.updateUserConfig(this._env.config)
    this.files.attach(nvim, env, window)
    this.contentProvider.attach(nvim)
    this.keymaps.attach(nvim)
    this.autocmds.attach(nvim, env)
    this.locations.attach(nvim, env)
    this.watchers.attach(nvim, env)
    await this.attach()
    await this.editors.attach(nvim)
    let channel = channels.create('watchman', nvim)
    this.fileSystemWatchers.attach(channel)
  }

  public get cwd(): string {
    return this.documentsManager.cwd
  }

  public get env(): Env {
    return this._env
  }

  public get root(): string {
    return this.documentsManager.root || this.cwd
  }

  public get rootPath(): string {
    return this.root
  }

  public get bufnr(): number {
    return this.documentsManager.bufnr
  }

  /**
   * @deprecated
   */
  public get insertMode(): boolean {
    return events.insertMode
  }

  public get floatSupported(): boolean {
    return this.env.floating || this.env.textprop
  }

  /**
   * @deprecated
   */
  public get uri(): string {
    return this.documentsManager.uri
  }

  /**
   * @deprecated
   */
  public get workspaceFolder(): WorkspaceFolder {
    return this.workspaceFolders[0]
  }

  public get textDocuments(): TextDocument[] {
    return this.documentsManager.textDocuments
  }

  public get documents(): Document[] {
    return this.documentsManager.documents
  }

  public get document(): Promise<Document | undefined> {
    return this.documentsManager.document
  }

  public get workspaceFolders(): ReadonlyArray<WorkspaceFolder> {
    return this.workspaceFolderControl.workspaceFolders
  }

  public get folderPaths(): string[] {
    return this.workspaceFolders.map(f => URI.parse(f.uri).fsPath)
  }

  public get channelNames(): string[] {
    return channels.names
  }

  public get pluginRoot(): string {
    return path.dirname(__dirname)
  }

  public get isVim(): boolean {
    return this._env.isVim
  }

  public get isNvim(): boolean {
    return !this._env.isVim
  }

  public get completeOpt(): string {
    return this._env.completeOpt
  }

  public get filetypes(): Set<string> {
    return this.documentsManager.filetypes
  }

  public get languageIds(): Set<string> {
    return this.documentsManager.languageIds
  }

  /**
   * @deprecated
   */
  public createNameSpace(name: string): number {
    return createNameSpace(name)
  }

  public getConfigFile(target: ConfigurationTarget): string {
    return this.configurations.getConfigFile(target)
  }

  public has(feature: string): boolean {
    return has(this.env, feature)
  }

  /**
   * Register autocmd on vim.
   */
  public registerAutocmd(autocmd: Autocmd): Disposable {
    return this.autocmds.registerAutocmd(autocmd)
  }

  /**
   * Watch for option change.
   */
  public watchOption(key: string, callback: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void {
    this.watchers.watchOption(key, callback, disposables)
  }

  /**
   * Watch global variable, works on neovim only.
   */
  public watchGlobal(key: string, callback?: (oldValue: any, newValue: any) => Thenable<void> | void, disposables?: Disposable[]): void {
    this.watchers.watchGlobal(key, callback || function() {}, disposables)
  }

  /**
   * Check if selector match document.
   */
  public match(selector: DocumentSelector, document: { uri: string, languageId: string }): number {
    return score(selector, document.uri, document.languageId)
  }

  /**
   * Create a FileSystemWatcher instance, doesn't fail when watchman not found.
   */
  public createFileSystemWatcher(globPattern: string, ignoreCreate?: boolean, ignoreChange?: boolean, ignoreDelete?: boolean): FileSystemWatcher {
    return this.fileSystemWatchers.createFileSystemWatcher(globPattern, ignoreCreate, ignoreChange, ignoreDelete)
  }

  public getWatchmanPath(): string | null {
    return getWatchmanPath(this.configurations)
  }

  /**
   * Get configuration by section and optional resource uri.
   */
  public getConfiguration(section?: string, resource?: string): WorkspaceConfiguration {
    return this.configurations.getConfiguration(section, resource)
  }

  /**
   * Get created document by uri or bufnr.
   */
  public getDocument(uri: number | string): Document | null {
    return this.documentsManager.getDocument(uri)
  }

  public isAttached(bufnr: number): boolean {
    let doc = this.documentsManager.getDocument(bufnr)
    return doc != null && doc.attached
  }

  /**
   * Get attached document by uri or bufnr.
   * Throw error when document doesn't exist or isn't attached.
   */
  public getAttachedDocument(uri: number | string): Document {
    let doc = this.getDocument(uri)
    if (!doc) throw new Error(`Buffer ${uri} not created.`)
    if (!doc.attached) throw new Error(`Buffer ${uri} not attached, try :CocCommand document.checkBuffer`)
    return doc
  }
  /**
   * Convert location to quickfix item.
   */
  public getQuickfixItem(loc: Location | LocationLink, text?: string, type = '', module?: string): Promise<QuickfixItem> {
    return this.documentsManager.getQuickfixItem(loc, text, type, module)
  }

  /**
   * Create persistence Mru instance.
   */
  public createMru(name: string): Mru {
    return new Mru(name)
  }

  public async getQuickfixList(locations: Location[]): Promise<ReadonlyArray<QuickfixItem>> {
    return this.documentsManager.getQuickfixList(locations)
  }

  /**
   * Populate locations to UI.
   */
  public async showLocations(locations: Location[]): Promise<void> {
    await this.locations.showLocations(locations)
  }

  /**
   * Get content of line by uri and line.
   */
  public getLine(uri: string, line: number): Promise<string> {
    return this.documentsManager.getLine(uri, line)
  }

  /**
   * Get WorkspaceFolder of uri
   */
  public getWorkspaceFolder(uri: string): WorkspaceFolder | undefined {
    return this.workspaceFolderControl.getWorkspaceFolder(URI.parse(uri))
  }

  /**
   * Get content from buffer or file by uri.
   */
  public readFile(uri: string): Promise<string> {
    return this.documentsManager.readFile(uri)
  }

  public async getCurrentState(): Promise<EditerState> {
    let document = await this.document
    let position = await ui.getCursorPosition(this.nvim)
    return {
      document: document.textDocument,
      position
    }
  }

  public async getFormatOptions(uri?: string): Promise<FormattingOptions> {
    return this.documentsManager.getFormatOptions(uri)
  }

  /**
   * Resolve module from yarn or npm.
   */
  public resolveModule(name: string): Promise<string> {
    return resolveModule(name)
  }

  /**
   * Run nodejs command
   */
  public async runCommand(cmd: string, cwd?: string, timeout?: number): Promise<string> {
    cwd = cwd || this.cwd
    return runCommand(cmd, { cwd }, timeout)
  }

  /**
   * Expand filepath with `~` and/or environment placeholders
   */
  public expand(filepath: string): string {
    return this.documentsManager.expand(filepath)
  }

  public async callAsync<T>(method: string, args: any[]): Promise<T> {
    if (this.isNvim) return await this.nvim.call(method, args)
    return await this.nvim.callAsync('coc#util#with_callback', [method, args])
  }

  public registerTextDocumentContentProvider(scheme: string, provider: TextDocumentContentProvider): Disposable {
    return this.contentProvider.registerTextDocumentContentProvider(scheme, provider)
  }

  public registerKeymap(modes: MapMode[], key: string, fn: Function, opts: Partial<KeymapOption> = {}): Disposable {
    return this.keymaps.registerKeymap(modes, key, fn, opts)
  }

  public registerExprKeymap(mode: 'i' | 'n' | 'v' | 's' | 'x', key: string, fn: Function, buffer = false): Disposable {
    return this.keymaps.registerExprKeymap(mode, key, fn, buffer)
  }

  public registerLocalKeymap(mode: LocalMode, key: string, fn: Function, notify = false): Disposable {
    return this.keymaps.registerLocalKeymap(mode, key, fn, notify)
  }

  /**
   * Create Task instance that runs in vim.
   */
  public createTask(id: string): Task {
    return new Task(this.nvim, id)
  }

  /**
   * Create DB instance at extension root.
   */
  public createDatabase(name: string): DB {
    let root: string
    if (global.hasOwnProperty('__TEST__')) {
      root = path.join(os.tmpdir(), `coc-${process.pid}`)
      fs.mkdirpSync(root)
    } else {
      root = path.dirname(this.env.extensionRoot)
    }
    let filepath = path.join(root, name + '.json')
    return new DB(filepath)
  }

  public registerBufferSync<T extends SyncItem>(create: (doc: Document) => T | undefined): BufferSync<T> {
    return new BufferSync(create, this.documentsManager)
  }

  public async attach(): Promise<void> {
    await this.documentsManager.attach(this.nvim, this._env)
  }

  public jumpTo(uri: string, position?: Position | null, openCommand?: string): Promise<void> {
    return this.files.jumpTo(uri, position, openCommand)
  }

  /**
   * Findup for filename or filenames from current filepath or root.
   */
  public findUp(filename: string | string[]): Promise<string | null> {
    return findUp(this.nvim, this.cwd, filename)
  }

  /**
   * Apply WorkspaceEdit.
   */
  public applyEdit(edit: WorkspaceEdit): Promise<boolean> {
    return this.files.applyEdit(edit)
  }

  /**
   * Create a file in vim and disk
   */
  public createFile(filepath: string, opts: CreateFileOptions = {}): Promise<void> {
    return this.files.createFile(filepath, opts)
  }

  /**
   * Load uri as document.
   */
  public loadFile(uri: string): Promise<Document> {
    return this.files.loadResource(uri)
  }

  /**
   * Load the files that not loaded
   */
  public async loadFiles(uris: string[]): Promise<(Document | undefined)[]> {
    return this.files.loadResources(uris)
  }

  /**
   * Rename file in vim and disk
   */
  public async renameFile(oldPath: string, newPath: string, opts: RenameFileOptions = {}): Promise<void> {
    await this.files.renameFile(oldPath, newPath, opts)
  }

  /**
   * Delete file from vim and disk.
   */
  public async deleteFile(filepath: string, opts: DeleteFileOptions = {}): Promise<void> {
    await this.files.deleteFile(filepath, opts)
  }

  public async renameCurrent(): Promise<void> {
    await this.files.renameCurrent()
  }

  /**
   * Open resource by uri
   */
  public async openResource(uri: string): Promise<void> {
    await this.files.openResource(uri)
  }

  public openTextDocument(uri: URI | string): Promise<Document> {
    return this.files.openTextDocument(uri)
  }

  public getRelativePath(pathOrUri: string | URI, includeWorkspace?: boolean): string {
    return this.workspaceFolderControl.getRelativePath(pathOrUri, includeWorkspace)
  }

  public async findFiles(include: GlobPattern, exclude?: GlobPattern | null, maxResults?: number, token?: CancellationToken): Promise<URI[]> {
    return this.files.findFiles(include, exclude, maxResults, token)
  }

  public detach(): void {
    this.documentsManager.detach()
  }

  public reset(): void {
    this.configurations.reset()
    this.workspaceFolderControl.reset()
    this.documentsManager.reset()
  }

  public dispose(): void {
    this.watchers.dispose()
    this.autocmds.dispose()
    this.contentProvider.dispose()
    this.documentsManager.dispose()
    this.configurations.dispose()
  }
}

export default new Workspace()
