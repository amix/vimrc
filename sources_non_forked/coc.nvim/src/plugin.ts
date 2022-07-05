'use strict'
import { NeovimClient as Neovim } from '@chemzqm/neovim'
import { EventEmitter } from 'events'
import { CodeActionKind, Disposable } from 'vscode-languageserver-protocol'
import commandManager from './commands'
import completion from './completion'
import channels from './core/channels'
import Cursors from './cursors'
import diagnosticManager from './diagnostic/manager'
import events from './events'
import extensions from './extensions'
import Handler from './handler'
import listManager from './list/manager'
import services from './services'
import snippetManager from './snippets/manager'
import sources from './sources'
import { disposeAll } from './util'
import window from './window'
import workspace from './workspace'
const logger = require('./util/logger')('plugin')

export default class Plugin extends EventEmitter {
  private _ready = false
  private handler: Handler | undefined
  private cursors: Cursors
  private actions: Map<string, Function> = new Map()
  private disposables: Disposable[] = []

  constructor(public nvim: Neovim) {
    super()
    this.disposables.push(workspace.registerTextDocumentContentProvider('output', channels.getProvider(nvim)))
    Object.defineProperty(workspace, 'nvim', {
      get: () => this.nvim
    })
    Object.defineProperty(window, 'cursors', {
      get: () => this.cursors
    })
    workspace.onDidChangeWorkspaceFolders(() => {
      nvim.setVar('WorkspaceFolders', workspace.folderPaths, true)
    }, null, this.disposables)
    events.on('VimResized', (columns, lines) => {
      if (workspace.env) Object.assign(workspace.env, { columns, lines })
    }, null, this.disposables)
    this.cursors = new Cursors(nvim)
    commandManager.init(nvim, this)
    this.addAction('checkJsonExtension', () => {
      if (extensions.has('coc-json')) return
      window.showMessage(`Run :CocInstall coc-json for json intellisense`, 'more')
    })
    this.addAction('rootPatterns', (bufnr: number) => this.handler.workspace.getRootPatterns(bufnr))
    this.addAction('ensureDocument', () => this.handler.workspace.ensureDocument())
    this.addAction('getConfig', async key => this.handler.workspace.getConfiguration(key))
    this.addAction('doAutocmd', async (id: number, ...args: []) => this.handler.workspace.doAutocmd(id, args))
    this.addAction('openLog', async () => this.handler.workspace.openLog())
    this.addAction('attach', () => workspace.attach())
    this.addAction('detach', () => workspace.detach())
    this.addAction('doKeymap', async (key, defaultReturn, pressed) => this.handler.workspace.doKeymap(key, defaultReturn, pressed))
    this.addAction('registExtensions', (...folders: string[]) => extensions.loadExtension(folders))
    this.addAction('snippetCheck', async (checkExpand: boolean, checkJump: boolean) => this.handler.workspace.snippetCheck(checkExpand, checkJump))
    this.addAction('snippetNext', () => snippetManager.nextPlaceholder())
    this.addAction('snippetPrev', () => snippetManager.previousPlaceholder())
    this.addAction('snippetCancel', () => snippetManager.cancel())
    this.addAction('openLocalConfig', () => window.openLocalConfig())
    this.addAction('bufferCheck', () => window.bufferCheck())
    this.addAction('showInfo', () => this.handler.workspace.showInfo())
    this.addAction('hasProvider', id => this.handler.hasProvider(id))
    this.addAction('hasSelected', () => completion.hasSelected())
    this.addAction('listNames', () => listManager.names)
    this.addAction('listDescriptions', () => listManager.descriptions)
    this.addAction('listLoadItems', name => listManager.loadItems(name))
    this.addAction('search', (...args: string[]) => this.handler.refactor.search(args))
    this.addAction('cursorsSelect', (bufnr: number, kind: string, mode: string) => this.cursors.select(bufnr, kind, mode))
    this.addAction('fillDiagnostics', (bufnr: number) => diagnosticManager.setLocationlist(bufnr))
    this.addAction('saveRefactor', bufnr => this.handler.refactor.save(bufnr))
    this.addAction('commandList', () => this.handler.commands.getCommandList())
    this.addAction('selectSymbolRange', (inner: boolean, visualmode: string, supportedSymbols: string[]) => this.handler.symbols.selectSymbolRange(inner, visualmode, supportedSymbols))
    this.addAction('openList', (...args: string[]) => listManager.start(args))
    this.addAction('listResume', (name?: string) => listManager.resume(name))
    this.addAction('listCancel', () => listManager.cancel(true))
    this.addAction('listPrev', (name?: string) => listManager.previous(name))
    this.addAction('listNext', (name?: string) => listManager.next(name))
    this.addAction('listFirst', (name?: string) => listManager.first(name))
    this.addAction('listLast', (name?: string) => listManager.last(name))
    this.addAction('sendRequest', (id: string, method: string, params?: any) => services.sendRequest(id, method, params))
    this.addAction('sendNotification', (id: string, method: string, params?: any) => services.sendNotification(id, method, params))
    this.addAction('registNotification', (id: string, method: string) => services.registNotification(id, method))
    this.addAction('updateConfig', (section: string, val: any) => workspace.configurations.updateUserConfig({ [section]: val }))
    this.addAction('links', () => this.handler.links.getLinks())
    this.addAction('openLink', () => this.handler.links.openCurrentLink())
    this.addAction('pickColor', () => this.handler.colors.pickColor())
    this.addAction('colorPresentation', () => this.handler.colors.pickPresentation())
    this.addAction('highlight', () => this.handler.documentHighlighter.highlight())
    this.addAction('fold', (kind?: string) => this.handler.fold.fold(kind))
    this.addAction('startCompletion', option => completion.startCompletion(option))
    this.addAction('stopCompletion', () => completion.stop())
    this.addAction('sourceStat', () => sources.sourceStats())
    this.addAction('refreshSource', name => sources.refresh(name))
    this.addAction('toggleSource', name => sources.toggleSource(name))
    this.addAction('diagnosticRefresh', bufnr => diagnosticManager.refresh(bufnr))
    this.addAction('diagnosticInfo', () => diagnosticManager.echoMessage())
    this.addAction('diagnosticToggle', enable => diagnosticManager.toggleDiagnostic(enable))
    this.addAction('diagnosticToggleBuffer', (bufnr, enable) => diagnosticManager.toggleDiagnosticBuffer(bufnr, enable))
    this.addAction('diagnosticNext', severity => diagnosticManager.jumpNext(severity))
    this.addAction('diagnosticPrevious', severity => diagnosticManager.jumpPrevious(severity))
    this.addAction('diagnosticPreview', () => diagnosticManager.preview())
    this.addAction('diagnosticList', async () => diagnosticManager.getDiagnosticList())
    this.addAction('findLocations', (id, method, params, openCommand) => this.handler.locations.findLocations(id, method, params, openCommand))
    this.addAction('getTagList', () => this.handler.locations.getTagList())
    this.addAction('jumpDefinition', openCommand => this.handler.locations.gotoDefinition(openCommand))
    this.addAction('definitions', () => this.handler.locations.definitions())
    this.addAction('jumpDeclaration', openCommand => this.handler.locations.gotoDeclaration(openCommand))
    this.addAction('declarations', () => this.handler.locations.declarations())
    this.addAction('jumpImplementation', openCommand => this.handler.locations.gotoImplementation(openCommand))
    this.addAction('implementations', () => this.handler.locations.implementations())
    this.addAction('jumpTypeDefinition', openCommand => this.handler.locations.gotoTypeDefinition(openCommand))
    this.addAction('typeDefinitions', () => this.handler.locations.typeDefinitions())
    this.addAction('jumpReferences', openCommand => this.handler.locations.gotoReferences(openCommand))
    this.addAction('references', excludeDeclaration => this.handler.locations.references(excludeDeclaration))
    this.addAction('jumpUsed', openCommand => this.handler.locations.gotoReferences(openCommand, false))
    this.addAction('doHover', hoverTarget => this.handler.hover.onHover(hoverTarget))
    this.addAction('definitionHover', hoverTarget => this.handler.hover.definitionHover(hoverTarget))
    this.addAction('getHover', () => this.handler.hover.getHover())
    this.addAction('showSignatureHelp', () => this.handler.signature.triggerSignatureHelp())
    this.addAction('documentSymbols', (bufnr?: number) => this.handler.symbols.getDocumentSymbols(bufnr))
    this.addAction('symbolRanges', () => this.handler.documentHighlighter.getSymbolsRanges())
    this.addAction('selectionRanges', () => this.handler.selectionRange.getSelectionRanges())
    this.addAction('rangeSelect', (visualmode, forward) => this.handler.selectionRange.selectRange(visualmode, forward))
    this.addAction('rename', newName => this.handler.rename.rename(newName))
    this.addAction('getWorkspaceSymbols', input => this.handler.symbols.getWorkspaceSymbols(input))
    this.addAction('resolveWorkspaceSymbol', symbolInfo => this.handler.symbols.resolveWorkspaceSymbol(symbolInfo))
    this.addAction('formatSelected', mode => this.handler.format.formatCurrentRange(mode))
    this.addAction('format', () => this.handler.format.formatCurrentBuffer())
    this.addAction('commands', () => this.handler.commands.getCommands())
    this.addAction('services', () => services.getServiceStats())
    this.addAction('toggleService', name => services.toggle(name))
    this.addAction('codeAction', (mode, only) => this.handler.codeActions.doCodeAction(mode, only))
    this.addAction('organizeImport', () => this.handler.codeActions.organizeImport())
    this.addAction('fixAll', () => this.handler.codeActions.doCodeAction(null, [CodeActionKind.SourceFixAll]))
    this.addAction('doCodeAction', codeAction => this.handler.codeActions.applyCodeAction(codeAction))
    this.addAction('codeActions', (mode, only) => this.handler.codeActions.getCurrentCodeActions(mode, only))
    this.addAction('quickfixes', mode => this.handler.codeActions.getCurrentCodeActions(mode, [CodeActionKind.QuickFix]))
    this.addAction('codeLensAction', () => this.handler.codeLens.doAction())
    this.addAction('runCommand', (...args: any[]) => this.handler.commands.runCommand(...args))
    this.addAction('doQuickfix', () => this.handler.codeActions.doQuickfix())
    this.addAction('refactor', () => this.handler.refactor.doRefactor())
    this.addAction('repeatCommand', () => this.handler.commands.repeat())
    this.addAction('installExtensions', (...list: string[]) => extensions.installExtensions(list))
    this.addAction('updateExtensions', sync => extensions.updateExtensions(sync))
    this.addAction('extensionStats', () => extensions.getExtensionStates())
    this.addAction('loadedExtensions', () => extensions.loadedExtensions())
    this.addAction('watchExtension', (id: string) => extensions.watchExtension(id))
    this.addAction('activeExtension', name => extensions.activate(name))
    this.addAction('deactivateExtension', name => extensions.deactivate(name))
    this.addAction('reloadExtension', name => extensions.reloadExtension(name))
    this.addAction('toggleExtension', name => extensions.toggleExtension(name))
    this.addAction('uninstallExtension', (...args: string[]) => extensions.uninstallExtension(args))
    this.addAction('getCurrentFunctionSymbol', () => this.handler.symbols.getCurrentFunctionSymbol())
    this.addAction('showOutline', (keep?: number) => this.handler.symbols.showOutline(keep))
    this.addAction('hideOutline', () => this.handler.symbols.hideOutline())
    this.addAction('getWordEdit', () => this.handler.rename.getWordEdit())
    this.addAction('addCommand', cmd => this.handler.commands.addVimCommand(cmd))
    this.addAction('addRanges', ranges => this.cursors.addRanges(ranges))
    this.addAction('currentWorkspacePath', () => workspace.rootPath)
    this.addAction('selectCurrentPlaceholder', triggerAutocmd => snippetManager.selectCurrentPlaceholder(!!triggerAutocmd))
    this.addAction('codeActionRange', (start, end, only) => this.handler.codeActions.codeActionRange(start, end, only))
    this.addAction('incomingCalls', item => this.handler.callHierarchy.getIncoming(item))
    this.addAction('outgoingCalls', item => this.handler.callHierarchy.getOutgoing(item))
    this.addAction('showIncomingCalls', () => this.handler.callHierarchy.showCallHierarchyTree('incoming'))
    this.addAction('showOutgoingCalls', () => this.handler.callHierarchy.showCallHierarchyTree('outgoing'))
    this.addAction('inspectSemanticToken', () => this.handler.semanticHighlighter.inspectSemanticToken())
    this.addAction('semanticHighlight', () => this.handler.semanticHighlighter.highlightCurrent())
    this.addAction('showSemanticHighlightInfo', () => this.handler.semanticHighlighter.showHighlightInfo())
  }

  private addAction(key: string, fn: Function): void {
    if (this.actions.has(key)) {
      throw new Error(`Action ${key} already exists`)
    }
    this.actions.set(key, fn)
  }

  public async init(): Promise<void> {
    let { nvim } = this
    let s = Date.now()
    try {
      await extensions.init()
      await workspace.init(window)
      nvim.setVar('coc_workspace_initialized', true, true)
      snippetManager.init()
      completion.init()
      diagnosticManager.init()
      listManager.init(nvim)
      sources.init()
      this.handler = new Handler(nvim)
      services.init()
      extensions.activateExtensions()
      workspace.autocmds.setupDynamicAutocmd(true)
      nvim.pauseNotification()
      nvim.setVar('WorkspaceFolders', workspace.folderPaths, true)
      nvim.setVar('coc_service_initialized', 1, true)
      nvim.call('coc#util#do_autocmd', ['CocNvimInit'], true)
      nvim.resumeNotification(false, true)
      this._ready = true
      await events.fire('ready', [])
      logger.info(`coc.nvim initialized with node: ${process.version} after ${Date.now() - s}ms`)
      this.emit('ready')
    } catch (e) {
      nvim.echoError(e)
    }
  }

  public get isReady(): boolean {
    return this._ready
  }

  public get ready(): Promise<void> {
    if (this._ready) return Promise.resolve()
    return new Promise<void>(resolve => {
      this.once('ready', () => {
        resolve()
      })
    })
  }

  public hasAction(method: string): boolean {
    return this.actions.has(method)
  }

  public async cocAction(method: string, ...args: any[]): Promise<any> {
    let fn = this.actions.get(method)
    if (!fn) throw new Error(`Action "${method}" doesn't exist`)
    let ts = Date.now()
    let res = await Promise.resolve(fn.apply(null, args))
    let dt = Date.now() - ts
    if (dt > 500) logger.warn(`Slow action "${method}" cost ${dt}ms`)
    return res
  }

  public getHandler(): Handler {
    return this.handler
  }

  public dispose(): void {
    this.removeAllListeners()
    disposeAll(this.disposables)
    extensions.dispose()
    listManager.dispose()
    workspace.dispose()
    channels.dispose()
    window.dispose()
    sources.dispose()
    services.stopAll()
    services.dispose()
    if (this.handler) {
      this.handler.dispose()
    }
    snippetManager.dispose()
    commandManager.dispose()
    completion.dispose()
    diagnosticManager.dispose()
  }
}
