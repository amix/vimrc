'use strict'
import { NeovimClient as Neovim } from '@chemzqm/neovim'
import { CancellationToken, CancellationTokenSource, CodeActionKind, Disposable, Position, Range, SymbolKind } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import events from '../events'
import languages from '../languages'
import Document from '../model/document'
import { StatusBarItem } from '../model/status'
import { ExtendedCodeAction, ProviderName } from '../types'
import { disposeAll } from '../util'
import window from '../window'
import workspace from '../workspace'
import CodeActions from './codeActions'
import CodeLens from './codelens/index'
import Colors from './colors/index'
import Commands from './commands'
import Fold from './fold'
import Format from './format'
import Highlights from './highlights'
import HoverHandler from './hover'
import Links from './links'
import Locations from './locations'
import Refactor from './refactor/index'
import Rename from './rename'
import WorkspaceHandler from './workspace'
import SelectionRange from './selectionRange'
import CallHierarchy from './callHierarchy'
import SemanticTokens from './semanticTokens/index'
import Signature from './signature'
import Symbols from './symbols/index'
import { HandlerDelegate } from '../types'
import { getSymbolKind } from '../util/convert'
import LinkedEditingHandler from './linkedEditing'
import InlayHintHandler from './inlayHint/index'
const logger = require('../util/logger')('Handler')

export interface CurrentState {
  doc: Document
  winid: number
  position: Position
  // :h mode()
  mode: string
}

export default class Handler implements HandlerDelegate {
  public readonly documentHighlighter: Highlights
  public readonly colors: Colors
  public readonly signature: Signature
  public readonly locations: Locations
  public readonly symbols: Symbols
  public readonly refactor: Refactor
  public readonly codeActions: CodeActions
  public readonly format: Format
  public readonly hover: HoverHandler
  public readonly codeLens: CodeLens
  public readonly commands: Commands
  public readonly links: Links
  public readonly rename: Rename
  public readonly fold: Fold
  public readonly selectionRange: SelectionRange
  public readonly callHierarchy: CallHierarchy
  public readonly semanticHighlighter: SemanticTokens
  public readonly workspace: WorkspaceHandler
  public readonly linkedEditingHandler: LinkedEditingHandler
  public readonly inlayHintHandler: InlayHintHandler
  private labels: { [key: string]: string }
  private requestStatusItem: StatusBarItem
  private requestTokenSource: CancellationTokenSource | undefined
  private requestTimer: NodeJS.Timer
  private disposables: Disposable[] = []

  constructor(private nvim: Neovim) {
    this.requestStatusItem = window.createStatusBarItem(0, { progress: true })
    events.on(['CursorMoved', 'CursorMovedI', 'InsertEnter', 'InsertSnippet', 'InsertLeave'], () => {
      if (this.requestTokenSource) {
        this.requestTokenSource.cancel()
        this.requestTokenSource = null
      }
    }, null, this.disposables)
    this.labels = workspace.getConfiguration('suggest').get<any>('completionItemKindLabels', {})
    this.fold = new Fold(nvim, this)
    this.links = new Links(nvim, this)
    this.codeLens = new CodeLens(nvim)
    this.colors = new Colors(nvim, this)
    this.format = new Format(nvim, this)
    this.symbols = new Symbols(nvim, this)
    this.refactor = new Refactor(nvim, this)
    this.hover = new HoverHandler(nvim, this)
    this.locations = new Locations(nvim, this)
    this.signature = new Signature(nvim, this)
    this.rename = new Rename(nvim, this)
    this.workspace = new WorkspaceHandler(nvim, this)
    this.codeActions = new CodeActions(nvim, this)
    this.commands = new Commands(nvim, workspace.env)
    this.callHierarchy = new CallHierarchy(nvim, this)
    this.documentHighlighter = new Highlights(nvim, this)
    this.semanticHighlighter = new SemanticTokens(nvim, this)
    this.selectionRange = new SelectionRange(nvim, this)
    this.linkedEditingHandler = new LinkedEditingHandler(nvim, this)
    this.inlayHintHandler = new InlayHintHandler(nvim, this)
    this.disposables.push({
      dispose: () => {
        this.callHierarchy.dispose()
        this.codeLens.dispose()
        this.links.dispose()
        this.refactor.dispose()
        this.signature.dispose()
        this.symbols.dispose()
        this.hover.dispose()
        this.locations.dispose()
        this.colors.dispose()
        this.documentHighlighter.dispose()
        this.semanticHighlighter.dispose()
      }
    })
    void this.refactor.init()
  }

  public async getCurrentState(): Promise<CurrentState> {
    let { nvim } = this
    let [bufnr, [line, character], winid, mode] = await nvim.eval("[bufnr('%'),coc#cursor#position(),win_getid(),mode()]") as [number, [number, number], number, string]
    let doc = workspace.getAttachedDocument(bufnr)
    return {
      doc,
      mode,
      position: Position.create(line, character),
      winid
    }
  }

  public addDisposable(disposable: Disposable): void {
    this.disposables.push(disposable)
  }

  /**
   * Throw error when provider doesn't exist.
   */
  public checkProvier(id: ProviderName, document: TextDocument): void {
    if (!languages.hasProvider(id, document)) {
      throw new Error(`${id} provider not found for current buffer, your language server doesn't support it.`)
    }
  }

  public async withRequestToken<T>(name: string, fn: (token: CancellationToken) => Thenable<T>, checkEmpty?: boolean): Promise<T | null> {
    if (this.requestTokenSource) {
      this.requestTokenSource.cancel()
      this.requestTokenSource.dispose()
    }
    if (this.requestTimer) {
      clearTimeout(this.requestTimer)
    }
    let statusItem = this.requestStatusItem
    this.requestTokenSource = new CancellationTokenSource()
    let { token } = this.requestTokenSource
    token.onCancellationRequested(() => {
      statusItem.text = `${name} request canceled`
      statusItem.isProgress = false
      this.requestTimer = setTimeout(() => {
        statusItem.hide()
      }, 500)
    })
    statusItem.isProgress = true
    statusItem.text = `requesting ${name}`
    statusItem.show()
    let res: T
    try {
      res = await Promise.resolve(fn(token))
    } catch (e) {
      this.nvim.echoError(e)
    }
    if (this.requestTokenSource) {
      this.requestTokenSource.dispose()
      this.requestTokenSource = undefined
    }
    if (token.isCancellationRequested) return null
    statusItem.hide()
    if (checkEmpty && (!res || (Array.isArray(res) && res.length == 0))) {
      window.showMessage(`${name} not found`, 'warning')
      return null
    }
    return res
  }

  public getIcon(kind: SymbolKind): { text: string, hlGroup: string } {
    let { labels } = this
    let kindText = getSymbolKind(kind)
    let defaultIcon = typeof labels['default'] === 'string' ? labels['default'] : kindText[0].toLowerCase()
    let text = kindText == 'Unknown' ? '' : labels[kindText[0].toLowerCase() + kindText.slice(1)]
    if (!text || typeof text !== 'string') text = defaultIcon
    return {
      text,
      hlGroup: kindText == 'Unknown' ? 'CocSymbolDefault' : `CocSymbol${kindText}`
    }
  }

  public async getCodeActions(doc: Document, range?: Range, only?: CodeActionKind[]): Promise<ExtendedCodeAction[]> {
    let codeActions = await this.codeActions.getCodeActions(doc, range, only)
    return codeActions.filter(o => !o.disabled)
  }

  public async applyCodeAction(action: ExtendedCodeAction): Promise<void> {
    await this.codeActions.applyCodeAction(action)
  }

  public async hasProvider(id: string): Promise<boolean> {
    let bufnr = await this.nvim.call('bufnr', '%')
    let doc = workspace.getDocument(bufnr)
    if (!doc) return false
    return languages.hasProvider(id as ProviderName, doc.textDocument)
  }

  public dispose(): void {
    if (this.requestTimer) {
      clearTimeout(this.requestTimer)
    }
    disposeAll(this.disposables)
  }
}
