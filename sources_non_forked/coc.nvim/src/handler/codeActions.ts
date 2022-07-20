'use strict'
import { NeovimClient as Neovim } from '@chemzqm/neovim'
import { CodeActionContext, CodeActionKind, Range } from 'vscode-languageserver-protocol'
import commandManager from '../commands'
import diagnosticManager from '../diagnostic/manager'
import languages from '../languages'
import Document from '../model/document'
import { ExtendedCodeAction, HandlerDelegate } from '../types'
import window from '../window'
import workspace from '../workspace'
const logger = require('../util/logger')('handler-codeActions')

/**
 * Handle codeActions related methods.
 */
export default class CodeActions {
  constructor(
    private nvim: Neovim,
    private handler: HandlerDelegate
  ) {
    handler.addDisposable(commandManager.registerCommand('editor.action.organizeImport', async (bufnr?: number) => {
      await this.organizeImport(bufnr)
    }))
    commandManager.titles.set('editor.action.organizeImport', 'run organize import code action.')
  }

  public async codeActionRange(start: number, end: number, only?: string): Promise<void> {
    let { doc } = await this.handler.getCurrentState()
    await doc.synchronize()
    let line = doc.getline(end - 1)
    let range = Range.create(start - 1, 0, end - 1, line.length)
    let codeActions = await this.getCodeActions(doc, range, only ? [only] : null)
    codeActions = codeActions.filter(o => !o.disabled)
    if (!codeActions || codeActions.length == 0) {
      window.showMessage(`No${only ? ' ' + only : ''} code action available`, 'warning')
      return
    }
    let idx = await window.showMenuPicker(codeActions.map(o => o.title), 'Choose action')
    let action = codeActions[idx]
    if (action) await this.applyCodeAction(action)
  }

  public async organizeImport(bufnr?: number): Promise<void> {
    let { doc } = await this.handler.getCurrentState()
    if (bufnr && doc.bufnr != bufnr) return
    await doc.synchronize()
    let actions = await this.getCodeActions(doc, undefined, [CodeActionKind.SourceOrganizeImports])
    if (actions && actions.length) {
      await this.applyCodeAction(actions[0])
      return
    }
    throw new Error('Organize import action not found.')
  }

  public async getCodeActions(doc: Document, range?: Range, only?: CodeActionKind[]): Promise<ExtendedCodeAction[]> {
    range = range || Range.create(0, 0, doc.lineCount, 0)
    let diagnostics = diagnosticManager.getDiagnosticsInRange(doc.textDocument, range)
    let context: CodeActionContext = { diagnostics }
    if (only && Array.isArray(only)) context.only = only
    let codeActions = await this.handler.withRequestToken('code action', token => {
      return languages.getCodeActions(doc.textDocument, range, context, token)
    })
    if (!codeActions || codeActions.length == 0) return []
    codeActions.sort((a, b) => {
      if (a.isPreferred && !b.isPreferred) return -1
      if (b.isPreferred && !a.isPreferred) return 1
      if (a.disabled && !b.disabled) return 1
      if (b.disabled && !a.disabled) return -1
      return 0
    })
    return codeActions
  }

  private get floatActions(): boolean {
    if (!workspace.floatSupported) return false
    let config = workspace.getConfiguration('coc.preferences')
    return config.get<boolean>('floatActions', true)
  }

  public async doCodeAction(mode: string | null, only?: CodeActionKind[] | string): Promise<void> {
    let { doc } = await this.handler.getCurrentState()
    let range: Range
    if (mode) range = await window.getSelectedRange(mode)
    await doc.synchronize()
    let codeActions = await this.getCodeActions(doc, range, Array.isArray(only) ? only : null)
    if (typeof only == 'string') {
      codeActions = codeActions.filter(o => o.title == only || (o.command && o.command.title == only))
    } else if (Array.isArray(only)) {
      codeActions = codeActions.filter(o => only.some(k => o.kind && o.kind.startsWith(k)))
    }
    if (!codeActions || codeActions.length == 0) {
      window.showMessage(`No${only ? ' ' + only : ''} code action available`, 'warning')
      return
    }
    if (only && codeActions.length == 1 && !codeActions[0].disabled) {
      await this.applyCodeAction(codeActions[0])
      return
    }
    if (!this.floatActions) codeActions = codeActions.filter(o => !o.disabled)
    let idx = this.floatActions
      ? await window.showMenuPicker(
        codeActions.map(o => {
          return { text: o.title, disabled: o.disabled }
        }),
        'Choose action'
      )
      : await window.showQuickpick(codeActions.map(o => o.title))
    let action = codeActions[idx]
    if (action) await this.applyCodeAction(action)
  }

  /**
   * Get current codeActions
   */
  public async getCurrentCodeActions(mode?: string, only?: CodeActionKind[]): Promise<ExtendedCodeAction[]> {
    let { doc } = await this.handler.getCurrentState()
    let range: Range
    if (mode) range = await window.getSelectedRange(mode)
    let codeActions = await this.getCodeActions(doc, range, only)
    return codeActions.filter(o => !o.disabled)
  }

  /**
   * Invoke preferred quickfix at current position
   */
  public async doQuickfix(): Promise<void> {
    let actions = await this.getCurrentCodeActions('line', [CodeActionKind.QuickFix])
    if (!actions || actions.length == 0) {
      throw new Error('No quickfix action available')
    }
    await this.applyCodeAction(actions[0])
    this.nvim.command(`silent! call repeat#set("\\<Plug>(coc-fix-current)", -1)`, true)
  }

  public async applyCodeAction(action: ExtendedCodeAction): Promise<void> {
    if (action.disabled) {
      throw new Error(`Action "${action.title}" is disabled: ${action.disabled.reason}`)
    }
    if (!action.providerId) {
      throw new Error('providerId not found with codeAction')
    }
    let resolved = await this.handler.withRequestToken('resolve codeAction', token => {
      return languages.resolveCodeAction(action, token)
    })
    let { edit, command } = resolved
    if (edit) await workspace.applyEdit(edit)
    if (command) await commandManager.execute(command)
  }
}
