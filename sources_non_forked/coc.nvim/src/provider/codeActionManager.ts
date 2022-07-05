'use strict'
import { CancellationToken, CodeAction, CodeActionContext, CodeActionKind, Command, Disposable, DocumentSelector, Range } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { CodeActionProvider } from './index'
import { ExtendedCodeAction } from '../types'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'
import { omit } from '../util/lodash'
const logger = require('../util/logger')('codeActionManager')

export default class CodeActionManager extends Manager<CodeActionProvider> {
  public register(selector: DocumentSelector, provider: CodeActionProvider, clientId: string | undefined, codeActionKinds?: CodeActionKind[]): Disposable {
    let item: ProviderItem<CodeActionProvider> = {
      id: uuid(),
      selector,
      provider,
      kinds: codeActionKinds,
      clientId
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public async provideCodeActions(
    document: TextDocument,
    range: Range,
    context: CodeActionContext,
    token: CancellationToken
  ): Promise<ExtendedCodeAction[]> {
    let providers = this.getProviders(document)
    if (!providers.length) return null
    if (context.only) {
      let { only } = context
      providers = providers.filter(p => {
        if (p.kinds && !p.kinds.some(kind => only.includes(kind))) {
          return false
        }
        return true
      })
    }
    let res: ExtendedCodeAction[] = []
    await Promise.all(providers.map(item => {
      let { provider, id } = item
      return Promise.resolve(provider.provideCodeActions(document, range, context, token)).then(actions => {
        if (!actions || actions.length == 0) return
        for (let action of actions) {
          if (Command.is(action)) {
            let codeAction: ExtendedCodeAction = {
              title: action.title,
              command: action,
              providerId: id
            }
            res.push(codeAction)
          } else {
            if (context.only) {
              if (!action.kind) continue
              let found = false
              for (let only of context.only) {
                if (action.kind.startsWith(only)) {
                  found = true
                  break
                }
              }
              if (!found) continue
            }
            let idx = res.findIndex(o => o.title == action.title)
            if (idx == -1) {
              res.push(Object.assign({ providerId: id }, action))
            }
          }
        }
      })
    }))
    return res
  }

  public async resolveCodeAction(codeAction: ExtendedCodeAction, token: CancellationToken): Promise<CodeAction> {
    // no need to resolve
    if (codeAction.edit != null) return codeAction
    let id = codeAction.providerId
    if (!id) throw new Error(`provider id not found from codeAction`)
    let provider = this.getProviderById(id)
    if (!provider || typeof provider.resolveCodeAction !== 'function') {
      return codeAction
    }
    let resolved = await Promise.resolve(provider.resolveCodeAction(omit(codeAction, ['providerId']), token))
    return resolved || codeAction
  }
}
