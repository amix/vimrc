'use strict'
import { CallHierarchyIncomingCall, CallHierarchyItem, CallHierarchyOutgoingCall, CancellationToken, Disposable, DocumentSelector, Position } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { CallHierarchyProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class CallHierarchyManager extends Manager<CallHierarchyProvider> {

  public register(selector: DocumentSelector, provider: CallHierarchyProvider): Disposable {
    let item: ProviderItem<CallHierarchyProvider> = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public async prepareCallHierarchy(document: TextDocument, position: Position, token: CancellationToken): Promise<CallHierarchyItem | CallHierarchyItem[]> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    if (provider.prepareCallHierarchy === null) return null
    return await Promise.resolve(provider.prepareCallHierarchy(document, position, token))
  }

  public async provideCallHierarchyOutgoingCalls(document: TextDocument, item: CallHierarchyItem, token: CancellationToken): Promise<CallHierarchyOutgoingCall[]> {
    let providerItem = this.getProvider(document)
    if (!providerItem) return null
    let { provider } = providerItem
    if (provider.provideCallHierarchyOutgoingCalls === null) return null
    return await Promise.resolve(provider.provideCallHierarchyOutgoingCalls(item, token))
  }

  public async provideCallHierarchyIncomingCalls(document: TextDocument, item: CallHierarchyItem, token: CancellationToken): Promise<CallHierarchyIncomingCall[]> {
    let providerItem = this.getProvider(document)
    if (!providerItem) return null
    let { provider } = providerItem
    if (provider.provideCallHierarchyIncomingCalls(item, token) === null) return null

    return await Promise.resolve(provider.provideCallHierarchyIncomingCalls(item, token))
  }
}
