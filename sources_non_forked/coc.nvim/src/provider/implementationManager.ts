'use strict'
import { CancellationToken, Disposable, DocumentSelector, Location, Position } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { ImplementationProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class ImplementationManager extends Manager<ImplementationProvider> {

  public register(selector: DocumentSelector, provider: ImplementationProvider): Disposable {
    let item: ProviderItem<ImplementationProvider> = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public async provideReferences(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<Location[] | null> {
    let providers = this.getProviders(document)
    if (!providers.length) return null
    let arr = await Promise.all(providers.map(item => {
      let { provider } = item
      return Promise.resolve(provider.provideImplementation(document, position, token))
    }))
    return this.toLocations(arr)
  }
}
