'use strict'
import { CancellationToken, Disposable, DocumentSelector, Location, Position, ReferenceContext } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { ReferenceProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class ReferenceManager extends Manager<ReferenceProvider>  {

  public register(selector: DocumentSelector, provider: ReferenceProvider): Disposable {
    let item: ProviderItem<ReferenceProvider> = {
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
    context: ReferenceContext,
    token: CancellationToken
  ): Promise<Location[] | null> {
    let providers = this.getProviders(document)
    if (!providers.length) return null
    let arr = await Promise.all(providers.map(item => {
      let { provider } = item
      return Promise.resolve(provider.provideReferences(document, position, context, token))
    }))
    return this.toLocations(arr)
  }
}
