'use strict'
import { CancellationToken, Disposable, DocumentSelector, Location, Position } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { TypeDefinitionProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class TypeDefinitionManager extends Manager<TypeDefinitionProvider> {

  public register(selector: DocumentSelector, provider: TypeDefinitionProvider): Disposable {
    let item: ProviderItem<TypeDefinitionProvider> = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public async provideTypeDefinition(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<Location[] | null> {
    let providers = this.getProviders(document)
    if (!providers.length) return null
    let arr = await Promise.all(providers.map(item => {
      let { provider } = item
      return Promise.resolve(provider.provideTypeDefinition(document, position, token))
    }))
    return this.toLocations(arr)
  }
}
