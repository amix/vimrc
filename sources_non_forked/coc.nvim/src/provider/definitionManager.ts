'use strict'
import { CancellationToken, Definition, DefinitionLink, Disposable, DocumentSelector, Location, LocationLink, Position } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DefinitionProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'
import { equals } from '../util/object'
const logger = require('../util/logger')('definitionManager')

export default class DefinitionManager extends Manager<DefinitionProvider> {

  public register(selector: DocumentSelector, provider: DefinitionProvider): Disposable {
    let item: ProviderItem<DefinitionProvider> = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  private async getDefinitions(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<(Definition | DefinitionLink[])[]> {
    const providers = this.getProviders(document)
    if (!providers.length) return []
    const arr = await Promise.all(providers.map(item => {
      const { provider } = item
      return Promise.resolve(provider.provideDefinition(document, position, token))
    }))
    return arr
  }

  public async provideDefinition(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<Location[]> {
    const arr = await this.getDefinitions(document, position, token)
    return this.toLocations(arr)
  }

  public async provideDefinitionLinks(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<DefinitionLink[]> {
    const arr = await this.getDefinitions(document, position, token)
    const defs: DefinitionLink[] = []
    for (const def of arr) {
      if (!Array.isArray(def)) continue
      for (const val of def) {
        if (LocationLink.is(val)) {
          let idx = defs.findIndex(o => o.targetUri == val.targetUri && equals(o.targetRange, val.targetRange))
          if (idx == -1) defs.push(val)
        }
      }
    }
    return defs
  }
}
