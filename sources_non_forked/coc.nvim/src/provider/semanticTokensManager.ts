'use strict'
import { v4 as uuid } from 'uuid'
import { CancellationToken, Disposable, DocumentSelector, SemanticTokens, SemanticTokensDelta, SemanticTokensLegend } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DocumentSemanticTokensProvider } from './index'
import Manager, { ProviderItem } from './manager'
const logger = require('../util/logger')('semanticTokensManager')

export default class SemanticTokensManager extends Manager<DocumentSemanticTokensProvider> {
  private resolvedProvider: Map<string, string> = new Map()

  public register(selector: DocumentSelector, provider: DocumentSemanticTokensProvider, legend: SemanticTokensLegend, onChange: () => void): Disposable {
    let id = uuid()
    let item: ProviderItem<DocumentSemanticTokensProvider> = {
      id,
      selector,
      legend,
      provider
    }
    this.providers.add(item)
    let disposable: Disposable | undefined
    if (typeof provider.onDidChangeSemanticTokens === 'function') {
      disposable = provider.onDidChangeSemanticTokens(() => {
        onChange()
      })
    }
    return Disposable.create(() => {
      disposable?.dispose()
      for (let [uri, providerId] of this.resolvedProvider.entries()) {
        if (providerId == id) {
          this.resolvedProvider.delete(uri)
        }
      }
      this.providers.delete(item)
    })
  }

  public getLegend(document: TextDocument): SemanticTokensLegend {
    const item = this.getProvider(document)
    if (!item) return
    // save matched provider
    this.resolvedProvider.set(document.uri, item.id)
    return item.legend
  }

  protected resolveProvider(document: TextDocument): DocumentSemanticTokensProvider {
    let id = this.resolvedProvider.get(document.uri)
    if (id) return this.getProviderById(id)
    return this.getProvider(document)?.provider
  }

  public hasSemanticTokensEdits(document: TextDocument): boolean {
    let provider = this.resolveProvider(document)
    if (!provider) return false
    return (typeof provider.provideDocumentSemanticTokensEdits === 'function')
  }

  public async provideDocumentSemanticTokens(document: TextDocument, token: CancellationToken): Promise<SemanticTokens> {
    let provider = this.resolveProvider(document)
    if (!provider || typeof provider.provideDocumentSemanticTokens !== 'function') return null
    return await Promise.resolve(provider.provideDocumentSemanticTokens(document, token))
  }

  public async provideDocumentSemanticTokensEdits(document: TextDocument, previousResultId: string, token: CancellationToken): Promise<SemanticTokens | SemanticTokensDelta> {
    let provider = this.resolveProvider(document)
    if (!provider || typeof provider.provideDocumentSemanticTokensEdits !== 'function') return null
    return await Promise.resolve(provider.provideDocumentSemanticTokensEdits(document, previousResultId, token))
  }
}
