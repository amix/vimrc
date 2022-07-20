'use strict'
import { v4 as uuid } from 'uuid'
import { CancellationToken, Disposable, DocumentSelector, Range, SemanticTokens, SemanticTokensLegend } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DocumentRangeSemanticTokensProvider } from './index'
import Manager, { ProviderItem } from './manager'
const logger = require('../util/logger')('semanticTokensRangeManager')

export default class SemanticTokensRangeManager extends Manager<DocumentRangeSemanticTokensProvider> {
  public register(selector: DocumentSelector, provider: DocumentRangeSemanticTokensProvider, legend: SemanticTokensLegend): Disposable {
    let item: ProviderItem<DocumentRangeSemanticTokensProvider> = {
      id: uuid(),
      selector,
      legend,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public getLegend(document: TextDocument): SemanticTokensLegend {
    const item = this.getProvider(document)
    if (!item) return
    return item.legend as SemanticTokensLegend
  }

  public async provideDocumentRangeSemanticTokens(document: TextDocument, range: Range, token: CancellationToken): Promise<SemanticTokens> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    if (provider.provideDocumentRangeSemanticTokens === null) return null
    return await Promise.resolve(provider.provideDocumentRangeSemanticTokens(document, range, token))
  }
}
