'use strict'
import { CancellationToken, Disposable, DocumentSelector, FormattingOptions, Range, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DocumentRangeFormattingEditProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class FormatRangeManager extends Manager<DocumentRangeFormattingEditProvider> {

  public register(selector: DocumentSelector,
    provider: DocumentRangeFormattingEditProvider,
    priority = 0): Disposable {
    let item: ProviderItem<DocumentRangeFormattingEditProvider> = {
      id: uuid(),
      selector,
      provider,
      priority
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public async provideDocumentRangeFormattingEdits(
    document: TextDocument,
    range: Range,
    options: FormattingOptions,
    token: CancellationToken
  ): Promise<TextEdit[]> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    return await Promise.resolve(provider.provideDocumentRangeFormattingEdits(document, range, options, token))
  }
}
