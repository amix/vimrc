'use strict'
import { CancellationToken, Disposable, DocumentSelector, FormattingOptions, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DocumentFormattingEditProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class FormatManager extends Manager<DocumentFormattingEditProvider> {

  public register(selector: DocumentSelector,
    provider: DocumentFormattingEditProvider,
    priority = 0): Disposable {
    let item: ProviderItem<DocumentFormattingEditProvider> = {
      id: uuid(),
      selector,
      priority,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public handles(doc: TextDocument): boolean {
    return this.getProvider(doc) != null
  }

  public async provideDocumentFormattingEdits(
    document: TextDocument,
    options: FormattingOptions,
    token: CancellationToken
  ): Promise<TextEdit[]> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    return await Promise.resolve(provider.provideDocumentFormattingEdits(document, options, token))
  }
}
