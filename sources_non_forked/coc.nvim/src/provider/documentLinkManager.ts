'use strict'
import { CancellationToken, Disposable, DocumentLink, DocumentSelector } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { DocumentLinkProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class DocumentLinkManager extends Manager<DocumentLinkProvider> {

  public register(selector: DocumentSelector, provider: DocumentLinkProvider): Disposable {
    let item = {
      id: uuid(),
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  private async _provideDocumentLinks(item: ProviderItem<DocumentLinkProvider>, document: TextDocument, token: CancellationToken): Promise<DocumentLink[]> {
    let { provider, id } = item
    let items = await Promise.resolve(provider.provideDocumentLinks(document, token))
    if (!items || !items.length) return []
    items.forEach(item => {
      item.data = item.data || {}
      item.data.source = id
    })
    return items
  }

  public async provideDocumentLinks(document: TextDocument, token: CancellationToken): Promise<DocumentLink[]> {
    let items = this.getProviders(document)
    if (items.length == 0) return []
    const arr = await Promise.all(items.map(item => this._provideDocumentLinks(item, document, token)))
    return [].concat(...arr)
  }

  public async resolveDocumentLink(link: DocumentLink, token: CancellationToken): Promise<DocumentLink> {
    let { data } = link
    if (!data || !data.source) return null
    for (let item of this.providers) {
      if (item.id == data.source) {
        let { provider } = item
        link = await Promise.resolve(provider.resolveDocumentLink(link, token))
        return link
      }
    }
    return null
  }
}
