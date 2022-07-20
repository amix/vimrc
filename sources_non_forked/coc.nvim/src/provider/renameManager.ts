'use strict'
import { CancellationToken, Disposable, DocumentSelector, Position, Range, WorkspaceEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { RenameProvider } from './index'
import Manager from './manager'
import { v4 as uuid } from 'uuid'

export default class RenameManager extends Manager<RenameProvider> {

  public register(selector: DocumentSelector, provider: RenameProvider): Disposable {
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

  public async provideRenameEdits(
    document: TextDocument,
    position: Position,
    newName: string,
    token: CancellationToken
  ): Promise<WorkspaceEdit | null> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    return await Promise.resolve(provider.provideRenameEdits(document, position, newName, token))
  }

  public async prepareRename(
    document: TextDocument,
    position: Position,
    token: CancellationToken
  ): Promise<Range | { range: Range; placeholder: string } | false> {
    let item = this.getProvider(document)
    if (!item) return null
    let { provider } = item
    if (provider.prepareRename == null) return null
    let res = await Promise.resolve(provider.prepareRename(document, position, token))
    // can not rename
    if (res == null) return false
    return res
  }
}
