'use strict'
import { CancellationToken, Disposable, DocumentSelector, Position, SignatureHelp, SignatureHelpContext } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { SignatureHelpProvider } from './index'
import Manager, { ProviderItem } from './manager'
import { v4 as uuid } from 'uuid'

export default class SignatureManager extends Manager<SignatureHelpProvider> {

  public register(selector: DocumentSelector, provider: SignatureHelpProvider, triggerCharacters?: string[]): Disposable {
    let characters = triggerCharacters.reduce((p, c) => p.concat(c.length == 1 ? [c] : c.split(/\s*/g)), [] as string[])
    let item: ProviderItem<SignatureHelpProvider> = {
      id: uuid(),
      selector,
      provider,
      triggerCharacters: characters
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public shouldTrigger(document: TextDocument, triggerCharacter: string): boolean {
    let item = this.getProvider(document)
    if (!item) return false
    let { triggerCharacters } = item
    return triggerCharacters && triggerCharacters.indexOf(triggerCharacter) != -1
  }

  public async provideSignatureHelp(
    document: TextDocument,
    position: Position,
    token: CancellationToken,
    context: SignatureHelpContext
  ): Promise<SignatureHelp | null> {
    let item = this.getProvider(document)
    if (!item) return null
    let res = await Promise.resolve(item.provider.provideSignatureHelp(document, position, token, context))
    if (res && res.signatures && res.signatures.length) return res
    return null
  }
}
