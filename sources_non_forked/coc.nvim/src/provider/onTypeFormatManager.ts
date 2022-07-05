'use strict'
import { CancellationToken, Disposable, DocumentSelector, Position, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import workspace from '../workspace'
import { OnTypeFormattingEditProvider } from './index'
const logger = require('../util/logger')('onTypeFormatManager')

export interface ProviderItem {
  triggerCharacters: string[]
  selector: DocumentSelector
  provider: OnTypeFormattingEditProvider
}

export default class OnTypeFormatManager {
  private providers: Set<ProviderItem> = new Set()

  public register(selector: DocumentSelector, provider: OnTypeFormattingEditProvider, triggerCharacters: string[]): Disposable {
    let item: ProviderItem = {
      triggerCharacters,
      selector,
      provider
    }
    this.providers.add(item)
    return Disposable.create(() => {
      this.providers.delete(item)
    })
  }

  public hasProvider(document: TextDocument): boolean {
    for (let o of this.providers) {
      let { selector } = o
      if (workspace.match(selector, document) > 0) {
        return true
      }
    }
    return false
  }

  public getProvider(document: TextDocument, triggerCharacter: string): OnTypeFormattingEditProvider | null {
    for (let o of this.providers) {
      let { triggerCharacters, selector } = o
      if (workspace.match(selector, document) > 0 && triggerCharacters.includes(triggerCharacter)) {
        return o.provider
      }
    }
    return null
  }

  public async onCharacterType(character: string, document: TextDocument, position: Position, token: CancellationToken): Promise<TextEdit[] | null> {
    let provider = this.getProvider(document, character)
    if (!provider) return
    let formatOpts = await workspace.getFormatOptions(document.uri)
    return await Promise.resolve(provider.provideOnTypeFormattingEdits(document, position, character, formatOpts, token))
  }
}
