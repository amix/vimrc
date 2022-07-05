'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict'

import { CancellationToken, ClientCapabilities, Disposable, DocumentSelector, Position, SelectionRange, SelectionRangeClientCapabilities, SelectionRangeOptions, SelectionRangeParams, SelectionRangeRegistrationOptions, SelectionRangeRequest, ServerCapabilities } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import languages from '../languages'
import { ProviderResult, SelectionRangeProvider } from '../provider'
import { BaseLanguageClient, ensure, TextDocumentFeature } from './client'

export interface ProvideSelectionRangeSignature {
  (this: void, document: TextDocument, positions: Position[], token: CancellationToken): ProviderResult<SelectionRange[]>
}

export interface SelectionRangeProviderMiddleware {
  provideSelectionRanges?: (this: void, document: TextDocument, positions: Position[], token: CancellationToken, next: ProvideSelectionRangeSignature) => ProviderResult<SelectionRange[]>
}

export class SelectionRangeFeature extends TextDocumentFeature<boolean | SelectionRangeOptions, SelectionRangeRegistrationOptions, SelectionRangeProvider> {
  constructor(client: BaseLanguageClient) {
    super(client, SelectionRangeRequest.type)
  }

  public fillClientCapabilities(capabilities: ClientCapabilities & SelectionRangeClientCapabilities): void {
    let capability = ensure(ensure(capabilities, 'textDocument')!, 'selectionRange')!
    capability.dynamicRegistration = true
  }

  public initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector): void {
    let [id, options] = this.getRegistration(documentSelector, capabilities.selectionRangeProvider)
    if (!id || !options) {
      return
    }
    this.register({ id, registerOptions: options })
  }

  protected registerLanguageProvider(options: SelectionRangeRegistrationOptions): [Disposable, SelectionRangeProvider] {
    const provider: SelectionRangeProvider = {
      provideSelectionRanges: (document, positions, token) => {
        const client = this._client
        const provideSelectionRanges: ProvideSelectionRangeSignature = (document, positions, token) => {
          const requestParams: SelectionRangeParams = {
            textDocument: { uri: document.uri },
            positions
          }
          return client.sendRequest(SelectionRangeRequest.type, requestParams, token).then(
            ranges => ranges,
            (error: any) => {
              return client.handleFailedRequest(SelectionRangeRequest.type, token, error, null)
            }
          )
        }
        const middleware = client.clientOptions.middleware
        return middleware.provideSelectionRanges
          ? middleware.provideSelectionRanges(document, positions, token, provideSelectionRanges)
          : provideSelectionRanges(document, positions, token)
      }
    }
    return [languages.registerSelectionRangeProvider(options.documentSelector, provider), provider]
  }
}
