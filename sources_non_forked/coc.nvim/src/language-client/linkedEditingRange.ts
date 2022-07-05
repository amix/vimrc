'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
* ------------------------------------------------------------------------------------------ */

import { CancellationToken, ClientCapabilities, Disposable, DocumentSelector, LinkedEditingRangeOptions, LinkedEditingRangeRegistrationOptions, LinkedEditingRangeRequest, LinkedEditingRanges, Position, ServerCapabilities } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import languages from '../languages'
import { LinkedEditingRangeProvider, ProviderResult } from '../provider'
import { BaseLanguageClient, ensure, TextDocumentFeature } from './client'
import * as cv from './utils/converter'
const logger = require('../util/logger')('languageclient-linkedEditingRange')

export interface ProvideLinkedEditingRangeSignature {
  (this: void, document: TextDocument, position: Position, token: CancellationToken): ProviderResult<LinkedEditingRanges>
}

/**
 * Linked editing middleware
 *
 * @since 3.16.0
 */
export interface LinkedEditingRangeMiddleware {
  provideLinkedEditingRange?: (this: void, document: TextDocument, position: Position, token: CancellationToken, next: ProvideLinkedEditingRangeSignature) => ProviderResult<LinkedEditingRanges>
}

export class LinkedEditingFeature extends TextDocumentFeature<boolean | LinkedEditingRangeOptions, LinkedEditingRangeRegistrationOptions, LinkedEditingRangeProvider> {

  constructor(client: BaseLanguageClient) {
    super(client, LinkedEditingRangeRequest.type)
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const linkedEditingSupport = ensure(ensure(capabilities, 'textDocument')!, 'linkedEditingRange')!
    linkedEditingSupport.dynamicRegistration = true
  }

  public initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector): void {
    let [id, options] = this.getRegistration(documentSelector, capabilities.linkedEditingRangeProvider)
    if (!id || !options) {
      return
    }
    this.register({ id, registerOptions: options })
  }

  protected registerLanguageProvider(options: LinkedEditingRangeRegistrationOptions): [Disposable, LinkedEditingRangeProvider] {
    const provider: LinkedEditingRangeProvider = {
      provideLinkedEditingRanges: (document, position, token) => {
        const client = this._client
        const provideLinkedEditing: ProvideLinkedEditingRangeSignature = (document, position, token) => {
          const params = cv.asTextDocumentPositionParams(document, position)
          return client.sendRequest(LinkedEditingRangeRequest.type, params, token).then(result => result, error => {
            return client.handleFailedRequest(LinkedEditingRangeRequest.type, token, error, null)
          })
        }
        const middleware = client.clientOptions.middleware!
        return middleware.provideLinkedEditingRange
          ? middleware.provideLinkedEditingRange(document, position, token, provideLinkedEditing)
          : provideLinkedEditing(document, position, token)
      }
    }
    return [languages.registerLinkedEditingRangeProvider(options.documentSelector!, provider), provider]
  }
}
