'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict'

import {
  CallHierarchyClientCapabilities, CallHierarchyIncomingCall, CallHierarchyIncomingCallsRequest, CallHierarchyItem, CallHierarchyOptions, CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsRequest, CallHierarchyPrepareRequest, CallHierarchyRegistrationOptions, CancellationToken, ClientCapabilities, Disposable, DocumentSelector, Position, ServerCapabilities
} from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import languages from '../languages'
import { CallHierarchyProvider, ProviderResult } from '../provider'
import { BaseLanguageClient, ensure, TextDocumentFeature } from './client'
import { asTextDocumentPositionParams } from './utils/converter'

export interface PrepareCallHierarchySignature {
  (this: void, document: TextDocument, position: Position, token: CancellationToken): ProviderResult<CallHierarchyItem | CallHierarchyItem[]>
}

export interface CallHierarchyIncomingCallsSignature {
  (this: void, item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyIncomingCall[]>
}

export interface CallHierarchyOutgoingCallsSignature {
  (this: void, item: CallHierarchyItem, token: CancellationToken): ProviderResult<CallHierarchyOutgoingCall[]>
}

/**
 * Call hierarchy middleware
 *
 * @since 3.16.0
 */
export interface CallHierarchyMiddleware {
  prepareCallHierarchy?: (this: void, document: TextDocument, positions: Position, token: CancellationToken, next: PrepareCallHierarchySignature) => ProviderResult<CallHierarchyItem | CallHierarchyItem[]>
  provideCallHierarchyIncomingCalls?: (this: void, item: CallHierarchyItem, token: CancellationToken, next: CallHierarchyIncomingCallsSignature) => ProviderResult<CallHierarchyIncomingCall[]>
  provideCallHierarchyOutgoingCalls?: (this: void, item: CallHierarchyItem, token: CancellationToken, next: CallHierarchyOutgoingCallsSignature) => ProviderResult<CallHierarchyOutgoingCall[]>
}

export class CallHierarchyFeature extends TextDocumentFeature<boolean | CallHierarchyOptions, CallHierarchyRegistrationOptions, CallHierarchyProvider> {
  constructor(client: BaseLanguageClient) {
    super(client, CallHierarchyPrepareRequest.type)
  }

  public fillClientCapabilities(cap: ClientCapabilities): void {
    const capabilities: ClientCapabilities & CallHierarchyClientCapabilities = cap as ClientCapabilities & CallHierarchyClientCapabilities
    const capability = ensure(ensure(capabilities, 'textDocument')!, 'callHierarchy')!
    capability.dynamicRegistration = true
  }

  public initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector): void {
    const [id, options] = this.getRegistration(documentSelector, capabilities.callHierarchyProvider)
    if (!id || !options) {
      return
    }
    this.register({ id, registerOptions: options })
  }

  protected registerLanguageProvider(options: CallHierarchyRegistrationOptions): [Disposable, CallHierarchyProvider] {
    const provider: CallHierarchyProvider = {
      prepareCallHierarchy: (document: TextDocument, position: Position, token: CancellationToken) => {
        const client = this._client
        const prepareCallHierarchy: PrepareCallHierarchySignature = (document, position, token) => {
          const params = asTextDocumentPositionParams(document, position)
          return client.sendRequest(CallHierarchyPrepareRequest.type, params, token).then(
            res => res,
            error => {
              return client.handleFailedRequest(CallHierarchyPrepareRequest.type, token, error, null)
            }
          )
        }

        const middleware = client.clientOptions.middleware
        return middleware.prepareCallHierarchy
          ? middleware.prepareCallHierarchy(document, position, token, prepareCallHierarchy)
          : prepareCallHierarchy(document, position, token)
      },

      provideCallHierarchyIncomingCalls: (item: CallHierarchyItem, token: CancellationToken) => {
        const client = this._client
        const provideCallHierarchyIncomingCalls: CallHierarchyIncomingCallsSignature = (item, token) => {
          return client.sendRequest(CallHierarchyIncomingCallsRequest.type, { item }, token).then(
            res => res,
            error => {
              return client.handleFailedRequest(CallHierarchyIncomingCallsRequest.type, token, error, null)
            }
          )
        }

        const middleware = client.clientOptions.middleware
        return middleware.provideCallHierarchyIncomingCalls
          ? middleware.provideCallHierarchyIncomingCalls(item, token, provideCallHierarchyIncomingCalls)
          : provideCallHierarchyIncomingCalls(item, token)
      },

      provideCallHierarchyOutgoingCalls: (item: CallHierarchyItem, token: CancellationToken) => {
        const client = this._client
        const provideCallHierarchyOutgoingCalls: CallHierarchyOutgoingCallsSignature = (item, token) => {
          return client.sendRequest(CallHierarchyOutgoingCallsRequest.type, { item }, token).then(
            res => res,
            error => {
              return client.handleFailedRequest(CallHierarchyOutgoingCallsRequest.type, token, error, null)
            }
          )
        }

        const middleware = client.clientOptions.middleware
        return middleware.provideCallHierarchyOutgoingCalls
          ? middleware.provideCallHierarchyOutgoingCalls(item, token, provideCallHierarchyOutgoingCalls)
          : provideCallHierarchyOutgoingCalls(item, token)
      }
    }

    return [languages.registerCallHierarchyProvider(options.documentSelector, provider), provider]
  }
}
