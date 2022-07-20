'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict'

import {
  CancellationToken, ClientCapabilities, Disposable, DocumentSelector, Emitter, Range, SemanticTokenModifiers, SemanticTokens, SemanticTokensDelta, SemanticTokensDeltaParams, SemanticTokensDeltaRequest, SemanticTokensOptions, SemanticTokensParams, SemanticTokensRangeParams, SemanticTokensRangeRequest, SemanticTokensRefreshRequest, SemanticTokensRegistrationOptions, SemanticTokensRegistrationType, SemanticTokensRequest, SemanticTokenTypes, ServerCapabilities, TokenFormat
} from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import languages from '../languages'
import { DocumentRangeSemanticTokensProvider, DocumentSemanticTokensProvider, ProviderResult } from '../provider'
import * as Is from '../util/is'
import { BaseLanguageClient, ensure, Middleware, TextDocumentFeature } from './client'
import * as cv from './utils/converter'
const logger = require('../util/logger')('languageclient-semanticTokens')

export interface DocumentSemanticsTokensSignature {
  (this: void, document: TextDocument, token: CancellationToken): ProviderResult<SemanticTokens>
}

export interface DocumentSemanticsTokensEditsSignature {
  (this: void, document: TextDocument, previousResultId: string, token: CancellationToken): ProviderResult<SemanticTokens | SemanticTokensDelta>
}

export interface DocumentRangeSemanticTokensSignature {
  (this: void, document: TextDocument, range: Range, token: CancellationToken): ProviderResult<SemanticTokens>
}

/**
 * The semantic token middleware
 *
 * @since 3.16.0
 */
export interface SemanticTokensMiddleware {
  provideDocumentSemanticTokens?: (this: void, document: TextDocument, token: CancellationToken, next: DocumentSemanticsTokensSignature) => ProviderResult<SemanticTokens>
  provideDocumentSemanticTokensEdits?: (this: void, document: TextDocument, previousResultId: string, token: CancellationToken, next: DocumentSemanticsTokensEditsSignature) => ProviderResult<SemanticTokens | SemanticTokensDelta>
  provideDocumentRangeSemanticTokens?: (this: void, document: TextDocument, range: Range, token: CancellationToken, next: DocumentRangeSemanticTokensSignature) => ProviderResult<SemanticTokens>
}

export interface SemanticTokensProviders {
  range?: DocumentRangeSemanticTokensProvider
  full?: DocumentSemanticTokensProvider
  onDidChangeSemanticTokensEmitter: Emitter<void>
}

export class SemanticTokensFeature extends TextDocumentFeature<boolean | SemanticTokensOptions, SemanticTokensRegistrationOptions, SemanticTokensProviders> {

  constructor(client: BaseLanguageClient) {
    super(client, SemanticTokensRegistrationType.type)
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    const capability = ensure(ensure(capabilities, 'textDocument')!, 'semanticTokens')!
    capability.dynamicRegistration = true
    capability.tokenTypes = [
      SemanticTokenTypes.namespace,
      SemanticTokenTypes.type,
      SemanticTokenTypes.class,
      SemanticTokenTypes.enum,
      SemanticTokenTypes.interface,
      SemanticTokenTypes.struct,
      SemanticTokenTypes.typeParameter,
      SemanticTokenTypes.parameter,
      SemanticTokenTypes.variable,
      SemanticTokenTypes.property,
      SemanticTokenTypes.enumMember,
      SemanticTokenTypes.event,
      SemanticTokenTypes.function,
      SemanticTokenTypes.method,
      SemanticTokenTypes.macro,
      SemanticTokenTypes.keyword,
      SemanticTokenTypes.modifier,
      SemanticTokenTypes.comment,
      SemanticTokenTypes.string,
      SemanticTokenTypes.number,
      SemanticTokenTypes.regexp,
      SemanticTokenTypes.operator
    ]
    capability.tokenModifiers = [
      SemanticTokenModifiers.declaration,
      SemanticTokenModifiers.definition,
      SemanticTokenModifiers.readonly,
      SemanticTokenModifiers.static,
      SemanticTokenModifiers.deprecated,
      SemanticTokenModifiers.abstract,
      SemanticTokenModifiers.async,
      SemanticTokenModifiers.modification,
      SemanticTokenModifiers.documentation,
      SemanticTokenModifiers.defaultLibrary
    ]
    capability.formats = [TokenFormat.Relative]
    capability.requests = {
      range: true,
      full: {
        delta: true
      }
    }
    capability.multilineTokenSupport = false
    capability.overlappingTokenSupport = false
    ensure(ensure(capabilities, 'workspace')!, 'semanticTokens')!.refreshSupport = true
  }

  public initialize(capabilities: ServerCapabilities, documentSelector: DocumentSelector): void {
    const client = this._client
    client.onRequest(SemanticTokensRefreshRequest.type, async () => {
      for (const provider of this.getAllProviders()) {
        provider.onDidChangeSemanticTokensEmitter.fire()
      }
    })
    const [id, options] = this.getRegistration(documentSelector, capabilities.semanticTokensProvider)
    if (!id || !options) {
      return
    }
    this.register({ id, registerOptions: options })
  }

  protected registerLanguageProvider(options: SemanticTokensRegistrationOptions): [Disposable, SemanticTokensProviders] {
    const fullProvider = Is.boolean(options.full) ? options.full : options.full !== undefined
    const hasEditProvider = options.full !== undefined && typeof options.full !== 'boolean' && options.full.delta === true
    const eventEmitter: Emitter<void> = new Emitter<void>()
    const documentProvider: DocumentSemanticTokensProvider | undefined = fullProvider
      ? {
        onDidChangeSemanticTokens: eventEmitter.event,
        provideDocumentSemanticTokens: (document, token) => {
          const client = this._client
          const middleware = client.clientOptions.middleware! as Middleware & SemanticTokensMiddleware
          const provideDocumentSemanticTokens: DocumentSemanticsTokensSignature = (document, token) => {
            const params: SemanticTokensParams = {
              textDocument: cv.asTextDocumentIdentifier(document)
            }
            return client.sendRequest(SemanticTokensRequest.type, params, token).then(result => result, (error: any) => {
              return client.handleFailedRequest(SemanticTokensRequest.type, token, error, null)
            })
          }
          return middleware.provideDocumentSemanticTokens
            ? middleware.provideDocumentSemanticTokens(document, token, provideDocumentSemanticTokens)
            : provideDocumentSemanticTokens(document, token)
        },
        provideDocumentSemanticTokensEdits: hasEditProvider
          ? (document, previousResultId, token) => {
            const client = this._client
            const middleware = client.clientOptions.middleware! as Middleware & SemanticTokensMiddleware
            const provideDocumentSemanticTokensEdits: DocumentSemanticsTokensEditsSignature = (document, previousResultId, token) => {
              const params: SemanticTokensDeltaParams = {
                textDocument: cv.asTextDocumentIdentifier(document),
                previousResultId
              }
              return client.sendRequest(SemanticTokensDeltaRequest.type, params, token).then(result => result, (error: any) => {
                return client.handleFailedRequest(SemanticTokensDeltaRequest.type, token, error, null)
              })
            }
            return middleware.provideDocumentSemanticTokensEdits
              ? middleware.provideDocumentSemanticTokensEdits(document, previousResultId, token, provideDocumentSemanticTokensEdits)
              : provideDocumentSemanticTokensEdits(document, previousResultId, token)
          }
          : undefined
      }
      : undefined

    const hasRangeProvider: boolean = options.range === true
    const rangeProvider: DocumentRangeSemanticTokensProvider | undefined = hasRangeProvider
      ? {
        provideDocumentRangeSemanticTokens: (document: TextDocument, range: Range, token: CancellationToken) => {
          const client = this._client
          const middleware = client.clientOptions.middleware! as Middleware & SemanticTokensMiddleware
          const provideDocumentRangeSemanticTokens: DocumentRangeSemanticTokensSignature = (document, range, token) => {
            const params: SemanticTokensRangeParams = {
              textDocument: cv.asTextDocumentIdentifier(document),
              range
            }
            return client.sendRequest(SemanticTokensRangeRequest.type, params, token).then(
              result => result,
              (error: any) => {
                return client.handleFailedRequest(SemanticTokensRangeRequest.type, token, error, null)
              })
          }
          return middleware.provideDocumentRangeSemanticTokens
            ? middleware.provideDocumentRangeSemanticTokens(document, range, token, provideDocumentRangeSemanticTokens)
            : provideDocumentRangeSemanticTokens(document, range, token)
        }
      }
      : undefined

    const disposables: Disposable[] = []
    if (documentProvider !== undefined) {
      disposables.push(languages.registerDocumentSemanticTokensProvider(options.documentSelector!, documentProvider, options.legend))
    }
    if (rangeProvider !== undefined) {
      disposables.push(languages.registerDocumentRangeSemanticTokensProvider(options.documentSelector!, rangeProvider, options.legend))
    }

    return [Disposable.create(() => disposables.forEach(item => item.dispose())), { range: rangeProvider, full: documentProvider, onDidChangeSemanticTokensEmitter: eventEmitter }]
  }
}
