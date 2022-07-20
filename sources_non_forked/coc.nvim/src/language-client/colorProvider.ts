'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict'

import { CancellationToken, ClientCapabilities, Color, ColorInformation, ColorPresentation, ColorPresentationRequest, Disposable, DocumentColorOptions, DocumentColorRegistrationOptions, DocumentColorRequest, DocumentSelector, Range, ServerCapabilities } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import languages from '../languages'
import { DocumentColorProvider, ProviderResult } from '../provider'
import { BaseLanguageClient, ensure, TextDocumentFeature } from './client'

export type ProvideDocumentColorsSignature = (document: TextDocument, token: CancellationToken) => ProviderResult<ColorInformation[]>

export type ProvideColorPresentationSignature = (
  color: Color,
  context: { document: TextDocument; range: Range },
  token: CancellationToken
) => ProviderResult<ColorPresentation[]>

export interface ColorProviderMiddleware {
  provideDocumentColors?: (
    this: void,
    document: TextDocument,
    token: CancellationToken,
    next: ProvideDocumentColorsSignature
  ) => ProviderResult<ColorInformation[]>
  provideColorPresentations?: (
    this: void,
    color: Color,
    context: { document: TextDocument; range: Range },
    token: CancellationToken,
    next: ProvideColorPresentationSignature
  ) => ProviderResult<ColorPresentation[]>
}

export class ColorProviderFeature extends TextDocumentFeature<
  boolean | DocumentColorOptions, DocumentColorRegistrationOptions, DocumentColorProvider
  > {
  constructor(client: BaseLanguageClient) {
    super(client, DocumentColorRequest.type)
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    ensure(ensure(capabilities, 'textDocument')!, 'colorProvider')!.dynamicRegistration = true
  }

  public initialize(
    capabilities: ServerCapabilities,
    documentSelector: DocumentSelector
  ): void {
    let [id, options] = this.getRegistration(documentSelector, capabilities.colorProvider)
    if (!id || !options) {
      return
    }

    this.register({ id, registerOptions: options })
  }

  protected registerLanguageProvider(
    options: DocumentColorRegistrationOptions
  ): [Disposable, DocumentColorProvider] {
    const provider: DocumentColorProvider = {
      provideColorPresentations: (color, context, token) => {
        const client = this._client
        const provideColorPresentations: ProvideColorPresentationSignature = (color, context, token) => {
          const requestParams = {
            color,
            textDocument: { uri: context.document.uri },
            range: context.range
          }
          return client.sendRequest(ColorPresentationRequest.type, requestParams, token).then(
            res => res,
            (error: any) => {
              return client.handleFailedRequest(ColorPresentationRequest.type, token, error, null)
            }
          )
        }
        const middleware = client.clientOptions.middleware
        return middleware.provideColorPresentations
          ? middleware.provideColorPresentations(color, context, token, provideColorPresentations)
          : provideColorPresentations(color, context, token)
      },
      provideDocumentColors: (document, token) => {
        const client = this._client
        const provideDocumentColors: ProvideDocumentColorsSignature = (document, token) => {
          const requestParams = {
            textDocument: { uri: document.uri }
          }
          return client.sendRequest(DocumentColorRequest.type, requestParams, token).then(
            res => res,
            (error: any) => {
              return client.handleFailedRequest(ColorPresentationRequest.type, token, error, null)
            }
          )
        }
        const middleware = client.clientOptions.middleware
        return middleware.provideDocumentColors
          ? middleware.provideDocumentColors(document, token, provideDocumentColors)
          : provideDocumentColors(document, token)
      }
    }

    return [languages.registerDocumentColorProvider(options.documentSelector, provider), provider]
  }
}
