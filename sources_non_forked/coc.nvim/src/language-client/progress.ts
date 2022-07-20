'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict'

import { ClientCapabilities, WorkDoneProgressCreateParams, WorkDoneProgressCreateRequest } from 'vscode-languageserver-protocol'
import { BaseLanguageClient, ensure, StaticFeature } from './client'
import { ProgressPart } from './progressPart'
// const logger = require('../util/logger')('language-client-progress')

export class ProgressFeature implements StaticFeature {
  private activeParts: Set<ProgressPart> = new Set()
  constructor(private _client: BaseLanguageClient) {
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    ensure(capabilities, 'window')!.workDoneProgress = true
  }

  public initialize(): void {
    let client = this._client
    const deleteHandler = (part: ProgressPart) => {
      this.activeParts.delete(part)
    }
    const createHandler = (params: WorkDoneProgressCreateParams) => {
      this.activeParts.add(new ProgressPart(this._client.id, this._client, params.token, deleteHandler))
    }
    client.onRequest(WorkDoneProgressCreateRequest.type, createHandler)
  }

  public dispose(): void {
    for (const part of this.activeParts) {
      part.done()
    }
    this.activeParts.clear()
  }
}
