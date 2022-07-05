'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import { Disposable, NotificationHandler, ProgressToken, ProgressType, ProtocolNotificationType, WorkDoneProgress, WorkDoneProgressBegin, WorkDoneProgressReport } from 'vscode-languageserver-protocol'
import { disposeAll } from '../util'
import window from '../window'
import workspace from '../workspace'
const logger = require('../util/logger')('language-client-progressPart')

export interface Progress {
  report(value: { message?: string; increment?: number }): void
}

export interface ProgressContext {
  onProgress<P>(type: ProgressType<P>, token: string | number, handler: NotificationHandler<P>): Disposable
  sendNotification<P, RO>(type: ProtocolNotificationType<P, RO>, params?: P): void
}

export class ProgressPart {
  private disposables: Disposable[] = []
  private _cancelled = false
  private _percent = 0
  private _started = false
  private progress: Progress
  private _resolve: () => void

  public constructor(private id: string, client: ProgressContext, private token: ProgressToken, done?: (part: ProgressPart) => void) {
    if (!workspace.env.dialog) return
    this.disposables.push(client.onProgress(WorkDoneProgress.type, this.token, value => {
      switch (value.kind) {
        case 'begin':
          this.begin(value)
          break
        case 'report':
          this.report(value)
          break
        case 'end':
          this.done(value.message)
          done && done(this)
          break
      }
    }))
  }

  public begin(params: WorkDoneProgressBegin): void {
    if (this._started) return
    this._started = true
    window.withProgress<void>({
      source: `language-client-${this.id}`,
      cancellable: params.cancellable,
      title: params.title,
    }, (progress, token) => {
      this.progress = progress
      this.report(params)
      return new Promise(resolve => {
        params.cancellable && token.onCancellationRequested(() => {
          this.cancel()
          resolve()
        })
        this._resolve = resolve
      })
    }).catch(e => {
      void window.showErrorMessage(e.message)
    }).finally(() => {
      this._resolve = undefined
      this.progress = undefined
    })
  }

  private report(params: WorkDoneProgressReport | WorkDoneProgressBegin): void {
    if (!this.progress) return
    let msg: { message?: string, increment?: number } = {}
    if (params.message) msg.message = params.message
    if (validPercent(params.percentage)) {
      msg.increment = params.percentage - this._percent
      this._percent = params.percentage
    }
    if (Object.keys(msg).length > 0) {
      this.progress.report(msg)
    }
  }

  public cancel(): void {
    if (this._cancelled) return
    this._cancelled = true
    disposeAll(this.disposables)
  }

  public done(message?: string): void {
    if (this.progress) {
      let msg: { message?: string, increment?: number } = {}
      if (message) msg.message = message
      if (this._percent > 0) msg.increment = 100 - this._percent
      this.progress.report(msg)
    }
    setTimeout(() => {
      if (this._resolve) {
        this._resolve()
      }
    }, 300)
    this.cancel()
  }
}

function validPercent(n: unknown): boolean {
  if (typeof n !== 'number') return false
  return n >= 0 && n <= 100
}
