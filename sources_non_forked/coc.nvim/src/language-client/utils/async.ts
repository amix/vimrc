'use strict'
/* --------------------------------------------------------------------------------------------
  * Copyright (c) Microsoft Corporation. All rights reserved.
  * Licensed under the MIT License. See License.txt in the project root for license information.
  * ------------------------------------------------------------------------------------------ */

import { RAL } from 'vscode-languageserver-protocol'

export interface ITask<T> {
  (): T
}

export class Delayer<T> {

  public defaultDelay: number
  private timeout: RAL.TimeoutHandle | undefined
  private completionPromise: Promise<T> | undefined
  private onSuccess: ((value: T | Promise<T> | undefined) => void) | undefined
  private task: ITask<T> | undefined

  constructor(defaultDelay: number) {
    this.defaultDelay = defaultDelay
    this.timeout = undefined
    this.completionPromise = undefined
    this.onSuccess = undefined
    this.task = undefined
  }

  public trigger(task: ITask<T>, delay: number = this.defaultDelay): Promise<T> {
    this.task = task
    if (delay >= 0) {
      this.cancelTimeout()
    }

    if (!this.completionPromise) {
      this.completionPromise = new Promise<T | undefined>(resolve => {
        this.onSuccess = resolve
      }).then(() => {
        this.completionPromise = undefined
        this.onSuccess = undefined
        let result = this.task!()
        this.task = undefined
        return result
      })
    }

    if (delay >= 0 || this.timeout === void 0) {
      this.timeout = RAL().timer.setTimeout(() => {
        this.timeout = undefined
        this.onSuccess!(undefined)
      }, delay >= 0 ? delay : this.defaultDelay)
    }

    return this.completionPromise
  }

  public forceDelivery(): T | undefined {
    if (!this.completionPromise) {
      return undefined
    }
    this.cancelTimeout()
    let result: T = this.task!()
    this.completionPromise = undefined
    this.onSuccess = undefined
    this.task = undefined
    return result
  }

  public isTriggered(): boolean {
    return this.timeout !== void 0
  }

  public cancel(): void {
    this.cancelTimeout()
    this.completionPromise = undefined
  }

  public dispose(): void {
    this.cancelTimeout()
  }

  private cancelTimeout(): void {
    if (this.timeout !== void 0) {
      RAL().timer.clearTimeout(this.timeout)
      this.timeout = undefined
    }
  }
}
