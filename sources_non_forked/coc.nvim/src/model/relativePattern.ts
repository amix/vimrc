'use strict'
import { URI } from 'vscode-uri'
import { illegalArgument } from '../util/errors'
import { WorkspaceFolder } from 'vscode-languageserver-protocol'

export default class RelativePattern {
  public pattern: string
  public baseUri: URI

  constructor(base: WorkspaceFolder | URI | string, pattern: string) {
    if (typeof base !== 'string') {
      if (!base || !URI.isUri(base) && typeof base.uri !== 'string') {
        throw illegalArgument('base')
      }
    }
    if (typeof pattern !== 'string') {
      throw illegalArgument('pattern')
    }
    if (typeof base === 'string') {
      this.baseUri = URI.file(base)
    } else if (URI.isUri(base)) {
      this.baseUri = base
    } else {
      this.baseUri = URI.parse(base.uri)
    }
    this.pattern = pattern
  }

  public toJSON() {
    return {
      pattern: this.pattern,
      baseUri: this.baseUri.toJSON()
    }
  }
}
