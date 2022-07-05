'use strict'
import { CancellationTokenSource } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import events from '../events'
import { SyncItem } from '../model/bufferSync'
import Document from '../model/document'
import { DidChangeTextDocumentParams } from '../types'
import { isGitIgnored } from '../util/fs'
const logger = require('../util/logger')('sources-keywords')
const MAX_LENGTH = 10 * 1024 // 10KB

export default class KeywordsBuffer implements SyncItem {
  private _words: Set<string> = new Set()
  private _gitIgnored = false
  private version: number
  private lineCount: number
  private tokenSource: CancellationTokenSource
  constructor(private doc: Document) {
    this.parse()
    let uri = URI.parse(doc.uri)
    if (uri.scheme === 'file') {
      void isGitIgnored(uri.fsPath).then(ignored => {
        this._gitIgnored = ignored
      })
    }
  }

  public get bufnr(): number {
    return this.doc.bufnr
  }

  public get gitIgnored(): boolean {
    return this._gitIgnored
  }

  public get words(): Set<string> {
    return this._words
  }

  public parse(): void {
    if (!this.doc.attached || events.completing) return
    let { textDocument } = this.doc
    let { version, lineCount } = textDocument
    if (this.version === version) return
    if (events.insertMode
      && this.lineCount == lineCount
      && textDocument.length > MAX_LENGTH) return
    this.cancel()
    let tokenSource = this.tokenSource = new CancellationTokenSource()
    void this.doc.matchWords(tokenSource.token).then(res => {
      if (res != null) {
        this._words = res
        this.lineCount = lineCount
        this.version = version
      }
    })
  }

  private cancel(): void {
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource = null
    }
  }

  public onChange(e: DidChangeTextDocumentParams): void {
    if (e.contentChanges.length == 0) return
    this.parse()
  }

  public dispose(): void {
    this.cancel()
    this._words.clear()
  }
}
