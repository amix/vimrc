'use strict'
import { Neovim } from '@chemzqm/neovim'
import { CancellationTokenSource, Disposable, DocumentLink, Range } from 'vscode-languageserver-protocol'
import events from '../events'
import languages from '../languages'
import FloatFactory from '../model/floatFactory'
import { ConfigurationChangeEvent, Documentation, HandlerDelegate } from '../types'
import { disposeAll } from '../util'
import { positionInRange } from '../util/position'
import window from '../window'
import workspace from '../workspace'
const logger = require('../util/logger')('handler-links')

const regex = /CocAction(Async)?\(["']openLink["']\)/
export default class Links implements Disposable {
  private floatFactory: FloatFactory | undefined
  private disposables: Disposable[] = []
  private _tooltip: boolean
  private tokenSource: CancellationTokenSource
  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
    this.setConfiguration()
    workspace.onDidChangeConfiguration(this.setConfiguration, this, this.disposables)
    this.floatFactory = new FloatFactory(nvim)
    events.on('CursorHold', async () => {
      if (!this._tooltip) return
      if (!nvim.hasFunction('nvim_get_keymap')) return
      await this.showTooltip()
    }, null, this.disposables)
    events.on(['CursorMoved', 'InsertEnter'], () => {
      this.cancel()
    }, null, this.disposables)
  }

  private setConfiguration(e?: ConfigurationChangeEvent): void {
    if (!e || e.affectsConfiguration('links')) {
      let config = workspace.getConfiguration('links')
      this._tooltip = config.get<boolean>('tooltip', false)
    }
  }

  public async showTooltip(): Promise<void> {
    let { nvim, floatFactory } = this
    let obj = await nvim.getKeymap('n') as any[]
    let find = obj.find(o => regex.test(o.rhs))
    let key = find ? find.lhs : undefined
    let link = await this.getCurrentLink()
    if (!link || !link.target) return
    let text = ''
    if (link.tooltip) text = link.tooltip + ' '
    if (key) text += `Press "${key}" to open link`
    if (!text.length) return
    let doc: Documentation = { content: text, filetype: 'txt' }
    await floatFactory.show([doc], { autoHide: true })
  }

  public async getLinks(): Promise<DocumentLink[]> {
    try {
      let { doc } = await this.handler.getCurrentState()
      if (!languages.hasProvider('documentLink', doc.textDocument)) return []
      let tokenSource = this.tokenSource = new CancellationTokenSource()
      let links = await languages.getDocumentLinks(doc.textDocument, tokenSource.token)
      return tokenSource.token.isCancellationRequested ? [] : links
    } catch (_) {
      return []
    }
  }

  public async openLink(link: DocumentLink): Promise<void> {
    if (!link.target) throw new Error(`Failed to resolve link target`)
    await workspace.openResource(link.target)
  }

  public async getCurrentLink(): Promise<DocumentLink | undefined> {
    let links = await this.getLinks()
    let pos = await window.getCursorPosition()
    if (links && links.length) {
      for (let link of links) {
        if (positionInRange(pos, link.range) == 0) {
          if (!link.target) {
            let tokenSource = this.tokenSource = this.tokenSource || new CancellationTokenSource()
            link = await languages.resolveDocumentLink(link, this.tokenSource.token)
            if (!link.target || tokenSource.token.isCancellationRequested) continue
          }
          return link
        }
      }
    }
    let line = await this.nvim.call('getline', ['.'])
    let regex = /\w+?:\/\/[^)\]'" ]+/g
    let arr
    let link: DocumentLink | undefined
    while ((arr = regex.exec(line)) !== null) {
      let start = arr.index
      if (start <= pos.character && start + arr[0].length >= pos.character) {
        link = DocumentLink.create(Range.create(pos.line, start, pos.line, start + arr[0].length), arr[0])
        break
      }
    }
    return link
  }

  public async openCurrentLink(): Promise<boolean> {
    let link = await this.getCurrentLink()
    if (link) {
      await this.openLink(link)
      return true
    }
    return false
  }

  private cancel(): void {
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource = null
    }
  }

  public dispose(): void {
    this.floatFactory?.dispose()
    disposeAll(this.disposables)
  }
}
