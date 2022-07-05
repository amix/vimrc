'use strict'
import { Neovim } from '@chemzqm/neovim'
import events from '../../events'
import languages from '../../languages'
import BufferSync from '../../model/bufferSync'
import { HandlerDelegate } from '../../types'
import workspace from '../../workspace'
import InlayHintBuffer, { InlayHintConfig } from './buffer'

export default class InlayHintHandler {
  private config: InlayHintConfig = {}
  private buffers: BufferSync<InlayHintBuffer> | undefined
  constructor(nvim: Neovim, handler: HandlerDelegate) {
    void nvim.createNamespace('coc-inlayHint').then(id => {
      this.config.srcId = id
    })
    this.buffers = workspace.registerBufferSync(doc => {
      if (!workspace.has('nvim-0.5.0')) return undefined
      return new InlayHintBuffer(nvim, doc, this.config)
    })
    handler.addDisposable(this.buffers)
    handler.addDisposable(languages.onDidInlayHintRefresh(async e => {
      for (let item of this.buffers.items) {
        if (workspace.match(e, item.doc.textDocument)) {
          item.clearCache()
          if (languages.hasProvider('inlayHint', item.doc.textDocument)) {
            await item.renderRange()
          } else {
            item.clearVirtualText()
          }
        }
      }
    }))
    handler.addDisposable(events.on('CursorMoved', bufnr => {
      this.refresh(bufnr)
    }))
    handler.addDisposable(events.on('WinScrolled', async winid => {
      let bufnr = await nvim.call('winbufnr', [winid])
      if (bufnr != -1) this.refresh(bufnr)
    }))
  }

  public getItem(bufnr: number): InlayHintBuffer {
    return this.buffers.getItem(bufnr)
  }

  public refresh(bufnr: number): void {
    let buf = this.buffers.getItem(bufnr)
    if (buf) buf.render()
  }
}
