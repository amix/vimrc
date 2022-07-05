'use strict'
import { Neovim } from '@chemzqm/neovim'
import debounce from 'debounce'
import { CancellationTokenSource, Emitter, Event, Range } from 'vscode-languageserver-protocol'
import languages from '../../languages'
import { SyncItem } from '../../model/bufferSync'
import Document from '../../model/document'
import Regions from '../../model/regions'
import { getLabel, InlayHintWithProvider } from '../../provider/inlayHintManager'
import { positionInRange } from '../../util/position'

export interface InlayHintConfig {
  srcId?: number
}

const debounceInterval = global.hasOwnProperty('__TEST__') ? 10 : 100
const highlightGroup = 'CocInlayHint'

export default class InlayHintBuffer implements SyncItem {
  private tokenSource: CancellationTokenSource
  private regions = new Regions()
  // Saved for resolve and TextEdits in the future.
  private currentHints: InlayHintWithProvider[] = []
  private readonly _onDidRefresh = new Emitter<void>()
  public readonly onDidRefresh: Event<void> = this._onDidRefresh.event
  public render: Function & { clear(): void }
  constructor(
    private readonly nvim: Neovim,
    public readonly doc: Document,
    private readonly config: InlayHintConfig
  ) {
    this.render = debounce(() => {
      void this.renderRange()
    }, debounceInterval)
    this.render()
  }

  public get current(): ReadonlyArray<InlayHintWithProvider> {
    return this.currentHints
  }

  public clearCache(): void {
    this.currentHints = []
    this.regions.clear()
    this.render.clear()
  }

  public onChange(): void {
    this.clearCache()
    this.cancel()
    this.render()
  }

  public cancel(): void {
    this.render.clear()
    if (this.tokenSource) {
      this.tokenSource.cancel()
      this.tokenSource = null
    }
  }

  public async renderRange(): Promise<void> {
    this.cancel()
    if (!languages.hasProvider('inlayHint', this.doc.textDocument)) return
    this.tokenSource = new CancellationTokenSource()
    let token = this.tokenSource.token
    let res = await this.nvim.call('coc#window#visible_range', [this.doc.bufnr]) as [number, number]
    if (res == null || this.doc.dirty || token.isCancellationRequested) return
    if (this.regions.has(res[0], res[1])) return
    let range = Range.create(res[0] - 1, 0, res[1], 0)
    let inlayHints = await languages.provideInlayHints(this.doc.textDocument, range, token)
    if (inlayHints == null || token.isCancellationRequested) return
    this.regions.add(res[0], res[1])
    this.currentHints = this.currentHints.filter(o => positionInRange(o.position, range) !== 0)
    this.currentHints.push(...inlayHints)
    this.setVirtualText(range, inlayHints)
  }

  private setVirtualText(range: Range, inlayHints: InlayHintWithProvider[]): void {
    let { nvim, doc } = this
    let srcId = this.config.srcId
    let buffer = doc.buffer
    const chunksMap = {}
    for (const item of inlayHints) {
      const chunks: [[string, string]] = [[getLabel(item), highlightGroup]]
      if (chunksMap[item.position.line] === undefined) {
        chunksMap[item.position.line] = chunks
      } else {
        chunksMap[item.position.line].push([' ', 'Normal'])
        chunksMap[item.position.line].push(chunks[0])
      }
    }
    nvim.pauseNotification()
    buffer.clearNamespace(srcId, range.start.line, range.end.line + 1)
    for (let key of Object.keys(chunksMap)) {
      buffer.setExtMark(srcId, Number(key), 0, {
        virt_text: chunksMap[key],
        virt_text_pos: 'eol',
        hl_mode: 'combine'
      })
    }
    nvim.resumeNotification(false, true)
    this._onDidRefresh.fire()
  }

  public clearVirtualText(): void {
    let srcId = this.config.srcId
    this.doc.buffer.clearNamespace(srcId)
  }

  public dispose(): void {
    this.cancel()
  }
}
