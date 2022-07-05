'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import commands from '../../commands'
import events from '../../events'
import languages from '../../languages'
import BufferSync from '../../model/bufferSync'
import FloatFactory from '../../model/floatFactory'
import Highlighter from '../../model/highligher'
import { ConfigurationChangeEvent, Documentation, HandlerDelegate } from '../../types'
import { disposeAll } from '../../util'
import { distinct } from '../../util/array'
import { upperFirst } from '../../util/string'
import workspace from '../../workspace'
import SemanticTokensBuffer, { HLGROUP_PREFIX, NAMESPACE, SemanticTokensConfig } from './buffer'
const logger = require('../../util/logger')('semanticTokens')
const headGroup = 'Statement'

export default class SemanticTokens {
  // shared with buffers
  private config: SemanticTokensConfig
  private disposables: Disposable[] = []
  private highlighters: BufferSync<SemanticTokensBuffer>
  private floatFactory: FloatFactory

  constructor(private nvim: Neovim, private handler: HandlerDelegate) {
    this.loadConfiguration()
    this.floatFactory = new FloatFactory(nvim)
    workspace.onDidChangeConfiguration(this.loadConfiguration, this, this.disposables)
    commands.register({
      id: 'semanticTokens.checkCurrent',
      execute: async () => {
        await this.showHighlightInfo()
      }
    }, false, 'show semantic tokens highlight information of current buffer')
    commands.register({
      id: 'semanticTokens.refreshCurrent',
      execute: () => {
        return this.highlightCurrent()
      }
    }, false, 'refresh semantic tokens highlight of current buffer.')
    commands.register({
      id: 'semanticTokens.inspect',
      execute: () => {
        return this.inspectSemanticToken()
      }
    }, false, 'Inspect semantic token information at cursor position.')
    commands.register({
      id: 'semanticTokens.clearCurrent',
      execute: async () => {
        let buf = await nvim.buffer
        buf.clearNamespace(NAMESPACE, 0, -1)
      }
    }, false, 'clear semantic tokens highlight of current buffer')
    commands.register({
      id: 'semanticTokens.clearAll',
      execute: async () => {
        let bufs = await nvim.buffers
        for (let buf of bufs) {
          buf.clearNamespace(NAMESPACE, 0, -1)
        }
      }
    }, false, 'clear semantic tokens highlight of all buffers')
    this.highlighters = workspace.registerBufferSync(doc => {
      return new SemanticTokensBuffer(this.nvim, doc, this.config)
    })
    languages.onDidSemanticTokensRefresh(async selector => {
      let visibleBufs = await this.nvim.call('coc#window#bufnrs') as number[]
      for (let item of this.highlighters.items) {
        let doc = workspace.getDocument(item.bufnr)
        if (!doc || !workspace.match(selector, doc.textDocument)) continue
        item.abandonResult()
        if (visibleBufs.includes(item.bufnr)) {
          item.highlight()
        }
      }
    }, null, this.disposables)
    events.on('BufWinEnter', async bufnr => {
      let item = this.highlighters.getItem(bufnr)
      if (item) await item.onShown()
    }, null, this.disposables)
    events.on('CursorMoved', async bufnr => {
      let item = this.highlighters.getItem(bufnr)
      if (item) await item.onCursorMoved()
    }, null, this.disposables)
  }

  private loadConfiguration(e?: ConfigurationChangeEvent): void {
    if (!e || e.affectsConfiguration('semanticTokens')) {
      let highlightGroups = []
      if (this.config?.highlightGroups) {
        highlightGroups = this.config.highlightGroups
      } else {
        highlightGroups = workspace.env.semanticHighlights || []
      }
      let config = workspace.getConfiguration('semanticTokens')
      this.config = Object.assign(this.config || {}, {
        highlightGroups,
        filetypes: config.get<string[]>('filetypes', []),
        highlightPriority: config.get<number>('highlightPriority', 2048),
        incrementTypes: config.get<string[]>('incrementTypes', []),
        combinedModifiers: config.get<string[]>('combinedModifiers', [])
      })
    }
  }

  public async inspectSemanticToken(): Promise<void> {
    let item = await this.getCurrentItem()
    if (!item || !item.enabled) {
      this.floatFactory.close()
      return
    }
    let [_, line, col] = await this.nvim.call('getcurpos', [])
    let highlights = item.highlights ?? []
    let highlight = highlights.find(o => {
      let column = col - 1
      return o.range[0] === line - 1 && column >= o.range[1] && column < o.range[2]
    })
    if (highlight) {
      let modifiers = highlight.tokenModifiers || []
      let highlights = []
      if (highlight.hlGroup) {
        let s = 'Highlight group: '.length
        highlights.push({
          lnum: 2,
          colStart: s,
          colEnd: s + highlight.hlGroup.length,
          hlGroup: highlight.hlGroup
        })
      }
      let docs: Documentation[] = [{
        filetype: 'txt',
        content: `Type: ${highlight.tokenType}\nModifiers: ${modifiers.join(', ')}\nHighlight group: ${highlight.hlGroup || ''}`,
        highlights
      }]
      await this.floatFactory.show(docs, {
        autoHide: true,
        focusable: true,
        title: 'Semantic token info',
        borderhighlight: 'MoreMsg',
        border: [1, 1, 1, 1]
      })
    } else {
      this.floatFactory.close()
    }
  }

  public async fetchHighlightGroups(): Promise<void> {
    let res = await this.nvim.call('coc#util#semantic_hlgroups') as string[]
    this.config.highlightGroups = res
  }

  public async getCurrentItem(): Promise<SemanticTokensBuffer | null> {
    let buf = await this.nvim.buffer
    let highlighter = this.highlighters.getItem(buf.id)
    if (!highlighter) null
    return highlighter
  }

  public getItem(bufnr: number): SemanticTokensBuffer | null {
    return this.highlighters.getItem(bufnr)
  }
  /**
   * Force highlight of current buffer
   */
  public async highlightCurrent(): Promise<void> {
    let item = await this.getCurrentItem()
    if (!item || !item.enabled) throw new Error(`Unable to perform semantic highlights for current buffer.`)
    await this.fetchHighlightGroups()
    await item.forceHighlight()
  }

  /**
   * Show semantic highlight info in temporarily buffer
   */
  public async showHighlightInfo(): Promise<void> {
    let buf = await this.nvim.buffer
    let { nvim } = this
    let item = this.highlighters.getItem(buf.id)
    if (!item) return nvim.echoError('Document not attached.')
    let hl = new Highlighter()
    nvim.pauseNotification()
    nvim.command(`vs +setl\\ buftype=nofile __coc_semantic_highlights_${buf.id}__`, true)
    nvim.command(`setl bufhidden=wipe noswapfile nobuflisted wrap undolevels=-1`, true)
    nvim.call('bufnr', ['%'], true)
    let res = await nvim.resumeNotification()
    hl.addLine('Semantic highlights info', headGroup)
    hl.addLine('')
    try {
      item.checkState()
      let highlights = item.highlights ?? []
      hl.addLine('The number of semantic tokens: ')
      hl.addText(String(highlights.length), 'Number')
      hl.addLine('')
      hl.addLine('Semantic highlight groups used by current buffer', headGroup)
      hl.addLine('')
      const groups = distinct(highlights.filter(o => o.hlGroup != null).map(({ hlGroup }) => hlGroup))
      for (const hlGroup of groups) {
        hl.addTexts([{ text: '-', hlGroup: 'Comment' }, { text: ' ' }, { text: hlGroup, hlGroup }])
      }
      hl.addLine('')
      hl.addLine('Tokens types that current Language Server supported:', headGroup)
      hl.addLine('')
      let doc = workspace.getDocument(item.bufnr)
      let legend = languages.getLegend(doc.textDocument) ?? languages.getLegend(doc.textDocument, true)
      if (legend.tokenTypes.length) {
        for (const t of [...new Set(legend.tokenTypes)]) {
          let text = HLGROUP_PREFIX + upperFirst(t)
          hl.addTexts([{ text: '-', hlGroup: 'Comment' }, { text: ' ' }, { text, hlGroup: text }])
        }
        hl.addLine('')
      } else {
        hl.addLine('No token types supported', 'Comment')
        hl.addLine('')
      }
      hl.addLine('Tokens modifiers that current Language Server supported:', headGroup)
      hl.addLine('')
      if (legend.tokenModifiers.length) {
        for (const t of [...new Set(legend.tokenModifiers)]) {
          let text = HLGROUP_PREFIX + upperFirst(t)
          hl.addTexts([{ text: '-', hlGroup: 'Comment' }, { text: ' ' }, { text, hlGroup: text }])
        }
        hl.addLine('')
      } else {
        hl.addLine('No token modifiers exist', 'Comment')
        hl.addLine('')
      }
    } catch (e) {
      hl.addLine(e instanceof Error ? e.message : e.toString(), 'Error')
    }
    nvim.pauseNotification()
    let bufnr = res[0][2] as number
    hl.render(nvim.createBuffer(bufnr))
    nvim.resumeNotification(true, true)
  }

  public dispose(): void {
    this.floatFactory.dispose()
    this.highlighters.dispose()
    disposeAll(this.disposables)
  }
}
