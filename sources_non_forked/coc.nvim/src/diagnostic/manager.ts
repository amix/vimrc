'use strict'
import { Neovim } from '@chemzqm/neovim'
import debounce from 'debounce'
import { Diagnostic, DiagnosticSeverity, DiagnosticTag, Disposable, Emitter, Event, Location, Position, Range } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import events from '../events'
import BufferSync from '../model/bufferSync'
import FloatFactory from '../model/floatFactory'
import { ConfigurationChangeEvent, Documentation, ErrorItem } from '../types'
import { disposeAll } from '../util'
import { readFileLines } from '../util/fs'
import { comparePosition, rangeIntersect } from '../util/position'
import { byteIndex } from '../util/string'
import window from '../window'
import workspace from '../workspace'
import { DiagnosticBuffer } from './buffer'
import DiagnosticCollection from './collection'
import { DiagnosticConfig, getSeverityName, severityLevel } from './util'
const logger = require('../util/logger')('diagnostic-manager')

export interface DiagnosticEventParams {
  bufnr: number
  uri: string
  diagnostics: ReadonlyArray<Diagnostic>
}

export interface DiagnosticItem {
  file: string
  lnum: number
  end_lnum: number
  col: number
  end_col: number
  source: string
  code: string | number
  message: string
  severity: string
  level: number
  location: Location
}

export class DiagnosticManager implements Disposable {
  public config: DiagnosticConfig
  private enabled = true
  private readonly _onDidRefresh = new Emitter<DiagnosticEventParams>()
  public readonly onDidRefresh: Event<DiagnosticEventParams> = this._onDidRefresh.event
  private buffers: BufferSync<DiagnosticBuffer>
  private floatFactory: FloatFactory
  private collections: DiagnosticCollection[] = []
  private disposables: Disposable[] = []
  private clearTimers: () => void | undefined

  public init(): void {
    this.setConfiguration()
    if (workspace.isNvim) {
      // setExtMark throws when namespace not created.
      this.nvim.createNamespace('coc-diagnostic-virtualText').then(id => {
        this.config.virtualTextSrcId = id
      }).logError()
    }
    workspace.onDidChangeConfiguration(this.setConfiguration, this, this.disposables)

    this.floatFactory = new FloatFactory(this.nvim)
    this.buffers = workspace.registerBufferSync(doc => {
      if (!this.enabled) return undefined
      let buf = new DiagnosticBuffer(
        this.nvim, doc, this.config,
        diagnostics => {
          this._onDidRefresh.fire({ diagnostics, uri: buf.uri, bufnr: buf.bufnr })
          if (buf.bufnr === workspace.bufnr && this.config.messageTarget === 'float') {
            void this.getCurrentDiagnostics().then(diagnostics => {
              return this.showFloat(diagnostics)
            })
          }
        })
      let diagnostics = this.getDiagnostics(doc.uri)
      // ignore empty diagnostics on first time.
      if (Object.keys(diagnostics).length > 0) {
        void buf.reset(diagnostics)
      }
      return buf
    })

    workspace.onDidCloseTextDocument(e => {
      for (let collection of this.collections) {
        collection.delete(e.uri)
      }
    }, null, this.disposables)

    let messageTimer: NodeJS.Timeout
    events.on('CursorMoved', bufnr => {
      if (this.config.enableMessage != 'always') return
      if (messageTimer) clearTimeout(messageTimer)
      messageTimer = setTimeout(async () => {
        let buf = this.buffers.getItem(bufnr)
        if (!buf || buf.dirty) return
        await this.echoMessage(true)
      }, this.config.messageDelay)
    }, null, this.disposables)

    let fn = debounce(async (bufnr, cursor) => {
      if (!this.config.virtualTextCurrentLineOnly) return
      if (events.insertMode && !this.config.refreshOnInsertMode) return
      let buf = this.buffers.getItem(bufnr)
      if (buf) {
        let enabled = await buf.isEnabled()
        if (enabled) buf.showVirtualText(cursor[0])
      }
    }, global.__TEST__ ? 10 : 100)
    this.disposables.push(Disposable.create(() => {
      fn.clear()
    }))
    events.on('CursorMoved', fn, null, this.disposables)
    events.on('InsertLeave', async () => {
      if (this.config.refreshOnInsertMode || !this.autoRefresh) return
      for (let buf of this.buffers.items) {
        if (buf.dirty) buf.refreshHighlights()
      }
    }, null, this.disposables)
    events.on('BufWinEnter', (bufnr: number) => {
      let buf = this.buffers.getItem(bufnr)
      if (buf && buf.dirty) buf.refreshHighlights()
    }, null, this.disposables)
    this.clearTimers = () => {
      if (messageTimer) clearTimeout(messageTimer)
      messageTimer = undefined
      fn.clear()
    }
    events.on('InsertEnter', this.clearTimers, this, this.disposables)
    let errorItems = workspace.configurations.errorItems
    this.setConfigurationErrors(errorItems)
    workspace.configurations.onError(items => {
      this.setConfigurationErrors(items)
    }, null, this.disposables)
  }

  private defineSigns(): void {
    let { nvim } = this
    let { enableHighlightLineNumber, enableSign } = this.config
    if (!enableSign) return
    nvim.pauseNotification()
    for (let kind of ['Error', 'Warning', 'Info', 'Hint']) {
      let signText = this.config[kind.toLowerCase() + 'Sign']
      let cmd = `sign define Coc${kind} linehl=Coc${kind}Line`
      if (signText) cmd += ` texthl=Coc${kind}Sign text=${signText}`
      if (enableHighlightLineNumber) cmd += ` numhl=Coc${kind}Sign`
      nvim.command(cmd, true)
    }
    nvim.resumeNotification(false, true)
  }

  /**
   * Fill location list with diagnostics
   */
  public async setLocationlist(bufnr: number): Promise<void> {
    if (!this.enabled) throw new Error(`Diagnostic not enabled.`)
    let buf = this.buffers.getItem(bufnr)
    if (!buf) throw new Error(`buffer ${bufnr} not attached.`)
    let diagnostics: Diagnostic[] = []
    for (let diags of Object.values(this.getDiagnostics(buf.uri))) {
      diagnostics.push(...diags)
    }
    let items = buf.toLocationListItems(diagnostics)
    let curr = await this.nvim.call('getloclist', [0, { title: 1 }]) as { title?: string }
    let action = curr.title && curr.title.indexOf('Diagnostics of coc') != -1 ? 'r' : ' '
    await this.nvim.call('setloclist', [0, [], action, { title: 'Diagnostics of coc', items }])
  }

  public setConfigurationErrors(errorItems?: ErrorItem[]): void {
    let collection = this.create('config')
    if (errorItems?.length) {
      let fsPath = URI.parse(errorItems[0].location.uri).fsPath
      void window.showErrorMessage(`Error detected for config file ${fsPath}, please check diagnostics list.`)
      let entries: Map<string, Diagnostic[]> = new Map()
      for (let item of errorItems) {
        let { uri } = item.location
        let diagnostics: Diagnostic[] = entries.get(uri) || []
        diagnostics.push(Diagnostic.create(item.location.range, item.message, DiagnosticSeverity.Error))
        entries.set(uri, diagnostics)
      }
      collection.set(Array.from(entries))
    } else {
      collection.clear()
    }
  }

  /**
   * Create collection by name
   */
  public create(name: string): DiagnosticCollection {
    let collection = this.getCollectionByName(name)
    if (collection) return collection
    collection = new DiagnosticCollection(name, () => {
      let idx = this.collections.findIndex(o => o == collection)
      if (idx !== -1) this.collections.splice(idx, 1)
    })
    this.collections.push(collection)
    collection.onDidDiagnosticsChange(uri => {
      let buf = this.buffers.getItem(uri)
      if (!this.autoRefresh || !buf) return
      void buf.update(name, this.getDiagnosticsByCollection(uri, collection))
    })
    return collection
  }

  /**
   * Get diagnostics ranges from document
   */
  public getSortedRanges(uri: string, severity?: string): Range[] {
    let collections = this.getCollections(uri)
    let res: Range[] = []
    let level = severity ? severityLevel(severity) : 0
    for (let collection of collections) {
      let diagnostics = collection.get(uri)
      if (level) {
        diagnostics = diagnostics.filter(o => o.severity == level)
      } else {
        let minLevel = this.config.level
        if (minLevel && minLevel < DiagnosticSeverity.Hint) {
          diagnostics = diagnostics.filter(o => {
            return o.severity && o.severity > minLevel ? false : true
          })
        }
      }
      let ranges = diagnostics.map(o => o.range)
      res.push(...ranges)
    }
    res.sort((a, b) => {
      if (a.start.line != b.start.line) {
        return a.start.line - b.start.line
      }
      return a.start.character - b.start.character
    })
    return res
  }

  /**
   * Get readonly diagnostics for a buffer
   */
  public getDiagnostics(uri: string): { [collection: string]: Diagnostic[] } {
    let res: { [collection: string]: Diagnostic[] } = {}
    let collections = this.getCollections(uri)
    for (let collection of collections) {
      if (!collection) continue
      res[collection.name] = this.getDiagnosticsByCollection(uri, collection)
    }
    return res
  }

  /**
   * Get filtered diagnostics by collection.
   */
  public getDiagnosticsByCollection(uri: string, collection: DiagnosticCollection): Diagnostic[] {
    let { level, showUnused, showDeprecated } = this.config
    let items = collection.get(uri) || []
    if (items.length) {
      items = items.filter(d => {
        if (level && d.severity && d.severity > level) {
          return false
        }
        if (!showUnused && d.tags?.includes(DiagnosticTag.Unnecessary)) {
          return false
        }
        if (!showDeprecated && d.tags?.includes(DiagnosticTag.Deprecated)) {
          return false
        }
        return true
      })
      items.sort((a, b) => {
        return comparePosition(a.range.start, b.range.start)
      })
    }
    return items
  }

  public getDiagnosticsInRange(document: TextDocument, range: Range): Diagnostic[] {
    let collections = this.getCollections(document.uri)
    let res: Diagnostic[] = []
    for (let collection of collections) {
      let items = collection.get(document.uri)
      if (!items) continue
      for (let item of items) {
        if (rangeIntersect(item.range, range)) {
          res.push(item)
        }
      }
    }
    return res
  }

  /**
   * Show diagnostics under curosr in preview window
   */
  public async preview(): Promise<void> {
    let diagnostics = await this.getCurrentDiagnostics()
    if (diagnostics.length == 0) {
      this.nvim.command('pclose', true)
      return
    }
    let lines: string[] = []
    for (let diagnostic of diagnostics) {
      let { source, code, severity, message } = diagnostic
      let s = getSeverityName(severity)[0]
      lines.push(`[${source}${code ? ' ' + code : ''}] [${s}]`)
      lines.push(...message.split(/\r?\n/))
      lines.push('')
    }
    this.nvim.call('coc#ui#preview_info', [lines, 'txt'], true)
  }

  /**
   * Jump to previous diagnostic position
   */
  public async jumpPrevious(severity?: string): Promise<void> {
    let buffer = await this.nvim.buffer
    let item = this.buffers.getItem(buffer.id)
    if (!item) return
    let curpos = await window.getCursorPosition()
    let ranges = this.getSortedRanges(item.uri, severity)
    let pos: Position
    for (let i = ranges.length - 1; i >= 0; i--) {
      let end = ranges[i].end
      if (comparePosition(end, curpos) < 0) {
        pos = ranges[i].start
        break
      } else if (i == 0) {
        let wrapscan = await this.nvim.getOption('wrapscan')
        if (wrapscan) pos = ranges[ranges.length - 1].start
      }
    }
    if (pos) {
      await window.moveTo(pos)
      if (this.config.enableMessage == 'never') return
      await this.echoMessage(false)
    }
  }

  /**
   * Jump to next diagnostic position
   */
  public async jumpNext(severity?: string): Promise<void> {
    let buffer = await this.nvim.buffer
    let item = this.buffers.getItem(buffer.id)
    if (!item) return
    let curpos = await window.getCursorPosition()
    let ranges = this.getSortedRanges(item.uri, severity)
    let pos: Position
    for (let i = 0; i <= ranges.length - 1; i++) {
      let start = ranges[i].start
      if (comparePosition(start, curpos) > 0) {
        pos = ranges[i].start
        break
      } else if (i == ranges.length - 1) {
        let wrapscan = await this.nvim.getOption('wrapscan')
        if (wrapscan) pos = ranges[0].start
      }
    }
    if (pos) {
      await window.moveTo(pos)
      if (this.config.enableMessage == 'never') return
      await this.echoMessage(false)
    }
  }

  /**
   * Get all sorted diagnostics
   */
  public async getDiagnosticList(): Promise<DiagnosticItem[]> {
    let res: DiagnosticItem[] = []
    const { level } = this.config
    for (let collection of this.collections) {
      for (let [uri, diagnostics] of collection.entries()) {
        if (diagnostics.length == 0) continue
        let u = URI.parse(uri)
        let doc = workspace.getDocument(uri)
        let lines = doc && doc.attached ? doc.textDocument.lines : undefined
        if (!lines && u.scheme === 'file') {
          try {
            const max = diagnostics.reduce((p, c) => {
              return Math.max(c.range.end.line, p)
            }, 0)
            lines = await readFileLines(u.fsPath, 0, max)
          } catch (e) {}
        }
        for (let diagnostic of diagnostics) {
          if (diagnostic.severity && diagnostic.severity > level) continue
          let { start, end } = diagnostic.range
          let o: DiagnosticItem = {
            file: u.fsPath,
            lnum: start.line + 1,
            end_lnum: end.line + 1,
            col: Array.isArray(lines) ? byteIndex(lines[start.line] ?? '', start.character) + 1 : start.character + 1,
            end_col: Array.isArray(lines) ? byteIndex(lines[end.line] ?? '', end.character) + 1 : end.character + 1,
            code: diagnostic.code,
            source: diagnostic.source || collection.name,
            message: diagnostic.message,
            severity: getSeverityName(diagnostic.severity),
            level: diagnostic.severity || 0,
            location: Location.create(uri, diagnostic.range)
          }
          res.push(o)
        }
      }
    }
    res.sort((a, b) => {
      if (a.level !== b.level) {
        return a.level - b.level
      }
      if (a.file !== b.file) {
        return a.file > b.file ? 1 : -1
      } else {
        if (a.lnum != b.lnum) {
          return a.lnum - b.lnum
        }
        return a.col - b.col
      }
    })
    return res
  }

  private getDiagnosticsAt(bufnr: number, cursor: [number, number], atEnd = false, lastline = false): Diagnostic[] {
    let buffer = this.buffers.getItem(bufnr)
    if (!buffer) return []
    let pos = Position.create(cursor[0], cursor[1])
    let res = buffer.getDiagnosticsAt(pos, this.config.checkCurrentLine)
    if (this.config.checkCurrentLine || res.length) return res
    // check next character when cursor at end of line.
    if (atEnd) {
      pos = Position.create(cursor[0], cursor[1] + 1)
      res = buffer.getDiagnosticsAt(pos, false)
      if (res.length) return res
    }
    // check next line when cursor at the beginning of last line.
    if (lastline && cursor[1] == 0) {
      pos = Position.create(cursor[0] + 1, 0)
      res = buffer.getDiagnosticsAt(pos, false)
    }
    return res
  }

  public async getCurrentDiagnostics(): Promise<Diagnostic[]> {
    let [bufnr, cursor, eol, lastline] = await this.nvim.eval(`[bufnr("%"),coc#cursor#position(),col('.')==col('$')-1,line('.')==line('$')]`) as [number, [number, number], number, number]
    return this.getDiagnosticsAt(bufnr, cursor, eol == 1, lastline == 1)
  }

  /**
   * Echo diagnostic message under cursor.
   */
  public async echoMessage(truncate = false): Promise<void> {
    const config = this.config
    if (!this.enabled || config.displayByAle) return
    let useFloat = config.messageTarget == 'float'
    let diagnostics = await this.getCurrentDiagnostics()
    if (config.messageLevel) {
      diagnostics = diagnostics.filter(diagnostic => {
        return diagnostic.severity && diagnostic.severity <= config.messageLevel
      })
    }
    if (useFloat) {
      await this.showFloat(diagnostics)
    } else {
      if (truncate && events.insertMode) return
      const lines = []
      diagnostics.forEach(diagnostic => {
        let { source, code, severity, message } = diagnostic
        let s = getSeverityName(severity)[0]
        const codeStr = code ? ' ' + code : ''
        const str = config.format.replace('%source', source).replace('%code', codeStr).replace('%severity', s).split('%message').join(message)
        lines.push(str)
      })
      if (lines.length) {
        await this.nvim.command('echo ""')
        await window.echoLines(lines, truncate)
      }
    }
  }

  private async showFloat(diagnostics: Diagnostic[]): Promise<void> {
    if (this.config.messageTarget !== 'float') return
    let { config } = this
    if (diagnostics.length == 0) {
      this.floatFactory.close()
      return
    }
    if (events.insertMode) return
    let ft = ''
    let docs: Documentation[] = []
    if (Object.keys(config.filetypeMap).length > 0) {
      let doc = workspace.getDocument(workspace.bufnr)
      let filetype = doc ? doc.filetype : ''
      const defaultFiletype = config.filetypeMap['default'] || ''
      ft = config.filetypeMap[filetype] || (defaultFiletype == 'bufferType' ? filetype : defaultFiletype)
    }
    diagnostics.forEach(diagnostic => {
      let { source, code, severity, message } = diagnostic
      let s = getSeverityName(severity)[0]
      const codeStr = code ? ' ' + code : ''
      const str = config.format.replace('%source', source).replace('%code', codeStr).replace('%severity', s).split('%message').join(message)
      let filetype = 'Error'
      if (ft === '') {
        switch (severity) {
          case DiagnosticSeverity.Hint:
            filetype = 'Hint'
            break
          case DiagnosticSeverity.Warning:
            filetype = 'Warning'
            break
          case DiagnosticSeverity.Information:
            filetype = 'Info'
            break
        }
      } else {
        filetype = ft
      }
      docs.push({ filetype, content: str })
      if (diagnostic.codeDescription?.href) {
        docs.push({ filetype: 'txt', content: diagnostic.codeDescription.href })
      }
    })
    let floatConfig = this.floatFactory.applyFloatConfig({ modes: ['n'], maxWidth: 80 }, this.config.floatConfig)
    logger.debug('floatConfig:', floatConfig)
    await this.floatFactory.show(docs, floatConfig)
  }

  public async jumpRelated(): Promise<void> {
    let diagnostics = await this.getCurrentDiagnostics()
    let diagnostic = diagnostics.find(o => o.relatedInformation != null)
    let locations = diagnostic ? diagnostic.relatedInformation.map(o => o.location) : []
    if (locations.length == 1) {
      await workspace.jumpTo(locations[0].uri, locations[0].range.start)
    } else if (locations.length > 1) {
      await workspace.showLocations(locations)
    } else {
      void window.showWarningMessage('No related information found.')
    }
  }

  public reset(): void {
    if (this.clearTimers) {
      this.clearTimers()
    }
    this.buffers.reset()
    for (let collection of this.collections) {
      collection.dispose()
    }
    this.collections = []
  }

  public dispose(): void {
    if (this.clearTimers) {
      this.clearTimers()
    }
    this.buffers.dispose()
    for (let collection of this.collections) {
      collection.dispose()
    }
    this.floatFactory?.close()
    this.collections = []
    disposeAll(this.disposables)
  }

  private get nvim(): Neovim {
    return workspace.nvim
  }

  private setConfiguration(event?: ConfigurationChangeEvent): void {
    if (event && !event.affectsConfiguration('diagnostic')) return
    let config = workspace.getConfiguration('diagnostic')
    let messageTarget = config.get<string>('messageTarget', 'float')
    if (messageTarget == 'float' && !workspace.env.floating && !workspace.env.textprop) {
      messageTarget = 'echo'
    }
    let enableHighlightLineNumber = config.get<boolean>('enableHighlightLineNumber', true)
    if (!workspace.isNvim) enableHighlightLineNumber = false
    this.config = Object.assign(this.config || {}, {
      floatConfig: config.get('floatConfig', {}),
      messageTarget,
      enableHighlightLineNumber,
      highlighLimit: config.get<number>('highlighLimit', 1000),
      highlightPriority: config.get<number>('highlightPriority'),
      autoRefresh: config.get<boolean>('autoRefresh', true),
      checkCurrentLine: config.get<boolean>('checkCurrentLine', false),
      enableSign: workspace.env.sign && config.get<boolean>('enableSign', true),
      locationlistUpdate: config.get<boolean>('locationlistUpdate', true),
      enableMessage: config.get<string>('enableMessage', 'always'),
      messageDelay: config.get<number>('messageDelay', 200),
      virtualText: config.get<boolean>('virtualText', false) && this.nvim.hasFunction('nvim_buf_set_virtual_text'),
      virtualTextAlignRight: workspace.has('nvim-0.5.1') && config.get<boolean>('virtualTextAlignRight', false),
      virtualTextWinCol: workspace.has('nvim-0.5.1') ? config.get<number | null>('virtualTextWinCol', null) : null,
      virtualTextCurrentLineOnly: config.get<boolean>('virtualTextCurrentLineOnly', true),
      virtualTextPrefix: config.get<string>('virtualTextPrefix', " "),
      virtualTextLineSeparator: config.get<string>('virtualTextLineSeparator', " \\ "),
      virtualTextLines: config.get<number>('virtualTextLines', 3),
      displayByAle: config.get<boolean>('displayByAle', false),
      level: severityLevel(config.get<string>('level', 'hint')),
      locationlistLevel: severityLevel(config.get<string>('locationlistLevel')),
      signLevel: severityLevel(config.get<string>('signLevel')),
      virtualTextLevel: severityLevel(config.get<string>('virtualTextLevel')),
      messageLevel: severityLevel(config.get<string>('messageLevel')),
      signPriority: config.get<number>('signPriority', 10),
      errorSign: config.get<string>('errorSign', '>>'),
      warningSign: config.get<string>('warningSign', '>>'),
      infoSign: config.get<string>('infoSign', '>>'),
      hintSign: config.get<string>('hintSign', '>>'),
      refreshOnInsertMode: config.get<boolean>('refreshOnInsertMode', false),
      filetypeMap: config.get<object>('filetypeMap', {}),
      showUnused: config.get<boolean>('showUnused', true),
      showDeprecated: config.get<boolean>('showDeprecated', true),
      format: config.get<string>('format', '[%source%code] [%severity] %message'),
    })
    this.enabled = config.get<boolean>('enable', true)
    this.defineSigns()
  }

  public getCollectionByName(name: string): DiagnosticCollection {
    return this.collections.find(o => o.name == name)
  }

  private getCollections(uri: string): DiagnosticCollection[] {
    return this.collections.filter(c => c.has(uri))
  }

  public toggleDiagnostic(enable?: number): void {
    let enabled = enable == undefined ? this.enabled : enable == 0
    this.enabled = !enabled
    for (let buf of this.buffers.items) {
      if (this.enabled) {
        void this.refreshBuffer(buf.uri)
      } else {
        buf.clear()
      }
    }
  }

  public async toggleDiagnosticBuffer(bufnr?: number, enable?: number): Promise<void> {
    if (!this.enabled) return
    bufnr = bufnr || workspace.bufnr
    let buf = this.buffers.getItem(bufnr)
    if (buf) {
      let isEnabled = enable == undefined ? await buf.isEnabled() : enable == 0
      await this.nvim.call('setbufvar', [bufnr, 'coc_diagnostic_disable', isEnabled ? 1 : 0])
      if (isEnabled) {
        buf.clear()
      } else {
        void this.refreshBuffer(bufnr)
      }
    }
  }

  private get autoRefresh(): boolean {
    return this.enabled && this.config.autoRefresh
  }

  /**
   * Refresh diagnostics by uri or bufnr
   */
  public async refreshBuffer(uri: string | number, force?: boolean): Promise<boolean> {
    let buf = this.buffers.getItem(uri)
    if (!buf) return false
    await buf.reset(this.getDiagnostics(buf.uri), force)
    return true
  }

  /**
   * Force diagnostics refresh.
   */
  public refresh(bufnr?: number): void {
    let items: Iterable<DiagnosticBuffer>
    if (!bufnr) {
      items = this.buffers.items
    } else {
      let item = this.buffers.getItem(bufnr)
      items = item ? [item] : []
    }
    for (let item of items) {
      void this.refreshBuffer(item.uri, true)
    }
  }
}

export default new DiagnosticManager()
