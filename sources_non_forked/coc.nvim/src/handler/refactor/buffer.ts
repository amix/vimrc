'use strict'
import { Buffer, Neovim } from '@chemzqm/neovim'
import fastDiff from 'fast-diff'
import path from 'path'
import { Disposable, Position, Range, TextEdit } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { URI } from 'vscode-uri'
import Document from '../../model/document'
import Highlighter from '../../model/highligher'
import { BufferSyncItem, DidChangeTextDocumentParams, Optional, TextDocumentContentChange } from '../../types'
import { disposeAll } from '../../util'
import { isParentFolder, readFileLines, sameFile } from '../../util/fs'
import { omit } from '../../util/lodash'
import { Mutex } from '../../util/mutex'
import { equals } from '../../util/object'
import { adjustRangePosition, emptyRange } from '../../util/position'
import { byteLength } from '../../util/string'
import { getChangedLineCount, lineCountChange } from '../../util/textedit'
import window from '../../window'
import workspace from '../../workspace'
import Changes, { LineInfo } from './changes'
const logger = require('../../util/logger')('handler-refactorBuffer')

export const SEPARATOR = '\u3000'

export interface LineChange {
  // zero indexed
  lnum: number
  delta: number
}

export interface FileRangeInfo {
  // line number of filepath
  lnum: number
  filepath: string
  lines: string[]
}

export interface FileChange extends FileRangeInfo {
  // start line 0 indexed
  start: number
  // end line 0 indexed, excluded
  end: number
}

export interface FileRange {
  // start lnum in refactor buffer, 1 indexed
  lnum: number
  // start line 0 indexed
  start: number
  lines: string[]
  // range relatived to new range
  highlights?: Range[]
}

export type FileRangeDef = Optional<FileRange, 'lines' | 'lnum'> & { end?: number }

export interface FileItem {
  filepath: string
  ranges: FileRange[]
}

export interface FileItemDef {
  filepath: string
  ranges: FileRangeDef[]
}

export interface RefactorConfig {
  openCommand: string
  beforeContext: number
  afterContext: number
  saveToFile: boolean
  showMenu: string
}

export interface RefactorBufferOpts {
  cwd: string
  winid: number
  fromWinid: number
}

export default class RefactorBuffer implements BufferSyncItem {
  private _disposed = false
  private _fileItems: FileItem[] = []
  private mutex = new Mutex()
  private disposables: Disposable[] = []
  private matchIds: Set<number> = new Set()
  private changes: Changes
  private changing = false
  constructor(
    public readonly bufnr: number,
    private srcId: number,
    private nvim: Neovim,
    public readonly config: RefactorConfig,
    private opts: RefactorBufferOpts
  ) {
    this.changes = new Changes()
    this.disposables.push(workspace.registerLocalKeymap('n', '<CR>', this.splitOpen.bind(this), true))
    if (config.showMenu) {
      this.disposables.push(workspace.registerLocalKeymap('n', config.showMenu, this.showMenu.bind(this), true))
    }
    workspace.onDidChangeTextDocument(this.onDocumentChange, this, this.disposables)
  }

  public async showMenu(): Promise<void> {
    let res = await window.showMenuPicker(['Tab open', 'Remove block'])
    if (res == -1) return
    let fileRange = await this.searchCurrentRange()
    if (!fileRange) return
    if (res == 0) {
      let before = await this.nvim.eval(`strpart(getline('.'), 0 ,col('.') - 1)`) as string
      let character = before.length
      let bufname = this.getAbsolutePath(fileRange.filepath)
      this.nvim.call('coc#util#jump', ['tabe', bufname, [fileRange.line, character]], true)
    }
    if (res == 1) {
      let range = this.getDeleteRange(fileRange)
      await this.document.applyEdits([TextEdit.del(range)])
    }
  }

  public get fileItems(): FileItem[] {
    return this._fileItems
  }

  public getFileItem(uri: string): FileItem | undefined {
    let filepath = URI.parse(uri).fsPath
    return this._fileItems.find(o => sameFile(o.filepath, filepath))
  }

  public getFileRange(lnum: number): FileRange & { filepath: string } {
    for (let item of this._fileItems) {
      for (let r of item.ranges) {
        if (r.lnum == lnum) {
          return Object.assign(omit(r, ['highlights']), { filepath: item.filepath })
        }
      }
    }
    throw new Error(`File range not found at lnum: ${lnum}`)
  }

  public onChange(e: DidChangeTextDocumentParams): void {
    if (this.changing) return
    if (e.contentChanges.length === 0) {
      this.highlightLineNr()
      this.nvim.redrawVim()
      return
    }
    let { nvim } = this
    e = fixChangeParams(e)
    let change = e.contentChanges[0]
    let { original } = e
    if (change.range.end.line > 2) {
      nvim.call('setbufvar', [e.bufnr, '&modified', 1], true)
    }
    let { range, text } = change
    let lineChange = lineCountChange(TextEdit.replace(range, text))
    if (lineChange == 0) return
    let edits: TextEdit[] = [TextEdit.replace(range, text)]
    let addRanges: LineInfo[] = []
    // Check removed ranges
    if (!emptyRange(range) && !text.includes('\u3000')) {
      let sl = range.start.line
      let lnums: number[] = []
      let lines = original.split(/\r?\n/)
      for (let i = 0; i < lines.length; i++) {
        let line = lines[i]
        if (line.length > 1 && line.includes('\u3000')) {
          lnums.push(sl + i + 1)
        }
      }
      if (lnums.length) {
        let infos: LineInfo[] = lnums.map(lnum => {
          return this.getFileRange(lnum)
        })
        for (let item of this._fileItems) {
          item.ranges = item.ranges.filter(o => !lnums.includes(o.lnum))
        }
        this.changes.add(infos)
      }
    } else if (emptyRange(range) && text.includes('\u3000')) {
      // check undo
      let lines = text.split(/\r?\n/)
      let lnums: number[] = []
      let sl = range.start.line
      for (let i = 0; i < lines.length; i++) {
        let line = lines[i]
        if (line.length > 1 && line.includes('\u3000')) {
          lnums.push(sl + i + 1)
        }
      }
      if (lnums.length) {
        let res = this.changes.checkInsert(lnums)
        if (res) addRanges = res
      }
    } else if (text.includes('\u3000')) {
      // check multiple ranges change
      edits = this.diffChanges(original, text)
      edits.forEach(e => {
        e.range = adjustRangePosition(e.range, range.start)
      })
    }
    this.adjustLnums(edits)
    nvim.pauseNotification()
    this.highlightLineNr()
    nvim.resumeNotification(true, true)
    if (addRanges.length) {
      addRanges.forEach(info => {
        let item = this._fileItems.find(o => o.filepath == info.filepath)
        item.ranges.push(info)
      })
    }
  }

  private diffChanges(original: string, text: string): TextEdit[] {
    let edits: TextEdit[] = []
    let diffs = fastDiff(original, text)
    let offset = 0
    let orig = TextDocument.create('file:///1', '', 0, original)
    for (let i = 0; i < diffs.length; i++) {
      let diff = diffs[i]
      let pos = orig.positionAt(offset)
      if (diff[0] == fastDiff.EQUAL) {
        offset = offset + diff[1].length
      } else if (diff[0] == fastDiff.DELETE) {
        let end = orig.positionAt(offset + diff[1].length)
        if (diffs[i + 1] && diffs[i + 1][0] == fastDiff.INSERT) {
          let text = diffs[i + 1][1]
          edits.push(TextEdit.replace(Range.create(pos, end), text))
          i = i + 1
        } else {
          edits.push(TextEdit.replace(Range.create(pos, end), ''))
        }
        offset = offset + diff[1].length
      } else if (diff[0] == fastDiff.INSERT) {
        edits.push(TextEdit.insert(pos, diff[1]))
      }
    }
    return edits
  }

  /**
   * Handle changes of other buffers.
   */
  private async onDocumentChange(e: DidChangeTextDocumentParams): Promise<void> {
    if (this.changing || e.contentChanges.length === 0) return
    let { uri } = e.textDocument
    let fileItem = this.getFileItem(uri)
    // not affected
    if (!fileItem) return
    let { range, text } = e.contentChanges[0]
    let lineChange = lineCountChange(TextEdit.replace(range, text))
    let edits: TextEdit[] = []
    let deleteIndexes: number[] = []
    // 4 cases: ignore, change lineNr, reload, remove
    for (let i = 0; i < fileItem.ranges.length; i++) {
      let r = fileItem.ranges[i]
      // change after range
      if (range.start.line >= r.start + r.lines.length) continue
      // change before range
      if (range.end.line < r.start) {
        r.start = r.start + lineChange
        continue
      }
      let textDocument = workspace.getDocument(uri).textDocument
      let end = r.start + r.lines.length + lineChange
      let newLines = textDocument.lines.slice(r.start, end)
      if (!newLines.length) {
        deleteIndexes.push(i)
        let replaceRange = this.getDeleteRange(r)
        edits.push(TextEdit.replace(replaceRange, ''))
      } else {
        r.lines = newLines
        let replaceRange = this.getReplaceRange(r)
        edits.push(TextEdit.replace(replaceRange, newLines.join('\n')))
      }
    }
    if (deleteIndexes.length) {
      fileItem.ranges = fileItem.ranges.filter((_, i) => !deleteIndexes.includes(i))
    }
    // clean fileItem with empty ranges
    this._fileItems = this._fileItems.filter(o => o.ranges && o.ranges.length > 0)
    if (edits.length) {
      this.adjustLnums(edits)
      this.changing = true
      await this.document.applyEdits(edits)
      this.changing = false
    }
    this.nvim.pauseNotification()
    this.highlightLineNr()
    this.buffer.setOption('modified', false, true)
    await this.nvim.resumeNotification(true)
  }

  private adjustLnums(edits: TextEdit[]): void {
    for (let item of this._fileItems) {
      for (let fileRange of item.ranges) {
        let line = fileRange.lnum - 1
        fileRange.lnum += getChangedLineCount(Position.create(line, 0), edits)
      }
    }
  }

  /**
   * Current changed file ranges
   */
  public async getFileChanges(): Promise<FileRangeInfo[]> {
    let changes: FileRangeInfo[] = []
    let lines = await this.buffer.lines
    lines.push(SEPARATOR)
    // current lines
    let arr: string[] = []
    let fsPath: string
    let lnum: number
    for (let i = 0; i < lines.length; i++) {
      let line = lines[i]
      if (line.startsWith(SEPARATOR)) {
        if (fsPath) {
          changes.push({
            filepath: fsPath,
            lines: arr.slice(),
            lnum
          })
          fsPath = undefined
          arr = []
        }
        if (line.length > 1) {
          let ms = line.match(/^\u3000(.*)/)
          if (ms) {
            fsPath = this.getAbsolutePath(ms[1].replace(/\s+$/, ''))
            lnum = i + 1
            arr = []
          }
        }
      } else {
        arr.push(line)
      }
    }
    return changes
  }

  /**
   * Open line under cursor in split window
   */
  public async splitOpen(): Promise<void> {
    let { nvim } = this
    let win = nvim.createWindow(this.opts.fromWinid)
    let valid = await win.valid
    let before = await nvim.eval(`strpart(getline('.'), 0 ,col('.') - 1)`) as string
    let character = before.length
    let fileRange = await this.searchCurrentRange()
    if (fileRange) {
      let bufname = this.getAbsolutePath(fileRange.filepath)
      nvim.pauseNotification()
      if (valid) {
        nvim.call('win_gotoid', [this.opts.fromWinid], true)
        this.nvim.call('coc#util#jump', ['edit', bufname, [fileRange.line, character]], true)
      } else {
        this.nvim.call('coc#util#jump', ['belowright vs', bufname, [fileRange.line, character]], true)
      }
      nvim.command('normal! zz', true)
      await nvim.resumeNotification(true)
      if (!valid) {
        this.opts.fromWinid = await nvim.call('win_getid')
      }
    }
  }

  public async searchCurrentRange(): Promise<FileRange & { filepath: string, line: number }> {
    let { nvim } = this
    let lines = await nvim.eval('getline(1,line("."))') as string[]
    let len = lines.length
    for (let i = 0; i < len; i++) {
      let line = lines[len - i - 1]
      let ms = line.match(/^\u3000(.+)/)
      if (ms) {
        let r = this.getFileRange(len - i)
        return Object.assign({ line: r.start + (i == 0 ? 1 : i) - 1 }, r)
      }
    }
    return undefined
  }

  /**
   * Add FileItem to refactor buffer.
   */
  public async addFileItems(items: FileItemDef[]): Promise<void> {
    if (this._disposed) return
    let { cwd } = this.opts
    let { document } = this
    const release = await this.mutex.acquire()
    try {
      await document.synchronize()
      let count = document.lineCount
      let highligher = new Highlighter()
      let hlRanges: Range[] = []
      for (let item of items) {
        let ranges: FileRange[] = []
        for (let range of item.ranges) {
          highligher.addLine(SEPARATOR)
          highligher.addLine(SEPARATOR)
          let lnum = count + highligher.length
          highligher.addText(`${isParentFolder(cwd, item.filepath) ? path.relative(cwd, item.filepath) : item.filepath}`)
          // white spaces for conceal texts
          let n = String(range.start + 1).length + String(range.end).length + 4
          if (!this.srcId) highligher.addText(' '.repeat(n))
          let base = 0 - highligher.length - count
          if (range.highlights) {
            hlRanges.push(...range.highlights.map(r => adjustRange(r, base)))
          }
          let { lines, start, end, highlights } = range
          if (!lines) {
            lines = await this.getLines(item.filepath, start, end)
          }
          ranges.push({ lines, lnum, start, highlights })
          highligher.addLines(lines)
        }
        if (ranges.length) {
          let newItem: FileItem = { filepath: item.filepath, ranges }
          let fileItem = this._fileItems.find(o => o.filepath == item.filepath)
          if (fileItem) {
            fileItem.ranges.push(...newItem.ranges)
          } else {
            this._fileItems.push(newItem)
          }
        }
      }
      let { nvim, buffer } = this
      this.changing = true
      nvim.pauseNotification()
      highligher.render(buffer, count)
      this.highlightLineNr()
      buffer.setOption('modified', false, true)
      buffer.setOption('undolevels', 1000, true)
      if (count == 2 && hlRanges.length) {
        let pos = hlRanges[0].start
        nvim.call('coc#cursor#move_to', [pos.line, pos.character], true)
      }
      await nvim.resumeNotification(true)
      await document.patchChange()
      this.changing = false
      await window.cursors.addRanges(hlRanges)
    } catch (e) {
      this.changing = false
      logger.error(`Error on add file item:`, e)
    }
    release()
  }

  public findRange(filepath: string, lnum: number): FileRange {
    let item = this.fileItems.find(o => sameFile(this.getAbsolutePath(o.filepath), filepath))
    let range = item.ranges.find(o => o.lnum == lnum)
    if (!range) throw new Error(`File range not found at lnum: ${lnum}`)
    return range
  }

  /**
   * Save changes to buffers/files, return false when no change made.
   */
  public async save(): Promise<boolean> {
    let { nvim } = this
    let doc = this.document
    let { buffer } = doc
    await doc.patchChange()
    let changes = await this.getFileChanges()
    if (!changes) return
    changes.sort((a, b) => a.lnum - b.lnum)
    // filter changes that not change
    let fileChanges: FileChange[] = []
    for (let i = 0; i < changes.length; i++) {
      let change = changes[i]
      let range = this.findRange(change.filepath, change.lnum)
      if (equals(range.lines, change.lines)) continue
      fileChanges.push(Object.assign({ start: range.start, end: range.start + range.lines.length }, change))
      range.lines = change.lines
    }
    if (fileChanges.length == 0) {
      await window.showInformationMessage('No change.')
      await buffer.setOption('modified', false)
      return false
    }
    let changeMap: { [uri: string]: TextEdit[] } = {}
    for (let change of fileChanges) {
      let uri = URI.file(change.filepath).toString()
      let edits = changeMap[uri] || []
      edits.push({
        range: Range.create(change.start, 0, change.end, 0),
        newText: change.lines.join('\n') + '\n'
      })
      changeMap[uri] = edits
    }
    this.changing = true
    await workspace.applyEdit({ changes: changeMap })
    this.changing = false
    for (let item of this.fileItems) {
      let uri = URI.file(this.getAbsolutePath(item.filepath)).toString()
      let edits = changeMap[uri]
      if (edits && edits.length > 0) {
        item.ranges.forEach(r => {
          r.start += getChangedLineCount(Position.create(r.start, 0), edits)
        })
      }
    }
    nvim.pauseNotification()
    buffer.setOption('modified', false, true)
    if (this.config.saveToFile) {
      nvim.command('silent noa wa', true)
    }
    this.highlightLineNr()
    await nvim.resumeNotification()
    return true
  }

  private async getLines(fsPath: string, start: number, end: number): Promise<string[]> {
    let uri = URI.file(fsPath).toString()
    let doc = workspace.getDocument(uri)
    if (doc) return doc.getLines(start, end)
    return await readFileLines(fsPath, start, end - 1)
  }

  private getAbsolutePath(filepath: string): string {
    if (path.isAbsolute(filepath)) return filepath
    return path.join(this.opts.cwd, filepath)
  }

  /**
   * Use conceal/virtual text to add lineNr
   */
  private highlightLineNr(): void {
    let { fileItems, nvim, srcId, bufnr } = this
    let { winid, cwd } = this.opts
    let info = {}
    if (srcId) {
      nvim.call('nvim_buf_clear_namespace', [bufnr, srcId, 0, -1], true)
      for (let item of fileItems) {
        for (let range of item.ranges) {
          let end = range.start + range.lines.length
          let text = `${range.start + 1}:${end}`
          info[range.lnum] = [range.start + 1, end]
          nvim.call('nvim_buf_set_virtual_text', [bufnr, srcId, range.lnum - 1, [[text, 'LineNr']], {}], true)
        }
      }
    } else {
      if (this.matchIds.size) {
        nvim.call('coc#highlight#clear_matches', [winid, Array.from(this.matchIds)], true)
        this.matchIds.clear()
      }
      let id = 2000
      for (let item of fileItems) {
        let filename = `${cwd ? path.relative(cwd, item.filepath) : item.filepath}`
        let col = byteLength(filename) + 1
        for (let range of item.ranges) {
          let end = range.start + range.lines.length
          let text = `:${range.start + 1}:${end}`
          for (let i = 0; i < text.length; i++) {
            let ch = text[i]
            this.matchIds.add(id)
            info[range.lnum] = [range.start + 1, end]
            nvim.call('matchaddpos', ['Conceal', [[range.lnum, col + i]], 99, id, { conceal: ch, window: winid }], true)
            id++
          }
        }
      }
    }
    this.buffer.setVar('line_infos', info, true)
  }

  public getDeleteRange(r: FileRange): Range {
    let { document } = this
    let start = r.lnum - 1
    let end: Position
    let total = document.lineCount
    for (let i = start; i < total; i++) {
      if (i + 1 == total) {
        end = Position.create(total, 0)
        break
      }
      let line = document.getline(i)
      if (line === SEPARATOR) {
        end = Position.create(i + 1, 0)
        break
      }
      if (i != start && line.startsWith(SEPARATOR)) {
        end = Position.create(i, 0)
        break
      }
    }
    return Range.create(Position.create(start, 0), end)
  }

  public getReplaceRange(r: FileRange): Range {
    let { document } = this
    let start = r.lnum
    let end: Position
    let total = document.lineCount
    for (let i = start; i < total; i++) {
      let line = document.getline(i)
      if (i + 1 == total) {
        end = Position.create(i, line.length)
        break
      }
      let next = document.getline(i + 1)
      if (next.startsWith('\u3000')) {
        end = Position.create(i, line.length)
        break
      }
    }
    return Range.create(Position.create(start, 0), end)
  }

  public get valid(): Promise<boolean> {
    return this.buffer.valid
  }

  public get buffer(): Buffer {
    return this.nvim.createBuffer(this.bufnr)
  }

  public get document(): Document | null {
    return workspace.getDocument(this.bufnr)
  }

  public dispose(): void {
    this._disposed = true
    disposeAll(this.disposables)
  }
}

function adjustRange(range: Range, offset: number): Range {
  let { start, end } = range
  return Range.create(start.line - offset, start.character, end.line - offset, end.character)
}

export function fixChangeParams(e: DidChangeTextDocumentParams): DidChangeTextDocumentParams {
  let { contentChanges, bufnr, textDocument, original, originalLines } = e
  let { range, text } = contentChanges[0]
  let changes: TextDocumentContentChange[] = [{ range, text }]
  if (!original) {
    if (emptyRange(range) && range.start.character != 0) {
      let lines = text.split(/\r?\n/)
      let last = lines[lines.length - 1]
      let before = originalLines[range.start.line].slice(0, range.start.character)
      if (last.startsWith(SEPARATOR) && before == last) {
        changes[0].text = before + lines.slice(0, -1).join('\n') + '\n'
        let { start, end } = range
        changes[0].range = Range.create(start.line, 0, end.line, 0)
      }
    }
  } else {
    let lines = original.split(/\r?\n/)
    let last = lines[lines.length - 1]
    if (last.startsWith(SEPARATOR)) {
      let before = originalLines[range.start.line].slice(0, range.start.character)
      if (before == last) {
        original = before + lines.slice(0, -1).join('\n') + '\n'
        let { start, end } = range
        changes[0].range = Range.create(start.line, 0, end.line, 0)
      }
    }
    let prev = originalLines[range.start.line - 1]
    let nest = lines.length > 1 ? lines[lines.length - 2] : ''
    if (last == '' &&
      nest.startsWith(SEPARATOR) &&
      prev == nest &&
      range.start.character == 0 && range.end.character == 0) {
      original = prev + '\n' + lines.slice(0, -2).join('\n') + '\n'
      let { start, end } = range
      changes[0].range = Range.create(start.line - 1, 0, end.line - 1, 0)
    }
  }
  return { contentChanges: changes, bufnr, textDocument, original, originalLines }
}
