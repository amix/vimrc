'use strict'
import { Neovim } from '@chemzqm/neovim'
import fastDiff from 'fast-diff'
import path from 'path'
import { Disposable } from 'vscode-languageserver-protocol'
import { ChangeAnnotation, CreateFile, DeleteFile, Position, RenameFile, TextDocumentEdit, WorkspaceEdit } from 'vscode-languageserver-types'
import { URI } from 'vscode-uri'
import type Keymaps from '../core/keymaps'
import events from '../events'
import { DocumentChange, LinesChange } from '../types'
import { disposeAll } from '../util'
import { isParentFolder } from '../util/fs'
import { getAnnotationKey, getPositionFromEdits, mergeSort } from '../util/textedit'
import Highlighter from './highligher'
const logger = require('../util/logger')('mdoe-editInspect')

export type RecoverFunc = () => Promise<any>

export interface EditState {
  edit: WorkspaceEdit
  changes: {
    [uri: string]: LinesChange
  }
  recovers: RecoverFunc[]
  applied: boolean
}

export interface ChangedFileItem {
  index: number
  filepath: string
  lnum?: number
}

let global_id = 0

export default class EditInspect {
  private disposables: Disposable[] = []
  private bufnr: number
  private items: ChangedFileItem[] = []
  private renameMap: Map<string, string> = new Map()
  constructor(private nvim: Neovim, private keymaps: Keymaps) {
    events.on('BufUnload', bufnr => {
      if (bufnr == this.bufnr) this.dispose()
    }, null, this.disposables)
  }

  private addFile(filepath: string, highligher: Highlighter, lnum?: number): void {
    this.items.push({
      index: highligher.length,
      filepath,
      lnum
    })
  }

  public async show(state: EditState): Promise<void> {
    let { nvim } = this
    let id = global_id++
    nvim.pauseNotification()
    nvim.command(`tabe +setl\\ buftype=nofile CocWorkspaceEdit${id}`, true)
    nvim.command(`setl bufhidden=wipe nolist`, true)
    nvim.command('setl nobuflisted wrap undolevels=-1 filetype=cocedits noswapfile', true)
    await nvim.resumeNotification(true)
    let buffer = await nvim.buffer
    let cwd = await nvim.call('getcwd')
    this.bufnr = buffer.id
    const relpath = (uri: string): string => {
      let fsPath = URI.parse(uri).fsPath
      return isParentFolder(cwd, fsPath, true) ? path.relative(cwd, fsPath) : fsPath
    }
    const absPath = filepath => {
      return path.isAbsolute(filepath) ? filepath : path.join(cwd, filepath)
    }
    let highligher = new Highlighter()
    let map = grouByAnnotation(state.edit.documentChanges ?? [], state.edit.changeAnnotations ?? {})
    for (let [label, changes] of map.entries()) {
      if (label) {
        highligher.addLine(label, 'MoreMsg')
        highligher.addLine('')
      }
      for (let change of changes) {
        if (TextDocumentEdit.is(change)) {
          let linesChange = state.changes[change.textDocument.uri]
          let fsPath = relpath(change.textDocument.uri)
          highligher.addTexts([
            { text: 'Change', hlGroup: 'Title' },
            { text: ' ' },
            { text: fsPath, hlGroup: 'Directory' },
            { text: `:${linesChange.lnum}`, hlGroup: 'LineNr' },
          ])
          this.addFile(fsPath, highligher, linesChange.lnum)
          highligher.addLine('')
          this.addChangedLines(highligher, linesChange, fsPath, linesChange.lnum)
          highligher.addLine('')
        } else if (CreateFile.is(change) || DeleteFile.is(change)) {
          let title = DeleteFile.is(change) ? 'Delete' : 'Create'
          let fsPath = relpath(change.uri)
          highligher.addTexts([
            { text: title, hlGroup: 'Title' },
            { text: ' ' },
            { text: fsPath, hlGroup: 'Directory' }
          ])
          this.addFile(fsPath, highligher)
          highligher.addLine('')
        } else if (RenameFile.is(change)) {
          let oldPath = relpath(change.oldUri)
          let newPath = relpath(change.newUri)
          highligher.addTexts([
            { text: 'Rename', hlGroup: 'Title' },
            { text: ' ' },
            { text: oldPath, hlGroup: 'Directory' },
            { text: '->', hlGroup: 'Comment' },
            { text: newPath, hlGroup: 'Directory' }
          ])
          this.renameMap.set(oldPath, newPath)
          this.addFile(newPath, highligher)
          highligher.addLine('')
        }
      }
    }
    nvim.pauseNotification()
    highligher.render(buffer)
    buffer.setOption('modifiable', false, true)
    await nvim.resumeNotification(true)
    this.disposables.push(this.keymaps.registerLocalKeymap('n', '<CR>', async () => {
      let lnum = await nvim.call('line', '.')
      let col = await nvim.call('col', '.')
      let find: ChangedFileItem
      for (let i = this.items.length - 1; i >= 0; i--) {
        let item = this.items[i]
        if (lnum >= item.index) {
          find = item
          break
        }
      }
      if (!find) return
      let uri = URI.file(absPath(find.filepath)).toString()
      let filepath = this.renameMap.has(find.filepath) ? this.renameMap.get(find.filepath) : find.filepath
      await nvim.call('coc#util#open_file', ['tab drop', absPath(filepath)])
      // need change old lnum to new lnum
      if (typeof find.lnum === 'number') {
        let changes = state.edit.documentChanges ?? []
        let change = changes.find(o => TextDocumentEdit.is(o) && o.textDocument.uri == uri) as TextDocumentEdit
        let lnum = find.lnum
        if (change) {
          let edits = mergeSort(change.edits, (a, b) => {
            let diff = a.range.start.line - b.range.start.line
            if (diff === 0) {
              return a.range.start.character - b.range.start.character
            }
            return diff
          })
          let pos = getPositionFromEdits(Position.create(lnum - 1, 0), edits)
          lnum = pos.line + 1
        }
        await nvim.call('cursor', [lnum, col])
      }
      nvim.redrawVim()
    }, true))
    this.disposables.push(this.keymaps.registerLocalKeymap('n', '<esc>', async () => {
      nvim.command('bwipeout!', true)
    }, true))
  }

  public addChangedLines(highligher: Highlighter, linesChange: LinesChange, fsPath: string, lnum: number): void {
    let diffs = fastDiff(linesChange.oldLines.join('\n'), linesChange.newLines.join('\n'))
    for (let i = 0; i < diffs.length; i++) {
      let diff = diffs[i]
      if (diff[0] == fastDiff.EQUAL) {
        let text = diff[1]
        if (!text.includes('\n')) {
          highligher.addText(text)
        } else {
          let parts = text.split('\n')
          highligher.addText(parts[0])
          let curr = lnum + parts.length - 1
          highligher.addLine('')
          highligher.addTexts([
            { text: 'Change', hlGroup: 'Title' },
            { text: ' ' },
            { text: fsPath, hlGroup: 'Directory' },
            { text: `:${curr}`, hlGroup: 'LineNr' },
          ])
          this.addFile(fsPath, highligher, curr)
          highligher.addLine('')
          let last = parts[parts.length - 1]
          if (last.length > 0) highligher.addText(last)
        }
        lnum += text.split('\n').length - 1
      } else if (diff[0] == fastDiff.DELETE) {
        lnum += diff[1].split('\n').length - 1
        highligher.addText(diff[1], 'DiffDelete')
      } else if (diff[0] == fastDiff.INSERT) {
        highligher.addText(diff[1], 'DiffAdd')
      }
    }
  }

  public dispose(): void {
    disposeAll(this.disposables)
  }
}

export function grouByAnnotation(changes: DocumentChange[], annotations: { [id: string]: ChangeAnnotation }): Map<string | null, DocumentChange[]> {
  let map: Map<string | null, DocumentChange[]> = new Map()
  for (let change of changes) {
    let id = getAnnotationKey(change) ?? null
    let key = id ? annotations[id].label ?? null : null
    let arr = map.get(key)
    if (arr) {
      arr.push(change)
    } else {
      map.set(key, [change])
    }
  }
  return map
}
