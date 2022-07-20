'use strict'
import { Neovim } from '@chemzqm/neovim'
import fs from 'fs-extra'
import glob from 'glob'
import minimatch from 'minimatch'
import os from 'os'
import path from 'path'
import { promisify } from 'util'
import { v4 as uuid } from 'uuid'
import { CancellationToken, CancellationTokenSource, CreateFile, CreateFileOptions, DeleteFile, DeleteFileOptions, Emitter, Event, Position, RenameFile, RenameFileOptions, TextDocumentEdit, WorkspaceEdit } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import Configurations from '../configuration'
import events from '../events'
import Document from '../model/document'
import EditInspect, { EditState, RecoverFunc } from '../model/editInspect'
import RelativePattern from '../model/relativePattern'
import { DocumentChange, Env, FileCreateEvent, FileDeleteEvent, FileRenameEvent, FileWillCreateEvent, FileWillDeleteEvent, FileWillRenameEvent, LinesChange } from '../types'
import * as errors from '../util/errors'
import { fixDriver, isFile, isParentFolder, statAsync } from '../util/fs'
import { byteLength } from '../util/string'
import { getAnnotationKey, getConfirmAnnotations, toDocumentChanges } from '../util/textedit'
import type { Window } from '../window'
import Documents from './documents'
import type Keymaps from './keymaps'
import * as ui from './ui'
import WorkspaceFolderController from './workspaceFolder'

export type GlobPattern = string | RelativePattern

interface WaitUntilEvent {
  waitUntil(thenable: Thenable<WorkspaceEdit | any>): void
}

const logger = require('../util/logger')('core-files')

export default class Files {
  private nvim: Neovim
  private env: Env
  private window: Window
  private editState: EditState | undefined
  private operationTimeout = 500
  private _onDidCreateFiles = new Emitter<FileCreateEvent>()
  private _onDidRenameFiles = new Emitter<FileRenameEvent>()
  private _onDidDeleteFiles = new Emitter<FileDeleteEvent>()
  private _onWillCreateFiles = new Emitter<FileWillCreateEvent>()
  private _onWillRenameFiles = new Emitter<FileWillRenameEvent>()
  private _onWillDeleteFiles = new Emitter<FileWillDeleteEvent>()

  public readonly onDidCreateFiles: Event<FileCreateEvent> = this._onDidCreateFiles.event
  public readonly onDidRenameFiles: Event<FileRenameEvent> = this._onDidRenameFiles.event
  public readonly onDidDeleteFiles: Event<FileDeleteEvent> = this._onDidDeleteFiles.event
  public readonly onWillCreateFiles: Event<FileWillCreateEvent> = this._onWillCreateFiles.event
  public readonly onWillRenameFiles: Event<FileWillRenameEvent> = this._onWillRenameFiles.event
  public readonly onWillDeleteFiles: Event<FileWillDeleteEvent> = this._onWillDeleteFiles.event
  constructor(
    private documents: Documents,
    private configurations: Configurations,
    private workspaceFolderControl: WorkspaceFolderController,
    private keymaps: Keymaps
  ) {
  }

  public attach(nvim: Neovim, env: Env, window: Window): void {
    this.nvim = nvim
    this.env = env
    this.window = window
  }

  public async openTextDocument(uri: URI | string): Promise<Document> {
    uri = typeof uri === 'string' ? URI.file(uri) : uri
    let doc = this.documents.getDocument(uri.toString())
    if (doc) {
      await this.jumpTo(uri.toString(), null, 'drop')
      return doc
    }
    const scheme = uri.scheme
    if (scheme == 'file') {
      if (!fs.existsSync(uri.fsPath)) throw errors.fileNotExists(uri.fsPath)
      fs.accessSync(uri.fsPath, fs.constants.R_OK)
    }
    if (scheme == 'untitled') {
      await this.nvim.call('coc#util#open_file', ['tab drop', uri.path])
      return await this.documents.document
    }
    return await this.loadResource(uri.toString())
  }

  public async jumpTo(uri: string, position?: Position | null, openCommand?: string): Promise<void> {
    const preferences = this.configurations.getConfiguration('coc.preferences')
    let jumpCommand = openCommand || preferences.get<string>('jumpCommand', 'edit')
    let { nvim } = this
    let doc = this.documents.getDocument(uri)
    let bufnr = doc ? doc.bufnr : -1
    if (bufnr != -1 && jumpCommand == 'edit') {
      // use buffer command since edit command would reload the buffer
      nvim.pauseNotification()
      nvim.command(`silent! normal! m'`, true)
      nvim.command(`buffer ${bufnr}`, true)
      nvim.command(`if &filetype ==# '' | filetype detect | endif`, true)
      if (position) {
        let line = doc.getline(position.line)
        let col = byteLength(line.slice(0, position.character)) + 1
        nvim.call('cursor', [position.line + 1, col], true)
      }
      await nvim.resumeNotification(true)
    } else {
      let { fsPath, scheme } = URI.parse(uri)
      let pos = position == null ? null : [position.line, position.character]
      if (scheme == 'file') {
        let bufname = fixDriver(path.normalize(fsPath))
        await this.nvim.call('coc#util#jump', [jumpCommand, bufname, pos])
      } else {
        await this.nvim.call('coc#util#jump', [jumpCommand, uri, pos])
      }
    }
  }

  /**
   * Open resource by uri
   */
  public async openResource(uri: string): Promise<void> {
    let { nvim } = this
    let u = URI.parse(uri)
    if (/^https?/.test(u.scheme)) {
      await nvim.call('coc#ui#open_url', uri)
      return
    }
    let wildignore = await nvim.getOption('wildignore')
    await nvim.setOption('wildignore', '')
    await this.jumpTo(uri)
    await nvim.setOption('wildignore', wildignore)
  }

  /**
   * Load uri as document.
   */
  public async loadResource(uri: string): Promise<Document> {
    let doc = this.documents.getDocument(uri)
    if (doc) return doc
    const preferences = this.configurations.getConfiguration('workspace')
    let cmd = preferences.get<string>('openResourceCommand', 'tab drop')
    let u = URI.parse(uri)
    let bufname = u.scheme === 'file' ? u.fsPath : uri
    let bufnr: number
    if (cmd) {
      let winid = await this.nvim.call('win_getid')
      bufnr = await this.nvim.call('coc#util#open_file', [cmd, bufname])
      await this.nvim.call('win_gotoid', [winid])
    } else {
      let arr = await this.nvim.call('coc#ui#open_files', [[bufname]])
      bufnr = arr[0]
    }
    return await this.documents.createDocument(bufnr)
  }

  /**
   * Load the files that not loaded
   */
  public async loadResources(uris: string[]): Promise<(Document | undefined)[]> {
    let { documents } = this
    let files = uris.map(uri => {
      let u = URI.parse(uri)
      return u.scheme == 'file' ? u.fsPath : uri
    })
    let bufnrs = await this.nvim.call('coc#ui#open_files', [files]) as number[]
    return await Promise.all(bufnrs.map(bufnr => {
      return documents.createDocument(bufnr)
    }))
  }

  /**
   * Create a file in vim and disk
   */
  public async createFile(filepath: string, opts: CreateFileOptions = {}, recovers?: RecoverFunc[]): Promise<void> {
    let { nvim } = this
    let exists = fs.existsSync(filepath)
    if (exists && !opts.overwrite && !opts.ignoreIfExists) {
      throw errors.fileExists(filepath)
    }
    if (!exists || opts.overwrite) {
      let tokenSource = new CancellationTokenSource()
      await this.fireWaitUntilEvent(this._onWillCreateFiles, {
        files: [URI.file(filepath)],
        token: tokenSource.token
      }, recovers)
      tokenSource.cancel()
      let dir = path.dirname(filepath)
      if (!fs.existsSync(dir)) {
        let folder: string
        let curr = dir
        while (!['.', '/', path.parse(dir).root].includes(curr)) {
          if (fs.existsSync(path.dirname(curr))) {
            folder = curr
            break
          }
          curr = path.dirname(curr)
        }
        await fs.mkdirp(dir)
        recovers && recovers.push(async () => {
          if (fs.existsSync(folder)) {
            await fs.remove(folder)
          }
        })
      }
      fs.writeFileSync(filepath, '', 'utf8')
      recovers && recovers.push(async () => {
        if (fs.existsSync(filepath)) {
          await fs.unlink(filepath)
        }
      })
      let doc = await this.loadResource(filepath)
      let bufnr = doc.bufnr
      recovers && recovers.push(() => {
        void events.fire('BufUnload', [bufnr])
        return nvim.command(`silent! bd! ${bufnr}`)
      })
      this._onDidCreateFiles.fire({ files: [URI.file(filepath)] })
    }
  }

  /**
   * Delete a file or folder from vim and disk.
   */
  public async deleteFile(filepath: string, opts: DeleteFileOptions = {}, recovers?: RecoverFunc[]): Promise<void> {
    let { ignoreIfNotExists, recursive } = opts
    let stat = await statAsync(filepath)
    let isDir = stat && stat.isDirectory()
    if (!stat && !ignoreIfNotExists) {
      throw errors.fileNotExists(filepath)
    }
    if (stat == null) return
    let uri = URI.file(filepath)
    await this.fireWaitUntilEvent(this._onWillDeleteFiles, { files: [uri] }, recovers)
    if (!isDir) {
      let bufnr = await this.nvim.call('bufnr', [filepath])
      if (bufnr) {
        void events.fire('BufUnload', [bufnr])
        await this.nvim.command(`silent! bwipeout ${bufnr}`)
        recovers && recovers.push(() => {
          return this.loadResource(uri.toString())
        })
      }
    }
    if (isDir && recursive) {
      // copy files for recover
      let folder = path.join(os.tmpdir(), 'coc-' + uuid())
      await fs.mkdir(folder)
      await fs.copy(filepath, folder, { recursive: true })
      await fs.remove(filepath)
      recovers && recovers.push(async () => {
        await fs.mkdir(filepath)
        await fs.copy(folder, filepath, { recursive: true })
        await fs.remove(folder)
      })
    } else if (isDir) {
      await fs.rmdir(filepath)
      recovers && recovers.push(() => {
        return fs.mkdir(filepath)
      })
    } else {
      let dest = path.join(os.tmpdir(), 'coc-' + uuid())
      await fs.copyFile(filepath, dest)
      await fs.unlink(filepath)
      recovers && recovers.push(() => {
        return fs.move(dest, filepath, { overwrite: true })
      })
    }
    this._onDidDeleteFiles.fire({ files: [uri] })
  }

  /**
   * Rename a file or folder on vim and disk
   */
  public async renameFile(oldPath: string, newPath: string, opts: RenameFileOptions & { skipEvent?: boolean } = {}, recovers?: RecoverFunc[]): Promise<void> {
    let { nvim } = this
    let { overwrite, ignoreIfExists } = opts
    if (newPath === oldPath) return
    let exists = fs.existsSync(newPath)
    if (exists && ignoreIfExists && !overwrite) return
    if (exists && !overwrite) throw errors.fileExists(newPath)
    let oldStat = await statAsync(oldPath)
    let loaded = (oldStat && oldStat.isDirectory()) ? 0 : await nvim.call('bufloaded', [oldPath])
    if (!loaded && !oldStat) throw errors.fileNotExists(oldPath)
    let file = { newUri: URI.parse(newPath), oldUri: URI.parse(oldPath) }
    if (!opts.skipEvent) await this.fireWaitUntilEvent(this._onWillRenameFiles, { files: [file] }, recovers)
    if (loaded) {
      let bufnr = await nvim.call('coc#ui#rename_file', [oldPath, newPath, oldStat != null])
      await this.documents.onBufCreate(bufnr)
    } else {
      if (oldStat?.isDirectory()) {
        for (let doc of this.documents.documents) {
          let u = URI.parse(doc.uri)
          if (u.scheme === 'file' && isParentFolder(oldPath, u.fsPath, false)) {
            let filepath = u.fsPath.replace(oldPath, newPath)
            let bufnr = await nvim.call('coc#ui#rename_file', [u.fsPath, filepath, false])
            await this.documents.onBufCreate(bufnr)
          }
        }
      }
      fs.renameSync(oldPath, newPath)
    }
    recovers && recovers.push(() => {
      return this.renameFile(newPath, oldPath, { skipEvent: true })
    })
    if (!opts.skipEvent) this._onDidRenameFiles.fire({ files: [file] })
  }

  public async renameCurrent(): Promise<void> {
    let { nvim } = this
    let oldPath = await nvim.call('expand', ['%:p'])
    // await nvim.callAsync()
    let newPath = await nvim.callAsync('coc#util#with_callback', ['input', ['New path: ', oldPath, 'file']])
    newPath = newPath ? newPath.trim() : null
    if (newPath === oldPath || !newPath) return
    if (oldPath.toLowerCase() != newPath.toLowerCase() && fs.existsSync(newPath)) {
      let overwrite = await ui.showPrompt(this.nvim, `${newPath} exists, overwrite?`)
      if (!overwrite) return
    }
    await this.renameFile(oldPath, newPath, { overwrite: true })
  }

  private get currentUri(): string {
    let document = this.documents.getDocument(this.documents.bufnr)
    return document ? document.uri : null
  }

  /**
   * Apply WorkspaceEdit.
   */
  public async applyEdit(edit: WorkspaceEdit, nested?: boolean): Promise<boolean> {
    let documentChanges = toDocumentChanges(edit)
    let recovers: RecoverFunc[] = []
    let currentOnly = false
    try {
      let { changeAnnotations } = edit
      let { currentUri } = this
      let toConfirm = changeAnnotations ? getConfirmAnnotations(documentChanges, changeAnnotations) : []
      let changes: { [uri: string]: LinesChange } = {}
      let denied: string[] = []
      for (let key of toConfirm) {
        let annotation = changeAnnotations[key]
        annotation.needsConfirmation = false
        let res = await this.window.showMenuPicker(['Yes', 'No'], {
          position: 'center',
          title: 'Confirm edits',
          content: annotation.label + (annotation.description ? ' ' + annotation.description : '')
        })
        if (res !== 0) denied.push(key)
      }
      documentChanges = documentChanges.filter(c => !denied.includes(getAnnotationKey(c)))
      if (!documentChanges.length) return true
      currentOnly = documentChanges.every(o => TextDocumentEdit.is(o) && o.textDocument.uri === currentUri)
      this.validateChanges(documentChanges)
      for (const change of documentChanges) {
        if (TextDocumentEdit.is(change)) {
          let { textDocument, edits } = change
          let { uri } = textDocument
          let doc = await this.loadResource(uri)
          let revertEdit = await doc.applyEdits(edits, false, uri === currentUri)
          if (revertEdit) {
            let version = doc.version
            let { newText, range } = revertEdit
            changes[uri] = {
              uri,
              lnum: range.start.line + 1,
              newLines: doc.getLines(range.start.line, range.end.line),
              oldLines: newText.endsWith('\n') ? newText.slice(0, -1).split('\n') : newText.split('\n')
            }
            recovers.push(async () => {
              let doc = this.documents.getDocument(uri)
              if (!doc || !doc.attached || doc.version !== version) return
              await doc.applyEdits([revertEdit])
              textDocument.version = doc.version
            })
          }
        } else if (CreateFile.is(change)) {
          await this.createFile(fsPath(change.uri), change.options, recovers)
        } else if (DeleteFile.is(change)) {
          await this.deleteFile(fsPath(change.uri), change.options, recovers)
        } else if (RenameFile.is(change)) {
          await this.renameFile(fsPath(change.oldUri), fsPath(change.newUri), change.options, recovers)
        }
      }
      // nothing changed
      if (recovers.length === 0) return true
      if (!nested) this.editState = { edit: { documentChanges, changeAnnotations: edit.changeAnnotations }, changes, recovers, applied: true }
      this.nvim.redrawVim()
    } catch (e) {
      logger.error('Error on applyEdits:', edit, e)
      await this.undoChanges(recovers)
      if (!nested) void this.window.showErrorMessage(`Error on applyEdits: ${e}`)
      return false
    }
    // avoid message when change current file only.
    if (nested || currentOnly) return true
    void this.window.showInformationMessage(`Use ':wa' to save changes or ':CocCommand workspace.inspectEdit' to inspect.`)
    return true
  }

  private async undoChanges(recovers: RecoverFunc[]): Promise<void> {
    while (recovers.length > 0) {
      let fn = recovers.pop()
      await fn()
    }
  }

  public async inspectEdit(): Promise<void> {
    if (!this.editState) {
      void this.window.showWarningMessage('No workspace edit to inspect')
      return
    }
    let inspect = new EditInspect(this.nvim, this.keymaps)
    await inspect.show(this.editState)
  }

  public async undoWorkspaceEdit(): Promise<void> {
    let { editState } = this
    if (!editState || !editState.applied) {
      void this.window.showWarningMessage(`No workspace edit to undo`)
      return
    }
    editState.applied = false
    await this.undoChanges(editState.recovers)
  }

  public async redoWorkspaceEdit(): Promise<void> {
    let { editState } = this
    if (!editState || editState.applied) {
      void this.window.showWarningMessage(`No workspace edit to redo`)
      return
    }
    this.editState = undefined
    await this.applyEdit(editState.edit)
  }

  private validateChanges(documentChanges: ReadonlyArray<DocumentChange>): void {
    let { documents } = this
    for (let change of documentChanges) {
      if (TextDocumentEdit.is(change)) {
        let { uri, version } = change.textDocument
        let doc = documents.getDocument(uri)
        if (typeof version === 'number' && version > 0) {
          if (!doc) throw new Error(`File ${uri} not loaded`)
          if (doc.version != version) throw new Error(`${uri} changed before apply edit`)
        } else if (!doc) {
          if (!isFile(uri)) throw errors.badScheme(URI.parse(uri).scheme)
        }
      } else if (CreateFile.is(change) || DeleteFile.is(change)) {
        if (!isFile(change.uri)) throw errors.badScheme(URI.parse(change.uri).scheme)
      } else if (RenameFile.is(change)) {
        if (!isFile(change.oldUri) || !isFile(change.newUri)) {
          throw errors.badScheme(URI.parse(change.oldUri).scheme)
        }
      }
    }
  }

  public async findFiles(include: GlobPattern, exclude?: GlobPattern | null, maxResults?: number, token?: CancellationToken): Promise<URI[]> {
    let folders = this.workspaceFolderControl.workspaceFolders
    if (token?.isCancellationRequested || !folders.length || maxResults === 0) return []
    maxResults = maxResults ?? Infinity
    let roots = folders.map(o => URI.parse(o.uri).fsPath)
    if (typeof include !== 'string') {
      let base = include.baseUri.fsPath
      roots = roots.filter(r => isParentFolder(base, r, true))
    }
    let pattern = typeof include === 'string' ? include : include.pattern
    let res: URI[] = []
    for (let root of roots) {
      if (res.length >= maxResults) break
      let files = await promisify(glob)(pattern, {
        dot: true,
        cwd: root,
        nodir: true,
        absolute: false
      })
      if (token?.isCancellationRequested) return []
      for (let file of files) {
        if (exclude && fileMatch(root, file, exclude)) continue
        res.push(URI.file(path.join(root, file)))
        if (res.length === maxResults) break
      }
    }
    return res
  }

  private async fireWaitUntilEvent<T extends WaitUntilEvent>(emitter: Emitter<T>, properties: Omit<T, 'waitUntil'>, recovers?: RecoverFunc[]): Promise<void> {
    let firing = true
    let promises: Promise<any>[] = []
    emitter.fire({
      ...properties,
      waitUntil: thenable => {
        if (!firing) throw errors.shouldNotAsync('waitUntil')
        let tp = new Promise(resolve => {
          setTimeout(resolve, this.operationTimeout)
        })
        let promise = Promise.race([thenable, tp]).then(edit => {
          if (edit && WorkspaceEdit.is(edit)) {
            return this.applyEdit(edit, true)
          }
        })
        promises.push(promise)
      }
    } as any)
    firing = false
    await Promise.all(promises)
  }
}

function fileMatch(root: string, relpath: string, pattern: GlobPattern): boolean {
  let filepath = path.join(root, relpath)
  if (typeof pattern !== 'string') {
    let base = pattern.baseUri.fsPath
    if (!isParentFolder(base, filepath)) return false
    let rp = path.relative(base, filepath)
    return minimatch(rp, pattern.pattern, { dot: true })
  }
  return minimatch(relpath, pattern, { dot: true })
}

function fsPath(uri: string): string {
  return URI.parse(uri).fsPath
}
