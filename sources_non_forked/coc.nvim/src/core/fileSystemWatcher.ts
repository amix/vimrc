'use strict'
import minimatch from 'minimatch'
import path from 'path'
import { Disposable, Emitter, WorkspaceFolder, Event } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import { OutputChannel } from '../types'
import { disposeAll } from '../util'
import { splitArray } from '../util/array'
import Watchman, { FileChange } from './watchman'
import WorkspaceFolderControl from './workspaceFolder'
const logger = require('../util/logger')('filesystem-watcher')

export interface RenameEvent {
  oldUri: URI
  newUri: URI
}

export class FileSystemWatcherManager {
  private clientsMap: Map<string, Watchman | null> = new Map()
  private disposables: Disposable[] = []
  private channel: OutputChannel | undefined
  private creating: Set<string> = new Set()
  public static watchers: Set<FileSystemWatcher> = new Set()
  private readonly _onDidCreateClient = new Emitter<string>()
  public readonly onDidCreateClient: Event<string> = this._onDidCreateClient.event
  constructor(
    private workspaceFolder: WorkspaceFolderControl,
    private watchmanPath: string | null
  ) {
  }

  public attach(channel: OutputChannel): void {
    this.channel = channel
    let createClient = (folder: WorkspaceFolder) => {
      let root = URI.parse(folder.uri).fsPath
      if (this.creating.has(root)) return
      this.creating.add(root)
      this.createClient(root).finally(() => {
        this.creating.delete(root)
      })
    }
    this.workspaceFolder.workspaceFolders.forEach(folder => {
      createClient(folder)
    })
    this.workspaceFolder.onDidChangeWorkspaceFolders(e => {
      e.added.forEach(folder => {
        createClient(folder)
      })
      e.removed.forEach(folder => {
        let root = URI.parse(folder.uri).fsPath
        let client = this.clientsMap.get(root)
        if (client) {
          this.clientsMap.delete(root)
          client.dispose()
        }
      })
    }, null, this.disposables)
  }

  public waitClient(root: string): Promise<void> {
    if (this.clientsMap.has(root)) return Promise.resolve()
    return new Promise(resolve => {
      let disposable = this.onDidCreateClient(r => {
        if (r == root) {
          disposable.dispose()
          resolve()
        }
      })
    })
  }

  public async createClient(root: string): Promise<void> {
    if (this.watchmanPath == null || this.clientsMap.has(root)) return
    try {
      let client = await Watchman.createClient(this.watchmanPath, root, this.channel)
      if (!client) return
      this.clientsMap.set(root, client)
      for (let watcher of FileSystemWatcherManager.watchers) {
        watcher.listen(client)
      }
      this._onDidCreateClient.fire(root)
    } catch (e) {
      if (this.channel) this.channel.appendLine(`Error on create watchman client:` + e)
    }
  }

  public createFileSystemWatcher(
    globPattern: string,
    ignoreCreateEvents: boolean,
    ignoreChangeEvents: boolean,
    ignoreDeleteEvents: boolean): FileSystemWatcher {
    let fileWatcher = new FileSystemWatcher(globPattern, ignoreCreateEvents, ignoreChangeEvents, ignoreDeleteEvents)
    for (let client of this.clientsMap.values()) {
      fileWatcher.listen(client)
    }
    FileSystemWatcherManager.watchers.add(fileWatcher)
    return fileWatcher
  }

  public dispose(): void {
    this._onDidCreateClient.dispose()
    for (let client of this.clientsMap.values()) {
      if (client) client.dispose()
    }
    this.clientsMap.clear()
    FileSystemWatcherManager.watchers.clear()
    disposeAll(this.disposables)
  }
}

/*
 * FileSystemWatcher for watch workspace folders.
 */
export class FileSystemWatcher implements Disposable {
  private _onDidCreate = new Emitter<URI>()
  private _onDidChange = new Emitter<URI>()
  private _onDidDelete = new Emitter<URI>()
  private _onDidRename = new Emitter<RenameEvent>()
  private disposables: Disposable[] = []
  private _disposed = false
  public subscribe: string
  public readonly onDidCreate: Event<URI> = this._onDidCreate.event
  public readonly onDidChange: Event<URI> = this._onDidChange.event
  public readonly onDidDelete: Event<URI> = this._onDidDelete.event
  public readonly onDidRename: Event<RenameEvent> = this._onDidRename.event

  constructor(
    private globPattern: string,
    public ignoreCreateEvents: boolean,
    public ignoreChangeEvents: boolean,
    public ignoreDeleteEvents: boolean,
  ) {
  }

  public listen(client: Watchman): void {
    let { globPattern,
      ignoreCreateEvents,
      ignoreChangeEvents,
      ignoreDeleteEvents } = this
    const onChange = (change: FileChange) => {
      let { root, files } = change
      files = files.filter(f => f.type == 'f' && minimatch(f.name, globPattern, { dot: true }))
      for (let file of files) {
        let uri = URI.file(path.join(root, file.name))
        if (!file.exists) {
          if (!ignoreDeleteEvents) this._onDidDelete.fire(uri)
        } else {
          if (file.new === true) {
            if (!ignoreCreateEvents) this._onDidCreate.fire(uri)
          } else {
            if (!ignoreChangeEvents) this._onDidChange.fire(uri)
          }
        }
      }
      // file rename
      if (files.length == 2 && !files[0].exists && files[1].exists) {
        let oldFile = files[0]
        let newFile = files[1]
        if (oldFile.size == newFile.size) {
          this._onDidRename.fire({
            oldUri: URI.file(path.join(root, oldFile.name)),
            newUri: URI.file(path.join(root, newFile.name))
          })
        }
      }
      // detect folder rename
      if (files.length >= 2) {
        let [oldFiles, newFiles] = splitArray(files, o => o.exists === false)
        if (oldFiles.length == newFiles.length) {
          for (let oldFile of oldFiles) {
            let newFile = newFiles.find(o => o.size == oldFile.size && o.mtime_ms == oldFile.mtime_ms)
            if (newFile) {
              this._onDidRename.fire({
                oldUri: URI.file(path.join(root, oldFile.name)),
                newUri: URI.file(path.join(root, newFile.name))
              })
            }
          }
        }
      }
    }
    client.subscribe(globPattern, onChange).then(disposable => {
      this.subscribe = disposable.subscribe
      if (this._disposed) return disposable.dispose()
      this.disposables.push(disposable)
    }).logError()
  }

  public dispose(): void {
    this._disposed = true
    FileSystemWatcherManager.watchers.delete(this)
    this._onDidRename.dispose()
    this._onDidCreate.dispose()
    this._onDidChange.dispose()
    disposeAll(this.disposables)
  }
}
