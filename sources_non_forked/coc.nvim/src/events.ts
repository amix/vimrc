'use strict'
import { CancellationToken, Disposable } from 'vscode-languageserver-protocol'
import { VimCompleteItem, CompleteDoneItem } from './types'
import { disposeAll } from './util'
import { equals } from './util/object'
import { byteSlice } from './util/string'
const logger = require('./util/logger')('events')

export type Result = void | Promise<void>

export interface PopupChangeEvent {
  readonly completed_item: VimCompleteItem | {}
  readonly height: number
  readonly width: number
  readonly row: number
  readonly col: number
  readonly size: number
  readonly scrollbar: boolean
}

export interface InsertChange {
  readonly lnum: number
  readonly col: number
  readonly line: string
  readonly changedtick: number
  pre: string
  /**
   * Insert character that cause change of this time.
   */
  insertChar?: string
}

export type BufEvents = 'BufHidden' | 'BufEnter'
  | 'InsertLeave' | 'TermOpen' | 'InsertEnter' | 'BufCreate' | 'BufUnload'
  | 'BufDetach' | 'Enter' | 'LinesChanged'

export type EmptyEvents = 'FocusGained' | 'FocusLost' | 'InsertSnippet' | 'ready' | 'VimLeavePre'

export type InsertChangeEvents = 'TextChangedP' | 'TextChangedI'

export type TaskEvents = 'TaskExit' | 'TaskStderr' | 'TaskStdout'

export type WindowEvents = 'WinLeave' | 'WinEnter' | 'WinClosed' | 'WinScrolled'

export type TabEvents = 'TabNew' | 'TabClosed'

export type AllEvents = BufEvents | EmptyEvents | CursorEvents | TaskEvents | WindowEvents | TabEvents
  | InsertChangeEvents | 'CompleteDone' | 'TextChanged' | 'MenuPopupChanged' | 'BufWritePost' | 'BufWritePre'
  | 'InsertCharPre' | 'FileType' | 'BufWinEnter' | 'BufWinLeave' | 'VimResized' | 'TermExit'
  | 'DirChanged' | 'OptionSet' | 'Command' | 'BufReadCmd' | 'GlobalChange' | 'InputChar'
  | 'WinLeave' | 'MenuInput' | 'PromptInsert' | 'FloatBtnClick' | 'InsertSnippet' | 'TextInsert'
  | 'PromptKeyPress'

export type CursorEvents = 'CursorMoved' | 'CursorMovedI' | 'CursorHold' | 'CursorHoldI'

export type OptionValue = string | number | boolean

export interface CursorPosition {
  readonly bufnr: number
  readonly lnum: number
  readonly col: number
  readonly insert: boolean
}

export interface LatestInsert {
  readonly bufnr: number
  readonly character: string
  readonly timestamp: number
}

class Events {

  private handlers: Map<string, ((...args: any[]) => Promise<unknown>)[]> = new Map()
  private _cursor: CursorPosition
  private _bufnr: number
  // bufnr & character
  private _recentInserts: [number, string][] = []
  private _lastChange = 0
  private _insertMode = false
  private _pumAlignTop = false
  private _pumVisible = false
  public completing = false

  public get cursor(): CursorPosition {
    return this._cursor
  }

  public get bufnr(): number {
    return this._bufnr
  }

  public get pumvisible(): boolean {
    return this._pumVisible
  }

  public get pumAlignTop(): boolean {
    return this._pumAlignTop
  }

  public get insertMode(): boolean {
    return this._insertMode
  }

  public get lastChangeTs(): number {
    return this._lastChange
  }

  /**
   * Resolved when first event fired or timeout
   */
  public race(events: AllEvents[], token?: number | CancellationToken): Promise<{ name: AllEvents, args: unknown[] } | undefined> {
    let disposables: Disposable[] = []
    return new Promise(resolve => {
      if (typeof token === 'number') {
        let timer = setTimeout(() => {
          disposeAll(disposables)
          resolve(undefined)
        }, token)
        disposables.push(Disposable.create(() => {
          clearTimeout(timer)
        }))
      } else if (CancellationToken.is(token)) {
        token.onCancellationRequested(() => {
          disposeAll(disposables)
          resolve(undefined)
        }, null, disposables)
      }
      events.forEach(ev => {
        this.on(ev, (...args) => {
          disposeAll(disposables)
          resolve({ name: ev, args })
        }, null, disposables)
      })
    })
  }

  public async fire(event: string, args: any[]): Promise<void> {
    let cbs = this.handlers.get(event)
    if (event == 'InsertEnter') {
      this._insertMode = true
    } else if (event == 'InsertLeave') {
      this._insertMode = false
      this._pumVisible = false
      this._recentInserts = []
    } else if (event == 'CursorHoldI' || event == 'CursorMovedI') {
      this._bufnr = args[0]
      if (!this._insertMode) {
        this._insertMode = true
        void this.fire('InsertEnter', [args[0]])
      }
    } else if (event == 'CursorHold' || event == 'CursorMoved') {
      this._bufnr = args[0]
      if (this._insertMode) {
        this._insertMode = false
        void this.fire('InsertLeave', [args[0]])
      }
    } else if (event == 'MenuPopupChanged') {
      this._pumVisible = true
      this._pumAlignTop = args[1] > args[0].row
    } else if (event == 'CompleteDone') {
      this._pumVisible = false
    } else if (event == 'InsertCharPre') {
      this._recentInserts.push([args[1], args[0]])
    } else if (event == 'TextChanged') {
      this._lastChange = Date.now()
    } else if (event == 'BufEnter') {
      this._bufnr = args[0]
    } else if (event == 'TextChangedI' || event == 'TextChangedP') {
      let arr = this._recentInserts.filter(o => o[0] == args[0])
      this._bufnr = args[0]
      this._recentInserts = []
      this._pumVisible = event == 'TextChangedP'
      this._lastChange = Date.now()
      let info: InsertChange = args[1]
      let pre = byteSlice(info.line ?? '', 0, info.col - 1)
      info.pre = pre
      // fix cursor since vim not send CursorMovedI event
      this._cursor = Object.freeze({
        bufnr: args[0],
        lnum: info.lnum,
        col: info.col,
        insert: true
      })
      if (arr.length && pre.length) {
        let character = pre.slice(-1)
        if (arr.findIndex(o => o[1] == character) !== -1) {
          info.insertChar = character
          // make it fires after TextChangedI & TextChangedP
          process.nextTick(() => {
            void this.fire('TextInsert', [...args, character])
          })
        }
      }
    }
    if (event == 'CursorMoved' || event == 'CursorMovedI') {
      args.push(this._recentInserts.length > 0)
      let cursor = {
        bufnr: args[0],
        lnum: args[1][0],
        col: args[1][1],
        insert: event == 'CursorMovedI'
      }
      // Avoid CursorMoved event when it's not moved at all
      if (this._cursor && equals(this._cursor, cursor)) return
      this._cursor = Object.freeze(cursor)
    }
    if (cbs) {
      try {
        args.forEach(val => {
          if (typeof val === 'object') Object.freeze(val)
        })
        // need slice since the array might changed when execute fn
        await Promise.all(cbs.slice().map(fn => fn(args)))
      } catch (e) {
        if (e instanceof Error && e.message?.includes('transport disconnected')) return
        logger.error(`Error on event: ${event}`, e instanceof Error ? e.stack : e)
      }
    }
  }

  public on(event: BufEvents, handler: (bufnr: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: CursorEvents, handler: (bufnr: number, cursor: [number, number]) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: InsertChangeEvents, handler: (bufnr: number, info: InsertChange) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: WindowEvents, handler: (winid: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: TabEvents, handler: (tabnr: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'TextInsert', handler: (bufnr: number, info: InsertChange, character: string) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'FloatBtnClick', handler: (bufnr: number, index: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'PromptKeyPress', handler: (bufnr: number, key: string) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'BufWritePre', handler: (bufnr: number, bufname: string, changedtick: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'TextChanged' | 'BufWritePost', handler: (bufnr: number, changedtick: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'TaskExit', handler: (id: string, code: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'TaskStderr' | 'TaskStdout', handler: (id: string, lines: string[]) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'BufReadCmd', handler: (scheme: string, fullpath: string) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'VimResized', handler: (columns: number, lines: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'Command', handler: (name: string) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'MenuPopupChanged', handler: (event: PopupChangeEvent, cursorline: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'CompleteDone', handler: (item: CompleteDoneItem) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'InsertCharPre', handler: (character: string, bufnr: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'FileType', handler: (filetype: string, bufnr: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'BufWinEnter' | 'BufWinLeave', handler: (bufnr: number, winid: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'TermExit', handler: (bufnr: number, status: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'DirChanged', handler: (cwd: string) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'OptionSet' | 'GlobalChange', handler: (option: string, oldVal: OptionValue, newVal: OptionValue) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'InputChar', handler: (session: string, character: string, mode: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: 'PromptInsert', handler: (value: string, bufnr: number) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: EmptyEvents, handler: () => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: AllEvents | AllEvents[], handler: (...args: unknown[]) => Result, thisArg?: any, disposables?: Disposable[]): Disposable
  public on(event: AllEvents[] | AllEvents, handler: (...args: any[]) => Result, thisArg?: any, disposables?: Disposable[]): Disposable {
    if (Array.isArray(event)) {
      let arr = disposables || []
      for (let ev of event) {
        this.on(ev as any, handler, thisArg, arr)
      }
      return Disposable.create(() => {
        disposeAll(arr)
      })
    } else {
      let arr = this.handlers.get(event) || []
      let wrappedhandler = args => new Promise((resolve, reject) => {
        try {
          Promise.resolve(handler.apply(thisArg ?? null, args)).then(() => {
            resolve(undefined)
          }, e => {
            reject(e)
          })
        } catch (e) {
          reject(e)
        }
      })
      arr.push(wrappedhandler)
      this.handlers.set(event, arr)
      let disposable = Disposable.create(() => {
        let idx = arr.indexOf(wrappedhandler)
        if (idx !== -1) {
          arr.splice(idx, 1)
        }
      })
      if (Array.isArray(disposables)) {
        disposables.push(disposable)
      }
      return disposable
    }
  }
}
export default new Events()
