'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Emitter, Event } from 'vscode-languageserver-protocol'
import events from '../events'
import { disposeAll } from '../util'
const logger = require('../util/logger')('model-input')

export interface InputPreference {
  position?: 'cursor' | 'center'
  marginTop?: number
  border?: [0 | 1, 0 | 1, 0 | 1, 0 | 1]
  rounded?: boolean
  minWidth?: number
  maxWidth?: number
  highlight?: string
  borderhighlight?: string
  /**
   * map list key-mappings
   */
  list?: boolean
}

export interface Dimension {
  width: number
  height: number
  row: number
  col: number
}

export type InputOptions = Pick<InputPreference, 'borderhighlight' | 'position' | 'marginTop'>

type RequestResult = [number, number, [number, number, number, number]]

export default class InputBox implements Disposable {
  private disposables: Disposable[] = []
  private _winid: number | undefined
  private _bufnr: number | undefined
  private _input: string
  private accepted = false
  public title: string
  public loading: boolean
  public borderhighlight: string
  // width, height, row, col
  private _dimension: [number, number, number, number] = [0, 0, 0, 0]
  private readonly _onDidFinish = new Emitter<string>()
  private readonly _onDidChange = new Emitter<string>()
  public readonly onDidFinish: Event<string | null> = this._onDidFinish.event
  public readonly onDidChange: Event<string> = this._onDidChange.event
  constructor(private nvim: Neovim, defaultValue: string) {
    this._input = defaultValue
    this.disposables.push(this._onDidFinish)
    this.disposables.push(this._onDidChange)
    let _title: string | undefined
    Object.defineProperty(this, 'title', {
      set: (newTitle: string) => {
        _title = newTitle
        if (this._winid) nvim.call('coc#dialog#change_title', [this._winid, newTitle], true)
      },
      get: () => {
        return _title
      }
    })
    let _loading = false
    Object.defineProperty(this, 'loading', {
      set: (loading: boolean) => {
        _loading = loading
        if (this._winid) nvim.call('coc#dialog#change_loading', [this._winid, loading], true)
      },
      get: () => {
        return _loading
      }
    })
    let _borderhighlight: string
    Object.defineProperty(this, 'borderhighlight', {
      set: (borderhighlight: string) => {
        _borderhighlight = borderhighlight
        if (this._winid) nvim.call('coc#dialog#change_border_hl', [this._winid, borderhighlight], true)
      },
      get: () => {
        return _borderhighlight
      }
    })
    events.on('BufWinLeave', bufnr => {
      if (bufnr == this._bufnr) {
        this._winid = undefined
        this.dispose()
      }
    }, null, this.disposables)
    events.on('PromptInsert', (value, bufnr) => {
      if (bufnr == this._bufnr) {
        this._input = value
        this.accepted = true
        this.dispose()
      }
    }, null, this.disposables)
    events.on('TextChangedI', (bufnr, info) => {
      if (bufnr == this._bufnr) {
        this._input = info.line
        this._onDidChange.fire(info.line)
      }
    }, null, this.disposables)
  }

  public get dimension(): Dimension | undefined {
    let { _dimension } = this
    return { width: _dimension[0], height: _dimension[1], row: _dimension[2], col: _dimension[3] }
  }

  public get bufnr(): number | undefined {
    return this._bufnr
  }

  public get winid(): number | undefined {
    return this._winid
  }

  public get value(): string {
    return this._input
  }

  public async show(title: string, preferences: InputPreference): Promise<boolean> {
    this.title = title
    this.borderhighlight = preferences.borderhighlight ?? 'CocFloating'
    this.loading = false
    let res = await this.nvim.call('coc#dialog#create_prompt_win', [title, this._input, preferences]) as RequestResult
    if (!res) throw new Error('Unable to open input window')
    this._bufnr = res[0]
    this._winid = res[1]
    this._dimension = res[2]
    return true
  }

  public dispose(): void {
    this._onDidFinish.fire(this.accepted ? this._input : null)
    if (this._winid) {
      this.nvim.call('coc#float#close', [this._winid], true)
    }
    this._winid = undefined
    this._bufnr = undefined
    disposeAll(this.disposables)
  }
}
