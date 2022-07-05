'use strict'
import DB from '../model/db'
import { fuzzyMatch, getCharCodes } from '../util/fuzzy'
import workspace from '../workspace'
import Prompt from './prompt'
const logger = require('../util/logger')('list-history')

export default class InputHistory {
  private db: DB
  private index = -1
  private loaded: string[] = []
  private current: string[] = []
  private historyInput: string
  private key: string

  constructor(
    private prompt: Prompt,
    private name: string
  ) {
    this.db = workspace.createDatabase(`list-${name}-history`)
    this.key = Buffer.from(workspace.cwd).toString('base64')
  }

  public filter(): void {
    let { input } = this.prompt
    if (input == this.curr) return
    this.historyInput = ''
    let codes = getCharCodes(input)
    this.current = this.loaded.filter(s => fuzzyMatch(codes, s))
    this.index = -1
  }

  public get curr(): string | null {
    return this.index == -1 ? null : this.current[this.index]
  }

  public load(input: string): void {
    let { db } = this
    input = input || ''
    let arr = db.fetch(this.key)
    if (!arr || !Array.isArray(arr)) {
      this.loaded = []
    } else {
      this.loaded = arr
    }
    this.index = -1
    this.current = this.loaded.filter(s => s.startsWith(input))
  }

  public add(): void {
    let { loaded, db, prompt } = this
    let { input } = prompt
    if (!input || input.length < 2 || input == this.historyInput) return
    let idx = loaded.indexOf(input)
    if (idx != -1) loaded.splice(idx, 1)
    loaded.push(input)
    if (loaded.length > 200) {
      loaded = loaded.slice(-200)
    }
    db.push(this.key, loaded)
  }

  public previous(): void {
    let { current, index } = this
    if (!current || !current.length) return
    if (index <= 0) {
      this.index = current.length - 1
    } else {
      this.index = index - 1
    }
    this.historyInput = this.prompt.input = current[this.index] || ''
  }

  public next(): void {
    let { current, index } = this
    if (!current || !current.length) return
    if (index == current.length - 1) {
      this.index = 0
    } else {
      this.index = index + 1
    }
    this.historyInput = this.prompt.input = current[this.index] || ''
  }
}
