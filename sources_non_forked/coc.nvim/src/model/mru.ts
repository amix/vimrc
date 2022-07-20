'use strict'
import path from 'path'
import fs from 'fs-extra'
import { readFileLines, writeFile } from '../util/fs'
import { distinct } from '../util/array'

/**
 * Mru - manage string items as lines in mru file.
 */
export default class Mru {
  private file: string

  /**
   * @param {string} name unique name
   * @param {string} base? optional directory name, default to config root of coc.nvim
   */
  constructor(
    name: string,
    base?: string,
    private maximum = 5000) {
    this.file = path.join(base || process.env.COC_DATA_HOME, name)
    let dir = path.dirname(this.file)
    fs.mkdirpSync(dir)
  }

  /**
   * Load iems from mru file
   */
  public async load(): Promise<string[]> {
    try {
      let lines = await readFileLines(this.file, 0, this.maximum)
      if (lines.length > this.maximum) {
        await writeFile(this.file, lines.join('\n'))
      }
      if (lines[lines.length - 1] == '') lines = lines.slice(0, -1)
      return distinct(lines)
    } catch (e) {
      return []
    }
  }

  public loadSync(): string[] {
    if (!fs.existsSync(this.file)) return []
    try {
      let content = fs.readFileSync(this.file, 'utf8')
      content = content.trim()
      return content.length ? content.trim().split('\n') : []
    } catch (e) {
      return []
    }
  }

  /**
   * Add item to mru file.
   */
  public async add(item: string): Promise<void> {
    let buf: Buffer
    try {
      buf = fs.readFileSync(this.file)
      if (buf[0] === 239 && buf[1] === 187 && buf[2] === 191) {
        buf = buf.slice(3)
      }
      buf = Buffer.concat([Buffer.from(item, 'utf8'), new Uint8Array([10]), buf])
    } catch (e) {
      buf = Buffer.concat([Buffer.from(item, 'utf8'), new Uint8Array([10])])
    }
    await fs.writeFile(this.file, buf, 'utf8')
  }

  /**
   * Remove item from mru file.
   */
  public async remove(item: string): Promise<void> {
    let items = await this.load()
    let len = items.length
    items = items.filter(s => s != item)
    if (items.length != len) {
      await fs.writeFile(this.file, items.join('\n'), 'utf8')
    }
  }

  /**
   * Remove the data file.
   */
  public async clean(): Promise<void> {
    try {
      await fs.unlink(this.file)
    } catch (e) {
      // noop
    }
  }
}
