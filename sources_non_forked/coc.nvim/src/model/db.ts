'use strict'
import fs from 'fs-extra'
import path from 'path'

export default class DB {
  constructor(public readonly filepath: string) {
  }

  /**
   * Get data by key.
   *
   * @param {string} key unique key allows dot notation.
   * @returns {any}
   */
  public fetch(key: string): any {
    let obj = this.load()
    if (!key) return obj
    let parts = key.split('.')
    for (let part of parts) {
      if (typeof obj[part] == 'undefined') {
        return undefined
      }
      obj = obj[part]
    }
    return obj
  }

  /**
   * Check if key exists
   *
   * @param {string} key unique key allows dot notation.
   */
  public exists(key: string): boolean {
    let obj = this.load()
    let parts = key.split('.')
    for (let part of parts) {
      if (typeof obj[part] == 'undefined') {
        return false
      }
      obj = obj[part]
    }
    return true
  }

  /**
   * Delete data by key
   *
   * @param {string} key unique key allows dot notation.
   */
  public delete(key: string): void {
    let obj = this.load()
    let origin = obj
    let parts = key.split('.')
    let len = parts.length
    for (let i = 0; i < len; i++) {
      if (typeof obj[parts[i]] == 'undefined') {
        break
      }
      if (i == len - 1) {
        delete obj[parts[i]]
        fs.writeFileSync(this.filepath, JSON.stringify(origin, null, 2), 'utf8')
        break
      }
      obj = obj[parts[i]]
    }
  }

  /**
   * Save data with key
   *
   * @param {string} key unique string that allows dot notation.
   * @param {number|null|boolean|string|{[index} data saved data.
   */
  public push(key: string, data: number | null | boolean | string | { [index: string]: any }): void {
    let origin = this.load() || {}
    let obj = origin
    let parts = key.split('.')
    let len = parts.length
    if (obj == null) {
      let dir = path.dirname(this.filepath)
      fs.mkdirpSync(dir)
      obj = origin
    }
    for (let i = 0; i < len; i++) {
      let key = parts[i]
      if (i == len - 1) {
        obj[key] = data
        fs.writeFileSync(this.filepath, JSON.stringify(origin, null, 2))
        break
      }
      if (typeof obj[key] == 'undefined') {
        obj[key] = {}
        obj = obj[key]
      } else {
        obj = obj[key]
      }
    }
  }

  private load(): any {
    let dir = path.dirname(this.filepath)
    let stat = fs.statSync(dir)
    if (!stat || !stat.isDirectory()) {
      fs.mkdirpSync(dir)
      fs.writeFileSync(this.filepath, '{}', 'utf8')
      return {}
    }
    try {
      let content = fs.readFileSync(this.filepath, 'utf8')
      return JSON.parse(content.trim())
    } catch (e) {
      fs.writeFileSync(this.filepath, '{}', 'utf8')
      return {}
    }
  }

  /**
   * Empty db file.
   */
  public clear(): void {
    let stat = fs.statSync(this.filepath)
    if (!stat || !stat.isFile()) return
    fs.writeFileSync(this.filepath, '{}', 'utf8')
  }

  /**
   * Remove db file.
   */
  public destroy(): void {
    if (fs.existsSync(this.filepath)) {
      fs.unlinkSync(this.filepath)
    }
  }
}
