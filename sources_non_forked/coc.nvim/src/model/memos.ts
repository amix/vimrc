'use strict'
import fs from 'fs'
import { deepClone } from '../util/object'
const logger = require('../util/logger')('model-memos')

/**
 * A memento represents a storage utility. It can store and retrieve
 * values.
 */
export interface Memento {
  get<T>(key: string): T | undefined
  get<T>(key: string, defaultValue: T): T
  update(key: string, value: any): Promise<void>
}

export default class Memos {
  constructor(private filepath: string) {
    if (!fs.existsSync(filepath)) {
      fs.writeFileSync(filepath, '{}', 'utf8')
    }
  }

  private fetchContent(id: string, key: string): any {
    try {
      let content = fs.readFileSync(this.filepath, 'utf8')
      let res = JSON.parse(content)
      let obj = res[id]
      if (!obj) return undefined
      return obj[key]
    } catch (e) {
      return undefined
    }
  }

  private async update(id: string, key: string, value: any): Promise<void> {
    let { filepath } = this
    try {
      let content = fs.readFileSync(filepath, 'utf8')
      let current = content ? JSON.parse(content) : {}
      current[id] = current[id] || {}
      if (value !== undefined) {
        current[id][key] = deepClone(value)
      } else {
        delete current[id][key]
      }
      content = JSON.stringify(current, null, 2)
      fs.writeFileSync(filepath, content, 'utf8')
    } catch (e) {
      logger.error(`Error on update memos:`, e)
    }
  }

  public createMemento(id: string): Memento {
    return {
      get: <T>(key: string, defaultValue?: T): T | undefined => {
        let res = this.fetchContent(id, key)
        return res === undefined ? defaultValue : res
      },
      update: async (key: string, value: any): Promise<void> => {
        await this.update(id, key, value)
      }
    }
  }
}
