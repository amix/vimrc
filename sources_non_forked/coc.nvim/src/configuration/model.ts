'use strict'
import { IConfigurationModel } from '../types'
import { objectLiteral } from '../util/is'
import { deepClone } from '../util/object'
import { addToValueTree, getConfigurationValue, removeFromValueTree } from './util'

export class ConfigurationModel implements IConfigurationModel {

  constructor(private _contents: any = {}) { }

  public get contents(): any {
    return this._contents
  }

  public clone(): ConfigurationModel {
    return new ConfigurationModel(deepClone(this._contents))
  }

  public getValue<V>(section: string): V {
    let res = section
      ? getConfigurationValue<any>(this.contents, section)
      : this.contents
    return res
  }

  public merge(...others: ConfigurationModel[]): ConfigurationModel {
    const contents = deepClone(this.contents)

    for (const other of others) {
      this.mergeContents(contents, other.contents)
    }
    return new ConfigurationModel(contents)
  }

  public freeze(): ConfigurationModel {
    if (!Object.isFrozen(this._contents)) {
      Object.freeze(this._contents)
    }
    return this
  }

  private mergeContents(source: any, target: any): void {
    for (const key of Object.keys(target)) {
      if (key in source) {
        if (objectLiteral(source[key]) && objectLiteral(target[key])) {
          this.mergeContents(source[key], target[key])
          continue
        }
      }
      source[key] = deepClone(target[key])
    }
  }

  // Update methods

  public setValue(key: string, value: any): void {
    addToValueTree(this.contents, key, value, message => {
      console.error(message)
    })
  }

  public removeValue(key: string): void {
    removeFromValueTree(this.contents, key)
  }
}
