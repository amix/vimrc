'use strict'
/* ---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

export class SnippetString {
  public static isSnippetString(thing: any): thing is SnippetString {
    if (thing instanceof SnippetString) {
      return true
    }
    if (!thing) {
      return false
    }
    return typeof thing.value === 'string'
  }

  private static _escape(value: string): string {
    return value.replace(/\$|}|\\/g, '\\$&')
  }

  private _tabstop = 1

  public value: string

  constructor(value?: string) {
    this.value = value || ''
  }

  public appendText(str: string): SnippetString {
    this.value += SnippetString._escape(str)
    return this
  }

  public appendTabstop(num: number = this._tabstop++): SnippetString {
    this.value += '$'
    this.value += num
    return this
  }

  public appendPlaceholder(
    value: string | ((snippet: SnippetString) => any),
    num: number = this._tabstop++
  ): SnippetString {
    if (typeof value === 'function') {
      const nested = new SnippetString()
      nested._tabstop = this._tabstop
      value(nested)
      this._tabstop = nested._tabstop
      value = nested.value
    } else {
      value = SnippetString._escape(value)
    }

    this.value += '${'
    this.value += num
    this.value += ':'
    this.value += value
    this.value += '}'

    return this
  }

  public appendChoice(
    values: string[],
    num: number = this._tabstop++
  ): SnippetString {
    const value = values.map(s => s.replace(/\$|}|\\|,/g, '\\$&')).join(',')

    this.value += '${'
    this.value += num
    this.value += '|'
    this.value += value
    this.value += '|}'

    return this
  }

  public appendVariable(
    name: string,
    defaultValue?: string | ((snippet: SnippetString) => any)
  ): SnippetString {
    if (typeof defaultValue === 'function') {
      const nested = new SnippetString()
      nested._tabstop = this._tabstop
      defaultValue(nested)
      this._tabstop = nested._tabstop
      defaultValue = nested.value
    } else if (typeof defaultValue === 'string') {
      defaultValue = defaultValue.replace(/\$|}/g, '\\$&')
    }

    this.value += '${'
    this.value += name
    if (defaultValue) {
      this.value += ':'
      this.value += defaultValue
    }
    this.value += '}'

    return this
  }
}
