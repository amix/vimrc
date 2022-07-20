'use strict'
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
import { ClientCapabilities, ConfigurationRequest } from 'vscode-languageserver-protocol'
import workspace from '../workspace'
import { BaseLanguageClient, StaticFeature } from './client'
const logger = require('../util/logger')('languageclient-configuration')

export interface ConfigurationWorkspaceMiddleware {
  configuration?: ConfigurationRequest.MiddlewareSignature
}

export class ConfigurationFeature implements StaticFeature {
  private languageserverSection: string | undefined
  constructor(private _client: BaseLanguageClient) {
    let section = this._client.clientOptions.synchronize?.configurationSection
    if (typeof section === 'string' && section.startsWith('languageserver.')) {
      this.languageserverSection = section
    }
  }

  public fillClientCapabilities(capabilities: ClientCapabilities): void {
    capabilities.workspace = capabilities.workspace || {}
    capabilities.workspace.configuration = true
  }

  public initialize(): void {
    let client = this._client
    client.onRequest(ConfigurationRequest.type, (params, token) => {
      let configuration: ConfigurationRequest.HandlerSignature = params => {
        let result: any[] = []
        for (let item of params.items) {
          result.push(this.getConfiguration(item.scopeUri, item.section))
        }
        return result
      }
      let middleware = client.clientOptions.middleware.workspace
      return middleware && middleware.configuration
        ? middleware.configuration(params, token, configuration)
        : configuration(params, token)
    })
  }

  private getConfiguration(
    resource: string | undefined,
    section: string | undefined
  ): any {
    let result: any = null
    if (section) {
      if (this.languageserverSection) {
        section = `${this.languageserverSection}.${section}`
      }
      let index = section.lastIndexOf('.')
      if (index === -1) {
        result = toJSONObject(workspace.getConfiguration(undefined, resource).get(section))
      } else {
        let config = workspace.getConfiguration(section.substr(0, index), resource)
        if (config) {
          result = toJSONObject(config.get(section.substr(index + 1)))
        }
      }
    } else {
      let config = workspace.getConfiguration(this.languageserverSection, resource)
      result = {}
      for (let key of Object.keys(config)) {
        if (config.has(key)) {
          result[key] = toJSONObject(config.get(key))
        }
      }
    }
    return result
  }

  public dispose(): void {
  }
}

export function toJSONObject(obj: any): any {
  if (obj) {
    if (Array.isArray(obj)) {
      return obj.map(toJSONObject)
    } else if (typeof obj === 'object') {
      const res = Object.create(null)
      for (const key in obj) {
        if (Object.prototype.hasOwnProperty.call(obj, key)) {
          res[key] = toJSONObject(obj[key])
        }
      }
      return res
    }
  }
  return obj
}
