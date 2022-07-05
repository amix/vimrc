'use strict'
import fs from 'fs'
import { applyEdits, modify } from 'jsonc-parser'
import os from 'os'
import path from 'path'
import { WorkspaceFolder } from 'vscode-languageserver-protocol'
import { FormattingOptions } from 'vscode-languageserver-types'
import { URI } from 'vscode-uri'
import { ConfigurationShape, ConfigurationTarget, IConfigurationOverrides } from '../types'
import { CONFIG_FILE_NAME } from '../util'
import { sameFile } from '../util/fs'
const logger = require('../util/logger')('configuration-shape')

interface IFolderController {
  root?: string
  getWorkspaceFolder?: (resource: string) => WorkspaceFolder
}

export default class ConfigurationProxy implements ConfigurationShape {

  constructor(private resolver: IFolderController) {
  }

  public modifyConfiguration(uri: URI | undefined, key: string, value?: any): void {
    if (!uri || uri.scheme !== 'file') return
    logger.info('modify configuration file:', uri.fsPath)
    let file = uri.fsPath
    let dir = path.dirname(file)
    let formattingOptions: FormattingOptions = { tabSize: 2, insertSpaces: true }
    if (!fs.existsSync(dir)) fs.mkdirSync(dir, { recursive: true })
    let content = fs.readFileSync(file, { encoding: 'utf8', flag: 'a+' })
    content = content || '{}'
    let edits = modify(content, [key], value, { formattingOptions })
    content = applyEdits(content, edits)
    fs.writeFileSync(file, content, 'utf8')
  }

  public getWorkspaceConfig(resource?: string): URI | undefined {
    let folder: string
    if (resource) {
      if (typeof this.resolver.getWorkspaceFolder === 'function') {
        let workspaceFolder = this.resolver.getWorkspaceFolder(resource)
        if (workspaceFolder) folder = URI.parse(workspaceFolder.uri).fsPath
      }
    } else {
      folder = this.resolver.root
    }
    if (folder && !sameFile(folder, os.homedir())) {
      return URI.file(path.join(folder, '.vim', CONFIG_FILE_NAME))
    }
    return undefined
  }

  public $updateConfigurationOption(_target: ConfigurationTarget, key: string, value: any, overrides?: IConfigurationOverrides): void {
    this.modifyConfiguration(overrides?.resource, key, value)
  }

  public $removeConfigurationOption(_target: ConfigurationTarget, key: string, overrides?: IConfigurationOverrides): void {
    this.modifyConfiguration(overrides?.resource, key)
  }
}
