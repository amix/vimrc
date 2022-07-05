'use strict'
import { ConfigurationModel } from './model'
import { IConfigurationData } from '../types'
export class Configuration {
  private _consolidateConfiguration: ConfigurationModel

  constructor(
    private _defaultConfiguration: ConfigurationModel,
    private _userConfiguration: ConfigurationModel,
    private _workspaceConfiguration: ConfigurationModel,
    private _memoryConfiguration: ConfigurationModel = new ConfigurationModel(),
  ) {
  }

  private getConsolidateConfiguration(): ConfigurationModel {
    if (!this._consolidateConfiguration) {
      this._consolidateConfiguration = this._defaultConfiguration.merge(this._userConfiguration, this._workspaceConfiguration, this._memoryConfiguration)
      this._consolidateConfiguration = this._consolidateConfiguration.freeze()
    }
    return this._consolidateConfiguration
  }

  public getValue(section?: string): any {
    let configuration = this.getConsolidateConfiguration()
    return configuration.getValue(section)
  }

  public inspect<C>(key: string): {
    default: C
    user: C
    workspace: C
    memory?: C
    value: C
  } {
    const consolidateConfigurationModel = this.getConsolidateConfiguration()
    const { _workspaceConfiguration, _memoryConfiguration } = this
    return {
      default: this._defaultConfiguration.freeze().getValue(key),
      user: this._userConfiguration.freeze().getValue(key),
      workspace: _workspaceConfiguration.freeze().getValue(key),
      memory: _memoryConfiguration.freeze().getValue(key),
      value: consolidateConfigurationModel.getValue(key)
    }
  }

  public get defaults(): ConfigurationModel {
    return this._defaultConfiguration
  }

  public get user(): ConfigurationModel {
    return this._userConfiguration
  }

  public get workspace(): ConfigurationModel {
    return this._workspaceConfiguration
  }

  public toData(): IConfigurationData {
    return {
      defaults: {
        contents: this._defaultConfiguration.contents
      },
      user: {
        contents: this._userConfiguration.contents
      },
      workspace: {
        contents: this._workspaceConfiguration.contents
      }
    }
  }
}
