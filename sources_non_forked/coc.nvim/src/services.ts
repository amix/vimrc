'use strict'
import { SpawnOptions } from 'child_process'
import { EventEmitter } from 'events'
import fs from 'fs'
import net from 'net'
import { CancellationToken, CancellationTokenSource, Disposable, DocumentSelector, Emitter, Event } from 'vscode-languageserver-protocol'
import { TextDocument } from 'vscode-languageserver-textdocument'
import { Executable, ForkOptions, LanguageClient, LanguageClientOptions, RevealOutputChannelOn, ServerOptions, State, Transport, TransportKind } from './language-client'
import { ServiceStat } from './types'
import { disposeAll, wait } from './util'
import window from './window'
import workspace from './workspace'
const logger = require('./util/logger')('services')

interface ServiceInfo {
  id: string
  state: string
  languageIds: string[]
}

export interface LanguageServerConfig {
  module?: string
  command?: string
  transport?: string
  transportPort?: number
  disableSnippetCompletion?: boolean
  disableDynamicRegister?: boolean
  disabledFeatures?: string[]
  formatterPriority?: number
  filetypes: string[]
  additionalSchemes?: string[]
  enable?: boolean
  args?: string[]
  cwd?: string
  env?: any
  // socket port
  port?: number
  host?: string
  detached?: boolean
  shell?: boolean
  execArgv?: string[]
  rootPatterns?: string[]
  ignoredRootPaths?: string[]
  initializationOptions?: any
  progressOnInitialization?: boolean
  revealOutputChannelOn?: string
  configSection?: string
  stdioEncoding?: string
  runtime?: string
}

export function getStateName(state: ServiceStat): string {
  switch (state) {
    case ServiceStat.Initial:
      return 'init'
    case ServiceStat.Running:
      return 'running'
    case ServiceStat.Starting:
      return 'starting'
    case ServiceStat.StartFailed:
      return 'startFailed'
    case ServiceStat.Stopping:
      return 'stopping'
    case ServiceStat.Stopped:
      return 'stopped'
    default:
      return 'unknown'
  }
}

export interface IServiceProvider {
  // unique service id
  id: string
  name: string
  client?: LanguageClient
  selector: DocumentSelector
  // current state
  state: ServiceStat
  start(): Promise<void>
  dispose(): void
  stop(): Promise<void> | void
  restart(): Promise<void> | void
  onServiceReady: Event<void>
}

export class ServiceManager extends EventEmitter implements Disposable {
  private readonly registered: Map<string, IServiceProvider> = new Map()
  private disposables: Disposable[] = []

  public init(): void {
    workspace.onDidOpenTextDocument(document => {
      this.start(document)
    }, null, this.disposables)
    workspace.onDidChangeConfiguration(e => {
      if (e.affectsConfiguration('languageserver')) {
        this.createCustomServices()
      }
    }, null, this.disposables)
    this.createCustomServices()
  }

  public dispose(): void {
    this.removeAllListeners()
    disposeAll(this.disposables)
    for (let service of this.registered.values()) {
      service.dispose()
    }
  }

  public regist(service: IServiceProvider): Disposable {
    let { id } = service
    if (!id) logger.error('invalid service configuration. ', service.name)
    if (this.registered.get(id)) return
    this.registered.set(id, service)
    logger.info(`registered service "${id}"`)
    if (this.shouldStart(service)) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      service.start()
    }
    if (service.state == ServiceStat.Running) {
      this.emit('ready', id)
    }
    service.onServiceReady(() => {
      logger.info(`service ${id} started`)
      this.emit('ready', id)
    }, null, this.disposables)
    return Disposable.create(() => {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      service.stop()
      service.dispose()
      this.registered.delete(id)
    })
  }

  public getService(id: string): IServiceProvider {
    let service = this.registered.get(id)
    if (!service) service = this.registered.get(`languageserver.${id}`)
    return service
  }

  private shouldStart(service: IServiceProvider): boolean {
    if (service.state != ServiceStat.Initial) {
      return false
    }
    let selector = service.selector
    for (let doc of workspace.documents) {
      if (workspace.match(selector, doc.textDocument)) {
        return true
      }
    }
    return false
  }

  private start(document: TextDocument): void {
    let services = this.getServices(document)
    for (let service of services) {
      if (service.state == ServiceStat.Initial) {
        // eslint-disable-next-line @typescript-eslint/no-floating-promises
        service.start()
      }
    }
  }

  public getServices(document: TextDocument): IServiceProvider[] {
    let res: IServiceProvider[] = []
    for (let service of this.registered.values()) {
      if (workspace.match(service.selector, document) > 0) {
        res.push(service)
      }
    }
    return res
  }

  public stop(id: string): Promise<void> {
    let service = this.registered.get(id)
    if (!service) {
      window.showMessage(`Service ${id} not found`, 'error')
      return
    }
    return Promise.resolve(service.stop())
  }

  public stopAll(): void {
    for (let service of this.registered.values()) {
      // eslint-disable-next-line @typescript-eslint/no-floating-promises
      service.stop()
    }
  }

  public async toggle(id: string): Promise<void> {
    let service = this.registered.get(id)
    if (!service) {
      window.showMessage(`Service ${id} not found`, 'error')
      return
    }
    let { state } = service
    try {
      if (state == ServiceStat.Running) {
        await Promise.resolve(service.stop())
      } else if (state == ServiceStat.Initial) {
        await service.start()
      } else if (state == ServiceStat.Stopped) {
        await service.restart()
      }
    } catch (e) {
      window.showMessage(`Service error: ${e}`, 'error')
    }
  }

  public getServiceStats(): ServiceInfo[] {
    let res: ServiceInfo[] = []
    for (let [id, service] of this.registered) {
      res.push({
        id,
        languageIds: documentSelectorToLanguageIds(service.selector),
        state: getStateName(service.state)
      })
    }
    return res
  }

  private createCustomServices(): void {
    let lspConfig = workspace.getConfiguration().get<{ key: LanguageServerConfig }>('languageserver', {} as any)
    for (let key of Object.keys(lspConfig)) {
      let config: LanguageServerConfig = lspConfig[key]
      if (!this.validServerConfig(key, config)) {
        continue
      }
      this.registLanguageClient(key, config)
    }
  }

  private validServerConfig(key: string, config: LanguageServerConfig): boolean {
    let errors: string[] = []
    if (config.module != null && typeof config.module !== 'string') {
      errors.push(`"module" field of languageserver ${key} should be string`)
    }
    if (config.command != null && typeof config.command !== 'string') {
      errors.push(`"command" field of languageserver ${key} should be string`)
    }
    if (config.transport != null && typeof config.transport !== 'string') {
      errors.push(`"transport" field of languageserver ${key} should be string`)
    }
    if (config.transportPort != null && typeof config.transportPort !== 'number') {
      errors.push(`"transportPort" field of languageserver ${key} should be string`)
    }
    if (!Array.isArray(config.filetypes) || !config.filetypes.every(s => typeof s === 'string')) {
      errors.push(`"filetypes" field of languageserver ${key} should be array of string`)
    }
    if (config.additionalSchemes && (!Array.isArray(config.additionalSchemes) || config.additionalSchemes.some(s => typeof s !== 'string'))) {
      errors.push(`"additionalSchemes" field of languageserver ${key} should be array of string`)
    }
    if (errors.length) {
      window.showMessage(errors.join('\n'), 'error')
      return false
    }
    return true
  }

  private waitClient(id: string): Promise<void> {
    let service = this.getService(id)
    if (service && service.state == ServiceStat.Running) return Promise.resolve()
    if (service) return new Promise(resolve => {
      service.onServiceReady(() => {
        resolve()
      })
    })
    return new Promise(resolve => {
      let listener = clientId => {
        if (clientId == id || clientId == `languageserver.${id}`) {
          this.off('ready', listener)
          resolve()
        }
      }
      this.on('ready', listener)
    })
  }

  public async registNotification(id: string, method: string): Promise<void> {
    await this.waitClient(id)
    let service = this.getService(id)
    if (!service.client) {
      window.showMessage(`Not a language client: ${id}`, 'error')
      return
    }
    let client = service.client
    client.onNotification(method, async result => {
      workspace.nvim.call('coc#do_notify', [id, method, result], true)
    })
  }

  public async sendNotification(id: string, method: string, params?: any): Promise<void> {
    if (!method) throw new Error(`method required for ontification`)
    let service = this.getService(id)
    // wait for extension activate
    if (!service || !service.client) throw new Error(`Language server ${id} not found`)
    if (service.state == ServiceStat.Starting) {
      await service.client.onReady()
    }
    if (service.state != ServiceStat.Running) {
      throw new Error(`Language server ${id} not running`)
    }
    await Promise.resolve(service.client.sendNotification(method, params))
  }

  public async sendRequest(id: string, method: string, params?: any, token?: CancellationToken): Promise<any> {
    if (!method) throw new Error(`method required for sendRequest`)
    let service = this.getService(id)
    // wait for extension activate
    if (!service) await wait(100)
    service = this.getService(id)
    if (!service || !service.client) {
      throw new Error(`Language server ${id} not found`)
    }
    if (service.state == ServiceStat.Starting) {
      await service.client.onReady()
    }
    if (service.state != ServiceStat.Running) {
      throw new Error(`Language server ${id} not running`)
    }
    if (!token) {
      let tokenSource = new CancellationTokenSource()
      token = tokenSource.token
    }
    return await Promise.resolve(service.client.sendRequest(method, params, token))
  }

  public registLanguageClient(client: LanguageClient): Disposable
  public registLanguageClient(name: string, config: LanguageServerConfig): Disposable
  public registLanguageClient(name: string | LanguageClient, config?: LanguageServerConfig): Disposable {
    let id = typeof name === 'string' ? `languageserver.${name}` : name.id
    let disposables: Disposable[] = []
    let onDidServiceReady = new Emitter<void>()
    let client: LanguageClient | null = typeof name === 'string' ? null : name
    if (this.registered.has(id)) return
    let created = false
    let service: IServiceProvider = {
      id,
      client,
      name: typeof name === 'string' ? name : name.name,
      selector: typeof name === 'string' ? getDocumentSelector(config.filetypes, config.additionalSchemes) : name.clientOptions.documentSelector,
      state: ServiceStat.Initial,
      onServiceReady: onDidServiceReady.event,
      start: (): Promise<void> => {
        if (service.state == ServiceStat.Starting || service.state == ServiceStat.Running) {
          return
        }
        if (client && !client.needsStart()) {
          return
        }
        if (created && client) {
          client.restart()
          return Promise.resolve()
        }
        if (!created) {
          if (typeof name == 'string' && !client) {
            let config: LanguageServerConfig = workspace.getConfiguration().get<{ key: LanguageServerConfig }>('languageserver', {} as any)[name]
            if (!config || config.enable === false) return
            let opts = getLanguageServerOptions(id, name, config)
            if (!opts) return
            client = new LanguageClient(id, name, opts[1], opts[0])
            service.selector = opts[0].documentSelector
            service.client = client
          }
          client.onDidChangeState(changeEvent => {
            let { oldState, newState } = changeEvent
            if (newState == State.Starting) {
              service.state = ServiceStat.Starting
            } else if (newState == State.Running) {
              service.state = ServiceStat.Running
            } else if (newState == State.Stopped) {
              service.state = ServiceStat.Stopped
            }
            let oldStr = stateString(oldState)
            let newStr = stateString(newState)
            logger.info(`${client.name} state change: ${oldStr} => ${newStr}`)
          }, null, disposables)
          created = true
        }
        service.state = ServiceStat.Starting
        logger.debug(`starting service: ${id}`)
        let disposable = client.start()
        disposables.push(disposable)
        return new Promise(resolve => {
          client.onReady().then(() => {
            onDidServiceReady.fire(void 0)
            resolve()
          }, e => {
            window.showMessage(`Server ${id} failed to start: ${e}`, 'error')
            logger.error(`Server ${id} failed to start:`, e)
            service.state = ServiceStat.StartFailed
            resolve()
          })
        })
      },
      dispose: async () => {
        onDidServiceReady.dispose()
        disposeAll(disposables)
      },
      stop: async (): Promise<void> => {
        if (!client || !client.needsStop()) return
        await Promise.resolve(client.stop())
      },
      restart: async (): Promise<void> => {
        if (client) {
          service.state = ServiceStat.Starting
          client.restart()
        } else {
          await service.start()
        }
      },
    }
    return this.regist(service)
  }
}

export function documentSelectorToLanguageIds(documentSelector: DocumentSelector): string[] {
  let res = documentSelector.map(filter => {
    if (typeof filter == 'string') {
      return filter
    }
    return filter.language
  })
  res = res.filter(s => typeof s == 'string')
  return Array.from(new Set(res))
}

// convert config to options
export function getLanguageServerOptions(id: string, name: string, config: Readonly<LanguageServerConfig>): [LanguageClientOptions, ServerOptions] {
  let { command, module, port, args, filetypes } = config
  args = args || []
  if (!filetypes) {
    window.showMessage(`Wrong configuration of LS "${name}", filetypes not found`, 'error')
    return null
  }
  if (!command && !module && !port) {
    window.showMessage(`Wrong configuration of LS "${name}", no command or module specified.`, 'error')
    return null
  }
  let serverOptions: ServerOptions
  if (module) {
    module = workspace.expand(module)
    if (!fs.existsSync(module)) {
      window.showMessage(`Module file "${module}" not found for LS "${name}"`, 'error')
      return null
    }
    serverOptions = {
      module,
      runtime: config.runtime || process.execPath,
      args,
      transport: getTransportKind(config),
      options: getForkOptions(config)
    }
  } else if (command) {
    serverOptions = {
      command,
      args,
      options: getSpawnOptions(config)
    } as Executable
  } else if (port) {
    serverOptions = () => new Promise((resolve, reject) => {
      let client = new net.Socket()
      let host = config.host || '127.0.0.1'
      logger.info(`languageserver "${id}" connecting to ${host}:${port}`)
      client.connect(port, host, () => {
        resolve({
          reader: client,
          writer: client
        })
      })
      client.on('error', e => {
        reject(new Error(`Connection error for ${id}: ${e.message}`))
      })
    })
  }
  // compatible
  let disabledFeatures: string[] = Array.from(config.disabledFeatures || [])
  for (let key of ['disableWorkspaceFolders', 'disableCompletion', 'disableDiagnostics']) {
    if (config[key] === true) {
      let s = key.slice(7)
      disabledFeatures.push(s[0].toLowerCase() + s.slice(1))
    }
  }
  let disableSnippetCompletion = !!config.disableSnippetCompletion
  let ignoredRootPaths = config.ignoredRootPaths || []
  let clientOptions: LanguageClientOptions = {
    ignoredRootPaths: ignoredRootPaths.map(s => workspace.expand(s)),
    disableSnippetCompletion,
    disableDynamicRegister: !!config.disableDynamicRegister,
    disabledFeatures,
    formatterPriority: config.formatterPriority || 0,
    documentSelector: getDocumentSelector(config.filetypes, config.additionalSchemes),
    revealOutputChannelOn: getRevealOutputChannelOn(config.revealOutputChannelOn),
    synchronize: {
      configurationSection: `${id}.settings`
    },
    diagnosticCollectionName: name,
    outputChannelName: id,
    stdioEncoding: config.stdioEncoding || 'utf8',
    progressOnInitialization: config.progressOnInitialization === true,
    initializationOptions: config.initializationOptions || {}
  }
  return [clientOptions, serverOptions]
}

export function getRevealOutputChannelOn(revealOn: string | undefined): RevealOutputChannelOn {
  switch (revealOn) {
    case 'info':
      return RevealOutputChannelOn.Info
    case 'warn':
      return RevealOutputChannelOn.Warn
    case 'error':
      return RevealOutputChannelOn.Error
    case 'never':
      return RevealOutputChannelOn.Never
    default:
      return RevealOutputChannelOn.Never
  }
}

export function getDocumentSelector(filetypes: string[] | undefined, additionalSchemes?: string[]): DocumentSelector {
  let documentSelector: DocumentSelector = []
  let schemes = ['file', 'untitled'].concat(additionalSchemes || [])
  if (!filetypes) return schemes.map(s => ({ scheme: s }))
  filetypes.forEach(filetype => {
    documentSelector.push(...schemes.map(scheme => ({ language: filetype, scheme })))
  })
  return documentSelector
}

export function getTransportKind(config: LanguageServerConfig): Transport {
  let { transport, transportPort } = config
  if (!transport || transport == 'ipc') return TransportKind.ipc
  if (transport == 'stdio') return TransportKind.stdio
  if (transport == 'pipe') return TransportKind.pipe
  return { kind: TransportKind.socket, port: transportPort }
}

function getForkOptions(config: LanguageServerConfig): ForkOptions {
  return {
    cwd: config.cwd,
    execArgv: config.execArgv || [],
    env: config.env || undefined
  }
}

function getSpawnOptions(config: LanguageServerConfig): SpawnOptions {
  return {
    cwd: config.cwd,
    detached: !!config.detached,
    shell: !!config.shell,
    env: config.env || undefined
  }
}

function stateString(state: State): string {
  switch (state) {
    case State.Running:
      return 'running'
    case State.Starting:
      return 'starting'
    case State.Stopped:
      return 'stopped'
    default:
      return 'unknown'
  }
}

export default new ServiceManager()
