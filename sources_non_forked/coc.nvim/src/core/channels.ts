'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import { URI } from 'vscode-uri'
import events from '../events'
import BufferChannel from '../model/outputChannel'
import { TextDocumentContentProvider } from '../provider'
import { OutputChannel } from '../types'
const logger = require('../util/logger')('core-channels')

export class Channels {
  private outputChannels: Map<string, BufferChannel> = new Map()
  private bufnrs: Map<number, string> = new Map()
  private disposable: Disposable
  constructor() {
    this.disposable = events.on('BufUnload', bufnr => {
      let name = this.bufnrs.get(bufnr)
      if (name) {
        let channel = this.outputChannels.get(name)
        if (channel) channel.created = false
      }
    })
  }

  /**
   * Get text document provider
   */
  public getProvider(nvim: Neovim): TextDocumentContentProvider {
    let provider: TextDocumentContentProvider = {
      onDidChange: null,
      provideTextDocumentContent: async (uri: URI) => {
        let channel = this.get(uri.path.slice(1))
        if (!channel) return ''
        nvim.pauseNotification()
        nvim.call('bufnr', ['%'], true)
        nvim.command('setlocal nospell nofoldenable nowrap noswapfile', true)
        nvim.command('setlocal buftype=nofile bufhidden=hide', true)
        nvim.command('setfiletype log', true)
        let res = await nvim.resumeNotification()
        this.bufnrs.set(res[0][0] as number, channel.name)
        channel.created = true
        return channel.content
      }
    }
    return provider
  }

  public get names(): string[] {
    return Array.from(this.outputChannels.keys())
  }

  public get(channelName: string): BufferChannel | null {
    return this.outputChannels.get(channelName)
  }

  public create(name: string, nvim: Neovim): OutputChannel | null {
    if (this.outputChannels.has(name)) return this.outputChannels.get(name)
    let channel = new BufferChannel(name, nvim, () => {
      this.outputChannels.delete(name)
    })
    this.outputChannels.set(name, channel)
    return channel
  }

  public show(name: string, cmd: string, preserveFocus?: boolean): void {
    let channel = this.outputChannels.get(name)
    if (!channel) return
    channel.show(preserveFocus, cmd)
  }

  public dispose(): void {
    this.disposable.dispose()
    for (let channel of this.outputChannels.values()) {
      channel.dispose()
    }
    this.outputChannels.clear()
  }
}

export default new Channels()
