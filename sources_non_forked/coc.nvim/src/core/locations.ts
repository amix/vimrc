'use strict'
import { Neovim } from '@chemzqm/neovim'
import { Disposable, Location } from 'vscode-languageserver-protocol'
import Configurations from '../configuration'
import { Env } from '../types'
import { disposeAll } from '../util'
import ContentProvider from './contentProvider'
import Documents from './documents'
const logger = require('../util/logger')('core-locations')

export default class Locations implements Disposable {
  private nvim: Neovim
  private env: Env
  private disposables: Disposable[] = []
  constructor(
    private configurations: Configurations,
    private documents: Documents,
    private contentProvider: ContentProvider
  ) {
  }

  public attach(nvim: Neovim, env: Env): void {
    this.nvim = nvim
    this.env = env
  }

  /**
   * Populate locations to UI.
   */
  public async showLocations(locations: Location[]): Promise<void> {
    let { documents, nvim, env, configurations } = this
    let items = await documents.getQuickfixList(locations)
    const preferences = configurations.getConfiguration('coc.preferences')
    if (preferences.get<boolean>('useQuickfixForLocations', false)) {
      let openCommand = await nvim.getVar('coc_quickfix_open_command') as string
      if (typeof openCommand != 'string') {
        openCommand = items.length < 10 ? `copen ${items.length}` : 'copen'
      }
      nvim.pauseNotification()
      nvim.call('setqflist', [items], true)
      nvim.command(openCommand, true)
      nvim.resumeNotification(false, true)
    } else {
      await nvim.setVar('coc_jump_locations', items)
      if (env.locationlist) {
        nvim.command('CocList --normal --auto-preview location', true)
      } else {
        nvim.call('coc#util#do_autocmd', ['CocLocationsChange'], true)
      }
    }
  }

  public dispose(): void {
    disposeAll(this.disposables)
  }
}
