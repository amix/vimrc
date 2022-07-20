'use strict'
import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { URI } from 'vscode-uri'
import fs from 'fs-extra'
import { ListContext, ListItem } from '../../types'
import { statAsync } from '../../util/fs'
import workspace from '../../workspace'
import window from '../../window'
import BasicList from '../basic'

export default class FoldList extends BasicList {
  public defaultAction = 'edit'
  public description = 'list of current workspace folders'
  public name = 'folders'

  constructor(nvim: Neovim) {
    super(nvim)

    this.addAction('edit', async item => {
      let newPath = await nvim.call('input', ['Folder: ', item.label, 'dir'])
      let stat = await statAsync(newPath)
      if (!stat || !stat.isDirectory()) {
        window.showMessage(`invalid path: ${newPath}`, 'error')
        return
      }
      workspace.workspaceFolderControl.renameWorkspaceFolder(item.label, newPath)
    })

    this.addAction('delete', async item => {
      workspace.workspaceFolderControl.removeWorkspaceFolder(item.label)
    }, { reload: true, persist: true })

    this.addAction('newfile', async (item, context) => {
      let file = await window.requestInput('File name', item.label + '/')
      if (!file) return
      let dir = path.dirname(file)
      let stat = await statAsync(dir)
      if (!stat || !stat.isDirectory()) {
        fs.mkdirpSync(dir)
      }
      await workspace.createFile(file, { overwrite: false, ignoreIfExists: true })
      await this.jumpTo(URI.file(file).toString(), null, context)
    })
  }

  public async loadItems(_context: ListContext): Promise<ListItem[]> {
    return workspace.folderPaths.map(p => ({ label: p }))
  }
}
