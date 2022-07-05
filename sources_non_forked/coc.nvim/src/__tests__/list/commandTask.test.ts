import { Neovim } from '@chemzqm/neovim'
import path from 'path'
import { ListContext, ListTask } from '../../types'
import manager from '../../list/manager'
import helper, { createTmpFile } from '../helper'
import BasicList from '../../list/basic'
import { Disposable } from 'vscode-languageserver-protocol'
import { disposeAll } from '../../util'

class DataList extends BasicList {
  public name = 'data'
  public async loadItems(_context: ListContext): Promise<ListTask> {
    let fsPath = await createTmpFile(`console.log('foo');console.log('');console.log('bar');`)
    return this.createCommandTask({
      cmd: 'node',
      args: [fsPath],
      cwd: path.dirname(fsPath),
      onLine: line => {
        if (!line) return undefined
        return {
          label: line
        }
      }
    })
  }
}

class SleepList extends BasicList {
  public name = 'sleep'
  public loadItems(_context: ListContext): Promise<ListTask> {
    return Promise.resolve(this.createCommandTask({
      cmd: 'sleep',
      args: ['10'],
      onLine: line => {
        return {
          label: line
        }
      }
    }))
  }
}

class StderrList extends BasicList {
  public name = 'stderr'
  public async loadItems(_context: ListContext): Promise<ListTask> {
    let fsPath = await createTmpFile(`console.error('stderr');console.log('stdout')`)
    return Promise.resolve(this.createCommandTask({
      cmd: 'node',
      args: [fsPath],
      cwd: path.dirname(fsPath),
      onLine: line => {
        return {
          label: line
        }
      }
    }))
  }
}

class ErrorTask extends BasicList {
  public name = 'error'
  public async loadItems(_context: ListContext): Promise<ListTask> {
    return Promise.resolve(this.createCommandTask({
      cmd: 'NOT_EXISTS',
      args: [],
      cwd: __dirname,
      onLine: line => {
        return {
          label: line
        }
      }
    }))
  }
}

let nvim: Neovim
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterAll(async () => {
  await helper.shutdown()
})

afterEach(async () => {
  disposeAll(disposables)
  manager.reset()
  await helper.reset()
})

describe('Command task', () => {
  it('should not show stderr', async () => {
    disposables.push(manager.registerList(new StderrList(nvim)))
    await manager.start(['stderr'])
    await manager.session.ui.ready
    let lines = await nvim.call('getline', [1, '$']) as string[]
    expect(lines).toEqual(['stdout'])
  })

  it('should show error for bad key', async () => {
    let list = new DataList(nvim)
    list.config.fixKey('<X-a>')
    await helper.wait(200)
    await nvim.command('redraw')
    let msg = await helper.getCmdline()
    expect(msg).toMatch('not supported')
  })

  it('should not show error', async () => {
    disposables.push(manager.registerList(new ErrorTask(nvim)))
    await manager.start(['error'])
    await helper.wait(300)
    await nvim.command('redraw')
    let len = manager.session.ui.length
    expect(len).toBe(0)
  })

  it('should create command task', async () => {
    let list = new DataList(nvim)
    disposables.push(manager.registerList(list))
    await manager.start(['data'])
    await manager.session.ui.ready
    await helper.wait(100)
    let lines = await nvim.call('getline', [1, '$']) as string[]
    expect(lines).toEqual(['foo', 'bar'])
  })

  it('should stop command task', async () => {
    let list = new SleepList(nvim)
    disposables.push(manager.registerList(list))
    await manager.start(['sleep'])
    manager.session.stop()
  })
})
