import { Neovim } from '@chemzqm/neovim'
import manager from '../../list/manager'
import { parseInput } from '../../list/worker'
import helper from '../helper'
import { ListContext, ListTask, ListItem } from '../../types'
import { CancellationToken, Disposable } from 'vscode-languageserver-protocol'
import { EventEmitter } from 'events'
import colors from 'colors/safe'
import BasicList from '../../list/basic'
import { disposeAll } from '../../util'

let items: ListItem[] = []

class DataList extends BasicList {
  public name = 'data'
  public loadItems(): Promise<ListItem[]> {
    return Promise.resolve(items)
  }
}

class EmptyList extends BasicList {
  public name = 'empty'
  public loadItems(): Promise<ListItem[]> {
    let emitter: any = new EventEmitter()
    setTimeout(() => {
      emitter.emit('end')
    }, 20)
    return emitter
  }
}

class IntervalTaskList extends BasicList {
  public name = 'task'
  public timeout = 3000
  public loadItems(_context: ListContext, token: CancellationToken): Promise<ListTask> {
    let emitter: any = new EventEmitter()
    let i = 0
    let interval = setInterval(() => {
      emitter.emit('data', { label: i.toFixed() })
      i++
    }, 50)
    emitter.dispose = () => {
      clearInterval(interval)
      emitter.emit('end')
    }
    token.onCancellationRequested(() => {
      emitter.dispose()
    })
    return emitter
  }
}

class DelayTask extends BasicList {
  public name = 'delay'
  public interactive = true
  public loadItems(_context: ListContext, token: CancellationToken): Promise<ListTask> {
    let emitter: any = new EventEmitter()
    let disposed = false
    setTimeout(() => {
      if (disposed) return
      emitter.emit('data', { label: 'ahead' })
    }, 100)
    setTimeout(() => {
      if (disposed) return
      emitter.emit('data', { label: 'abort' })
    }, 200)
    emitter.dispose = () => {
      disposed = true
      emitter.emit('end')
    }
    token.onCancellationRequested(() => {
      emitter.dispose()
    })
    return emitter
  }
}

class InteractiveList extends BasicList {
  public name = 'test'
  public interactive = true
  public loadItems(context: ListContext, _token: CancellationToken): Promise<ListItem[]> {
    return Promise.resolve([{
      label: colors.magenta(context.input || '')
    }])
  }
}

class ErrorList extends BasicList {
  public name = 'error'
  public interactive = true
  public loadItems(_context: ListContext, _token: CancellationToken): Promise<ListItem[]> {
    return Promise.reject(new Error('test error'))
  }
}

class ErrorTaskList extends BasicList {
  public name = 'task'
  public loadItems(_context: ListContext, _token: CancellationToken): Promise<ListTask> {
    let emitter: any = new EventEmitter()
    let timeout = setTimeout(() => {
      emitter.emit('error', new Error('task error'))
    }, 100)
    emitter.dispose = () => {
      clearTimeout(timeout)
    }
    return emitter
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

describe('parseInput', () => {
  it('should parse input with space', async () => {
    let res = parseInput('a b')
    expect(res).toEqual(['a', 'b'])
    res = parseInput('a b ')
    expect(res).toEqual(['a', 'b'])
  })

  it('should parse input with escaped space', async () => {
    let res = parseInput('a\\ b')
    expect(res).toEqual(['a b'])
  })
})

describe('list worker', () => {

  it('should work with long running task', async () => {
    disposables.push(manager.registerList(new IntervalTaskList(nvim)))
    await manager.start(['task'])
    await manager.session.ui.ready
    await helper.wait(200)
    let len = manager.session?.length
    expect(len > 2).toBe(true)
    await manager.cancel()
  })

  it('should sort by sortText', async () => {
    items = [{
      label: 'abc',
      sortText: 'b'
    }, {
      label: 'ade',
      sortText: 'a'
    }]
    disposables.push(manager.registerList(new DataList(nvim)))
    await manager.start(['data'])
    await manager.session.ui.ready
    await nvim.input('a')
    await helper.wait(50)
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual(['ade', 'abc'])
    await manager.cancel()
  })

  it('should show empty line for empty task', async () => {
    disposables.push(manager.registerList(new EmptyList(nvim)))
    await manager.start(['empty'])
    await manager.session.ui.ready
    let line = await nvim.call('getline', [1])
    expect(line).toMatch('No results')
  })

  it('should cancel task by use CancellationToken', async () => {
    disposables.push(manager.registerList(new IntervalTaskList(nvim)))
    await manager.start(['task'])
    expect(manager.session?.worker.isLoading).toBe(true)
    await helper.wait(100)
    manager.session?.stop()
    expect(manager.session?.worker.isLoading).toBe(false)
  })

  it('should render slow interactive list', async () => {
    disposables.push(manager.registerList(new DelayTask(nvim)))
    await manager.start(['delay'])
    await nvim.input('a')
    await helper.wait(600)
    let buf = await nvim.buffer
    let lines = await buf.lines
    expect(lines).toEqual(['ahead', 'abort'])
  })

  it('should work with interactive list', async () => {
    disposables.push(manager.registerList(new InteractiveList(nvim)))
    await manager.start(['-I', 'test'])
    await manager.session?.ui.ready
    expect(manager.isActivated).toBe(true)
    await nvim.eval('feedkeys("f", "in")')
    await helper.wait(100)
    await nvim.eval('feedkeys("a", "in")')
    await helper.wait(100)
    await nvim.eval('feedkeys("x", "in")')
    await helper.wait(300)
    let item = await manager.session?.ui.item
    expect(item.label).toBe('fax')
  })

  it('should not activate on load error', async () => {
    disposables.push(manager.registerList(new ErrorList(nvim)))
    await manager.start(['test'])
    expect(manager.isActivated).toBe(false)
  })

  it('should deactivate on task error', async () => {
    disposables.push(manager.registerList(new ErrorTaskList(nvim)))
    await manager.start(['task'])
    await helper.wait(300)
    expect(manager.isActivated).toBe(false)
  })
})
