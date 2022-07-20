import { Neovim } from '@chemzqm/neovim'
import { Disposable } from 'vscode-languageserver-protocol'
import { disposeAll } from '../../util'
import workspace from '../../workspace'
import helper, { createTmpFile } from '../helper'

let nvim: Neovim
let disposables: Disposable[] = []
beforeAll(async () => {
  await helper.setup()
  nvim = helper.nvim
})

afterEach(() => {
  disposeAll(disposables)
})

afterAll(async () => {
  await helper.shutdown()
})

describe('task test', () => {
  it('should start task', async () => {
    let task = workspace.createTask('sleep')
    disposables.push(task)
    let started = await task.start({ cmd: 'sleep', args: ['50'] })
    expect(started).toBe(true)
  })

  it('should stop task', async () => {
    let task = workspace.createTask('sleep')
    disposables.push(task)
    await task.start({ cmd: 'sleep', args: ['50'] })
    await task.stop()
    let running = await task.running
    expect(running).toBe(false)
  })

  it('should emit exit event', async () => {
    let fn = jest.fn()
    let task = workspace.createTask('sleep')
    disposables.push(task)
    task.onExit(fn)
    await task.start({ cmd: 'sleep', args: ['50'] })
    await helper.wait(10)
    await task.stop()
    expect(fn).toBeCalled()
  })

  it('should emit stdout event', async () => {
    let file = await createTmpFile('echo foo')
    let task = workspace.createTask('echo')
    disposables.push(task)
    let p = new Promise<string[]>(resolve => {
      let lines: string[] = []
      task.onStdout(stdout => {
        lines.push(...stdout)
      })
      task.onExit(() => {
        resolve(lines)
      })
    })
    await task.start({ cmd: '/bin/sh', args: [file] })
    let lines = await p
    expect(lines).toEqual(['foo'])
  })

  it('should change environment variables', async () => {
    let file = await createTmpFile('echo $NODE_ENV\necho $COC_NVIM_TEST')
    let task = workspace.createTask('ENV')
    disposables.push(task)
    let lines: string[] = []
    task.onStdout(arr => {
      lines.push(...arr)
    })
    await task.start({
      cmd: '/bin/sh',
      args: [file],
      env: {
        NODE_ENV: 'production',
        COC_NVIM_TEST: 'yes'
      }
    })
    await new Promise<void>(resolve => {
      task.onExit(() => {
        resolve()
      })
    })
    expect(lines).toEqual(['production', 'yes'])
    let res = await nvim.call('getenv', 'COC_NVIM_TEST')
    expect(res).toBeNull()
  })

  it('should receive stdout lines as expected', async () => {
    let file = await createTmpFile('echo 3\necho ""\necho 4')
    let task = workspace.createTask('ENV')
    let p = new Promise(resolve => {
      let lines: string[] = []
      task.onStdout(arr => {
        lines.push(...arr)
      })
      task.onExit(() => {
        resolve(lines)
      })
    })
    await task.start({ cmd: '/bin/sh', args: [file] })
    let lines = await p
    expect(lines).toEqual(['3', '', '4'])
    task.dispose()
  })

  it('should emit stderr event', async () => {
    let file = await createTmpFile('console.error("start\\n\\nend");')
    let task = workspace.createTask('error')
    disposables.push(task)
    let p = new Promise<string[]>(resolve => {
      let lines: string[] = []
      task.onStderr(arr => {
        lines.push(...arr)
      })
      task.onExit(() => {
        resolve(lines)
      })
    })
    await task.start({ cmd: 'node', args: [file] })
    let lines = await p
    expect(lines).toEqual(['start', '', 'end'])
  })

  it('should not receive event from other task', async () => {
    let task1 = workspace.createTask('one')
    disposables.push(task1)
    let count = 0
    let cb = () => {
      count++
    }
    task1.onExit(cb)
    task1.onStderr(cb)
    task1.onStdout(cb)
    let file = await createTmpFile('console.log("start");console.error("end");')
    let task = workspace.createTask('error')
    await task.start({ cmd: 'node', args: [file] })
    let promise = new Promise<void>(resolve => {
      task.onExit(() => {
        resolve(undefined)
      })
    })
    await promise
    expect(count).toBe(0)
  })
})
