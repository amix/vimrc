import { CancellationTokenSource, Disposable } from 'vscode-languageserver-protocol'
import events from '../../events'
import { disposeAll, wait } from '../../util'

const disposables: Disposable[] = []
afterEach(async () => {
  disposeAll(disposables)
})

describe('register handler', () => {
  it('should fire InsertEnter and InsertLeave when necessary', async () => {
    let fn = jest.fn()
    events.on('InsertEnter', fn, null, disposables)
    events.on('InsertLeave', fn, null, disposables)
    expect(events.insertMode).toBe(false)
    await events.fire('CursorMovedI', [1, [1, 1]])
    expect(events.insertMode).toBe(true)
    await events.fire('CursorMoved', [1, [1, 1]])
    expect(events.insertMode).toBe(false)
    expect(fn).toBeCalledTimes(2)
  })

  it('should change pumvisible', async () => {
    expect(events.pumvisible).toBe(false)
    await events.fire('MenuPopupChanged', [{
      col: 6,
      row: 2,
      scrollbar: false,
      completed_item: {},
      width: 20,
      height: 12,
      size: 12
    }])
    expect(events.pumAlignTop).toBe(false)
    expect(events.pumvisible).toBe(true)
    await events.fire('CompleteDone', [{}])
    expect(events.pumvisible).toBe(false)
  })

  it('should register single handler', async () => {
    let fn = jest.fn()
    let obj = {}
    let disposable = events.on('BufEnter', fn, obj)
    disposables.push(disposable)
    await events.fire('BufEnter', ['a', 'b'])
    expect(fn).toBeCalledWith('a', 'b')
  })

  it('should register multiple events', async () => {
    let fn = jest.fn()
    let disposable = events.on(['TaskExit', 'TaskStderr'], fn)
    disposables.push(disposable)
    await events.fire('TaskExit', [])
    await events.fire('TaskStderr', [])
    expect(fn).toBeCalledTimes(2)
  })

  it('should resolve after timeout', async () => {
    let fn = (): Promise<void> => new Promise(resolve => {
      setTimeout(() => {
        resolve()
      }, 20)
    })
    let disposable = events.on('FocusGained', fn, {})
    disposables.push(disposable)
    let ts = Date.now()
    await events.fire('FocusGained', [])
    expect(Date.now() - ts >= 10).toBe(true)
  })

  it('should emit TextInsert after TextChangedI', async () => {
    let arr: string[] = []
    events.on('TextInsert', () => {
      arr.push('insert')
    }, null, disposables)
    events.on('TextChangedI', () => {
      arr.push('change')
    }, null, disposables)
    await events.fire('InsertCharPre', ['i', 1])
    await events.fire('TextChangedI', [1, {
      lnum: 1,
      col: 2,
      pre: 'i',
      changedtick: 1,
      line: 'i'
    }])
    expect(events.lastChangeTs).toBeDefined()
    await events.race(['TextInsert'])
    expect(arr).toEqual(['change', 'insert'])
  })

  it('should race events', async () => {
    let p = events.race(['InsertCharPre', 'TextChangedI', 'MenuPopupChanged'])
    await events.fire('InsertCharPre', ['i', 1])
    await events.fire('TextChangedI', [1, {
      lnum: 1,
      col: 2,
      pre: 'i',
      changedtick: 1
    }])
    let res = await p
    expect(res.name).toBe('InsertCharPre')
    res = await events.race(['TextChanged'], 50)
    expect(res).toBeUndefined()
  })

  it('should race same events', async () => {
    let arr: any[] = []
    void events.race(['TextChangedI'], 200).then(res => {
      arr.push(res)
    })
    void events.race(['TextChangedI'], 200).then(res => {
      arr.push(res)
    })
    await events.fire('TextChangedI', [2, {}])
    expect(arr.length).toBe(2)
    expect(arr.map(o => o.name)).toEqual(['TextChangedI', 'TextChangedI'])
  })

  it('should cancel race by CancellationToken', async () => {
    let tokenSource = new CancellationTokenSource()
    setTimeout(() => {
      tokenSource.cancel()
    }, 20)
    let res = await events.race(['TextChanged'], tokenSource.token)
    expect(res).toBeUndefined()
  })
})
