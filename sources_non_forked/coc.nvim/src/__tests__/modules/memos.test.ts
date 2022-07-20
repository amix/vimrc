import Memos from '../../model/memos'
import os from 'os'
import path from 'path'
import fs from 'fs'

let filepath = path.join(os.tmpdir(), 'test')
let memos: Memos
beforeEach(() => {
  memos = new Memos(filepath)
})

afterEach(() => {
  if (fs.existsSync(filepath)) {
    fs.unlinkSync(filepath)
  }
})

describe('Memos', () => {
  it('should update and get', async () => {
    let memo = memos.createMemento('x')
    await memo.update('foo.bar', 'memo')
    let res = memo.get<string>('foo.bar')
    expect(res).toBe('memo')
  })

  it('should get value for key if it does not exist', async () => {
    let memo = memos.createMemento('y')
    let res = memo.get<any>('xyz')
    expect(res).toBeUndefined()
  })

  it('should use defaultValue when it does not exist', async () => {
    let memo = memos.createMemento('y')
    let res = memo.get<any>('f.o.o', 'default')
    expect(res).toBe('default')
  })

  it('should update multiple values', async () => {
    let memo = memos.createMemento('x')
    await memo.update('foo', 'x')
    await memo.update('bar', 'y')
    expect(memo.get<string>('foo')).toBe('x')
    expect(memo.get<string>('bar')).toBe('y')
  })
})
