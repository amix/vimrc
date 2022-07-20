import Mru from '../../model/mru'
import os from 'os'
import fs from 'fs'
import path from 'path'
const root = fs.mkdtempSync(path.join(os.tmpdir(), 'coc-mru-'))

describe('Mru', () => {

  it('should load items', async () => {
    let mru = new Mru('test', root)
    await mru.clean()
    let res = await mru.load()
    expect(res.length).toBe(0)
    res = mru.loadSync()
    expect(res.length).toBe(0)
  })

  it('should add items', async () => {
    let mru = new Mru('test', root)
    await mru.add('a')
    await mru.add('b')
    let res = await mru.load()
    expect(res.length).toBe(2)
    await mru.clean()
  })

  it('should add when file it does not exist', async () => {
    let mru = new Mru('test', root)
    await mru.clean()
    await mru.add('a')
    let res = await mru.load()
    expect(res).toEqual(['a'])
  })

  it('should remove item', async () => {
    let mru = new Mru('test', root)
    await mru.add('a')
    await mru.remove('a')
    let res = await mru.load()
    expect(res.length).toBe(0)
    await mru.clean()
  })
})
