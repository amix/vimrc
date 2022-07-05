import DB from '../../model/db'
import path from 'path'

let db: DB
beforeAll(async () => {
  db = new DB(path.join(__dirname, 'db.json'))
})

afterAll(async () => {
  db.destroy()
})

afterEach(async () => {
  db.clear()
})

describe('DB', () => {

  test('db.exists()', async () => {
    let exists = db.exists('a.b')
    expect(exists).toBe(false)
    db.push('a.b', { foo: 1 })
    exists = db.exists('a.b.foo')
    expect(exists).toBe(true)
  })

  test('db.fetch()', async () => {
    let res = await db.fetch('x')
    expect(res).toBeUndefined()
    db.push('x', 1)
    res = await db.fetch('x')
    expect(res).toBe(1)
    db.push('x', { foo: 1 })
    res = await db.fetch('x')
    expect(res).toEqual({ foo: 1 })
  })

  test('db.delete()', async () => {
    db.push('foo.bar', 1)
    db.delete('foo.bar')
    let exists = db.exists('foo.bar')
    expect(exists).toBe(false)
  })

  test('db.push()', async () => {
    db.push('foo.x', 1)
    db.push('foo.y', '2')
    db.push('foo.z', true)
    db.push('foo.n', null)
    db.push('foo.o', { x: 1 })
    let res = db.fetch('foo')
    expect(res).toEqual({
      x: 1,
      y: '2',
      z: true,
      n: null,
      o: { x: 1 }
    })
  })
})
