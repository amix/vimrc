import { CancellationTokenSource } from 'vscode-languageserver-protocol'
import { Chars } from '../../model/chars'

describe('chars keyword option', () => {
  it('should match @', () => {
    let chars = new Chars('@')
    expect(chars.isKeywordChar('a')).toBe(true)
    expect(chars.isKeywordChar('z')).toBe(true)
    expect(chars.isKeywordChar('A')).toBe(true)
    expect(chars.isKeywordChar('Z')).toBe(true)
    expect(chars.isKeywordChar('\u205f')).toBe(false)
  })

  it('should match code range', () => {
    let chars = new Chars('48-57')
    expect(chars.isKeywordChar('0')).toBe(true)
    expect(chars.isKeywordChar('9')).toBe(true)
  })

  it('should match @-@', () => {
    let chars = new Chars('@-@')
    expect(chars.isKeywordChar('@')).toBe(true)
  })

  it('should match single code', () => {
    let chars = new Chars('58')
    expect(chars.isKeywordChar(':')).toBe(true)
  })

  it('should match single character', () => {
    let chars = new Chars('_')
    expect(chars.isKeywordChar('_')).toBe(true)
  })
})

describe('chars addKeyword', () => {
  it('should add keyword', () => {
    let chars = new Chars('_')
    chars.addKeyword(':')
    expect(chars.isKeywordChar(':')).toBe(true)
  })
})

describe('chars change keyword', () => {
  it('should change keyword', () => {
    let chars = new Chars('_')
    chars.setKeywordOption(':')
    expect(chars.isKeywordChar(':')).toBe(true)
    expect(chars.isKeywordChar('_')).toBe(false)
  })
})

describe('chars match keywords', () => {
  it('should match keywords', async () => {
    let chars = new Chars('@')
    let source = new CancellationTokenSource()
    let res = await chars.matchLines(['foo bar'], 3, source.token)
    expect(Array.from(res)).toEqual(['foo', 'bar'])
  })

  it('should consider unicode character as word', async () => {
    let chars = new Chars('@')
    let res = await chars.matchLines(['blackкофе'], 3)
    expect(Array.from(res)).toEqual(['blackкофе'])
  })
})

describe('chars isKeyword', () => {
  it('should check isKeyword', () => {
    let chars = new Chars('@')
    expect(chars.isKeyword('foo')).toBe(true)
    expect(chars.isKeyword('f@')).toBe(false)
  })
})
