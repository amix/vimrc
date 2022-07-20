import { SemanticTokensBuilder } from '../../model/semanticTokensBuilder'
import { Range, SemanticTokensLegend } from 'vscode-languageserver-protocol'

function toArr(uint32Arr: ReadonlyArray<number>): number[] {
  const r = []
  for (let i = 0, len = uint32Arr.length; i < len; i++) {
    r[i] = uint32Arr[i]
  }
  return r
}

function deepStrictEqual(one: any, two: any): void {
  expect(one).toEqual(two)
}

describe('SemanticTokensBuilder', () => {
  it('should build SemanticTokensBuilder simple', () => {
    const builder = new SemanticTokensBuilder()
    builder.push(1, 0, 5, 1, 1)
    builder.push(1, 10, 4, 2, 2)
    builder.push(2, 2, 3, 2, 2)
    deepStrictEqual(toArr(builder.build().data), [
      1, 0, 5, 1, 1,
      0, 10, 4, 2, 2,
      1, 2, 3, 2, 2
    ])
  })

  it('should build SemanticTokensBuilder no modifier', () => {
    const builder = new SemanticTokensBuilder()
    builder.push(1, 0, 5, 1)
    builder.push(1, 10, 4, 2)
    builder.push(2, 2, 3, 2)
    deepStrictEqual(toArr(builder.build().data), [
      1, 0, 5, 1, 0,
      0, 10, 4, 2, 0,
      1, 2, 3, 2, 0
    ])
  })

  it('should build SemanticTokensBuilder out of order 1', () => {
    const builder = new SemanticTokensBuilder()
    builder.push(2, 0, 5, 1, 1)
    builder.push(2, 10, 1, 2, 2)
    builder.push(2, 15, 2, 3, 3)
    builder.push(1, 0, 4, 4, 4)
    deepStrictEqual(toArr(builder.build().data), [
      1, 0, 4, 4, 4,
      1, 0, 5, 1, 1,
      0, 10, 1, 2, 2,
      0, 5, 2, 3, 3
    ])
  })

  it('SemanticTokensBuilder out of order 2', () => {
    const builder = new SemanticTokensBuilder()
    builder.push(2, 10, 5, 1, 1)
    builder.push(2, 2, 4, 2, 2)
    deepStrictEqual(toArr(builder.build().data), [
      2, 2, 4, 2, 2,
      0, 8, 5, 1, 1
    ])
  })

  test('SemanticTokensBuilder with legend', () => {
    const legend: SemanticTokensLegend = {
      tokenTypes: ['aType', 'bType', 'cType', 'dType'],
      tokenModifiers: ['mod0', 'mod1', 'mod2', 'mod3', 'mod4', 'mod5']
    }
    const builder = new SemanticTokensBuilder(legend)
    builder.push(Range.create(1, 0, 1, 5), 'bType')
    builder.push(Range.create(2, 0, 2, 4), 'cType', ['mod0', 'mod5'])
    builder.push(Range.create(3, 0, 3, 3), 'dType', ['mod2', 'mod4'])
    deepStrictEqual(toArr(builder.build().data), [
      1, 0, 5, 1, 0,
      1, 0, 4, 2, 1 | (1 << 5),
      1, 0, 3, 3, (1 << 2) | (1 << 4)
    ])
  })
})
