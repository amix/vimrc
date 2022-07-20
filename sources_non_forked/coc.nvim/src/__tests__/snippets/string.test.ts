import { SnippetString } from '../../snippets/string'

describe('SnippetString', () => {
  describe('Append', () => {
    it('should append plain snippets', () => {
      let snippetString = new SnippetString('foo')
      expect(snippetString.value).toBe('foo')

      snippetString = new SnippetString().appendText('foo')
      expect(snippetString.value).toBe('foo')
    })

    it('should append tabstop', () => {
      let snippetString = new SnippetString()
        .appendTabstop()
        .appendText(' ')
        .appendTabstop()
        .appendText(' ')
        .appendTabstop(4)
        .appendText(' ')
        .appendTabstop(3)

      expect(snippetString.value).toBe('$1 $2 $4 $3')
    })

    it('should append placeholder', () => {
      let snippetString = new SnippetString()
        .appendPlaceholder('abcdef')
        .appendText(' ')
        .appendPlaceholder('foo')
        .appendText(' ')
        .appendPlaceholder('bar', 4)
        .appendText(' ')
        .appendPlaceholder('a', 3)
        .appendText(' ')
        .appendPlaceholder(s => {
          s.appendText('plain')
        }, 5)

      expect(snippetString.value).toBe('${1:abcdef} ${2:foo} ${4:bar} ${3:a} ${5:plain}')
    })

    it('should append choice', () => {
      let snippetString = new SnippetString()
        .appendChoice(['foo', 'bar'])
        .appendText(' ')
        .appendChoice(['foo3', 'bar3'], 3)
        .appendText(' ')
        .appendChoice(['foo2', 'bar2'], 2)

      expect(snippetString.value).toBe('${1|foo,bar|} ${3|foo3,bar3|} ${2|foo2,bar2|}')
    })

    it('should append variables', () => {
      let snippetString = new SnippetString()
        .appendVariable('foo', 'abcdef')
        .appendText(' ')
        .appendVariable('bar')

      expect(snippetString.value).toBe('${foo:abcdef} ${bar}')

      snippetString = new SnippetString()
        .appendVariable('foo', s => s.appendText('abcdef'))
        .appendText(' ')
        .appendVariable('bar')

      expect(snippetString.value).toBe('${foo:abcdef} ${bar}')
    })
  })

  describe('isSnippetString()', () => {
    let snippetString = new SnippetString()
    expect(SnippetString.isSnippetString(snippetString)).toBe(true)

    let snippetStr = ''
    expect(SnippetString.isSnippetString(snippetStr)).toBe(false)
  })
})
