describe 'snippet parser'

    before
        function! Parse(snippet, ...)
            let [snip, stops] = snipmate#parse#snippet(a:snippet)
            return a:0 ? [snip, stops] : snip
        endfunction
        let b:snipmate_visual = 'testvisual'
    end

    it 'parses numeric $id and ${id} vars as [id] lists'
        let expect = [[1234567890]]
        Expect Parse('$1234567890') == expect
        Expect Parse('${1234567890}') == expect
    end

    it 'disregards $ or ${ followed by a non-id'
        Expect Parse('$x1') == ['x1']
        Expect Parse('${x}1') == ['x}1']
        Expect Parse('$VISUA1') == ['VISUA1']
        Expect Parse('${VISUA}1') == ['VISUA}1']
    end

    it 'gathers references to each instance of each stop id'
        let [snip, b:stops] = Parse('x$1x${2:x$1x}x$1x${1/a/b}x$VISUALx', 1)
        function! InstanceFound(list)
            return !empty(filter(copy(b:stops[a:list[0]].instances),
                        \ 'v:val is a:list'))
        endfunction
        function! CheckList(list)
            for item in a:list
                if type(item) == type([])
                    Expect InstanceFound(item) to_be_true
                    call CheckList(item)
                endif
                unlet item " E732
            endfor
        endfunction
        call CheckList(snip)
    end

    it 'parses mirror substitutions ${n/pat/sub} as [n, {...}]'
        let expect = [[1, { 'pat' : 'abc', 'sub' : 'def' }]]
        Expect Parse('${1/abc/def}') == expect
        let expect[0][1].flags = ''
        Expect Parse('${1/abc/def/}') == expect
        let expect[0][1].flags = 'g'
        Expect Parse('${1/abc/def/g}') == expect
    end

    it 'parses vars with placeholders as [id, placeholder] lists'
        Expect Parse('${1:abc}') == [[1, 'abc']]
    end

    it 'evaluates backtick expressions'
        Expect Parse('`fnamemodify("x.y", ":r")`') == ['x']
    end

    it 'parses placeholders for vars and other specials'
        let text = 'a `fnamemodify("x.y", ":r")` ${2:(${3/a/b})}'
        let expect = ['a x ', [2, '(', [3, { 'pat' : 'a', 'sub' : 'b' }], ')']]
        Expect Parse(text) == expect
        Expect Parse(printf('${1:%s}', text)) == [[1] + expect]
    end

    it 'converts tabs according to &et, &sts, &sw'
        " &noet -> leave tabs alone
        setl noet
        Expect Parse("abc\tdef\n\t\tghi") == ["abc\tdef", "\t\tghi"]

        " &et -> &sts or &sw
        setl et sts=2 sw=3
        Expect Parse("abc\tdef\n\t\tghi") == ["abc  def", "    ghi"]

        setl et sts=0 sw=3
        Expect Parse("abc\tdef\n\t\tghi") == ["abc   def", "      ghi"]

        setl et sts=-1 sw=3
        Expect Parse("abc\tdef\n\t\tghi") == ["abc   def", "      ghi"]
    end

    it 'parses backslashes as escaping the next character or joining lines'
        Expect Parse('x\x') == ['xx']
        Expect Parse('x\\x') == ['x\x']
        Expect Parse("x\\\nx") == ['xx']
        Expect Parse('x\$1') == ['x$1']
        Expect Parse('${1:\}}') == [[1, '}']]
        Expect Parse('${1/\//\}}') == [[1, { 'pat' : '/', 'sub' : '}' }]]
        Expect Parse('`fnamemodify("\`.x", ":r")`') == ['`']
        Expect Parse('\`x\`') == ['`x`']
    end

    it 'splits text at newlines'
        Expect Parse("x\nx") == ['x', 'x']
    end

    it 'joins evaluated expressions to surrounding text on the same line'
        let g:foo = 'bar'
        Expect Parse("x`g:foo`x") == ['xbarx']
        Expect Parse("x`g:foo`\nx") == ['xbar', 'x']
        Expect Parse("x\n`g:foo`x") == ['x', 'barx']
    end

    it 'adds empty strings before/after vars if at the start/end of a line'
        Expect Parse("x$1\nx") == ['x', [1], '', 'x']
        Expect Parse("x\n$1x") == ['x', '', [1], 'x']
    end

    it 'expands $VISUAL placeholders with any indents'
        Expect Parse("x$VISUALx") == ['xtestvisualx']
        let b:snipmate_visual = "  foo\nbar\n  baz"
        setl noet
        Expect Parse("\tx\n\t$VISUAL\nx") == ["\tx", "\t  foo", "\tbar", "\t  baz", "x"]
    end

end
