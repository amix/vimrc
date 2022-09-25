source plugin/jedi.vim

describe 'signatures'
    before
        enew
        set filetype=python
    end

    after
        try | %bwipeout! | catch | endtry
    end

    it 'simple'
        normal odef xyz(number): return
        normal o
        normal oxyz()
        doautocmd CursorHoldI
        Expect getline(3) == '?!?jedi=0, ?!?   (*_*number*_*) ?!?jedi?!?'

        doautocmd InsertLeave
        Expect getline(3) == ''
    end

    it 'multiple buffers'
        set hidden
        new
        setfiletype python
        redir => autocmds
        autocmd jedi_call_signatures * <buffer>
        redir END
        Expect autocmds =~# 'jedi_call_signatures'
        buffer #
        redir => autocmds
        autocmd jedi_call_signatures * <buffer>
        redir END
        Expect autocmds =~# 'jedi_call_signatures'
    end

    it 'simple after CursorHoldI with only parenthesis'
        noautocmd normal o
        doautocmd CursorHoldI
        noautocmd normal istaticmethod()
        doautocmd CursorHoldI
        Expect getline(1) == '?!?jedi=0, ?!?            (*_*f: Callable[..., Any]*_*) ?!?jedi?!?'
    end

    it 'highlights correct argument'
        noautocmd normal o
        doautocmd CursorHoldI
        noautocmd normal iformat(42, "x")
        " Move to x - highlights "x".
        noautocmd normal 2h
        doautocmd CursorHoldI
        Expect getline(1) == '?!?jedi=0, ?!?      (value: object, *_*format_spec: str=...*_*) ?!?jedi?!?'
        " Move left to 42 - hightlights first argument ("value").
        noautocmd normal 4h
        doautocmd CursorHoldI
        Expect getline(1) == '?!?jedi=0, ?!?      (*_*value: object*_*, format_spec: str=...) ?!?jedi?!?'
    end

    it 'no signature'
        exe 'normal ostr '
        python3 jedi_vim.show_call_signatures()
        Expect getline(1, '$') == ['', 'str ']
    end

    it 'signatures disabled'
        let g:jedi#show_call_signatures = 0

        exe 'normal ostr( '
        python3 jedi_vim.show_call_signatures()
        Expect getline(1, '$') == ['', 'str( ']

        let g:jedi#show_call_signatures = 1
    end

    it 'command line simple'
        let g:jedi#show_call_signatures = 2
        call jedi#configure_call_signatures()

        exe 'normal ostaticmethod( '
        redir => msg
        python3 jedi_vim.show_call_signatures()
        redir END
        Expect msg == "\nstaticmethod(f: Callable[..., Any])"

        redir => msg
        doautocmd InsertLeave
        redir END
        Expect msg == "\n"

        normal Sdef foo(a, b): pass
        exe 'normal ofoo(a, b, c, '
        redir => msg
        python3 jedi_vim.show_call_signatures()
        redir END
        Expect msg == "\nfoo(a, b)"
    end

    it 'command line truncation'
        let g:jedi#show_call_signatures = 2
        call jedi#configure_call_signatures()

        function! Signature()
            redir => msg
            python3 jedi_vim.show_call_signatures()
            redir END
            return msg
        endfunction

        let funcname = repeat('a', &columns - (30 + (&ruler ? 18 : 0)))
        put = 'def '.funcname.'(arg1, arg2, arg3, a, b, c):'
        put = '    pass'
        execute "normal o\<BS>".funcname."( "
        Expect Signature() == "\n".funcname."(arg1, …)"

        exe 'normal sarg1, '
        Expect Signature() == "\n".funcname."(…, arg2, …)"

        exe 'normal sarg2, arg3, '
        Expect Signature() == "\n".funcname."(…, a, b, c)"

        exe 'normal sa, b, '
        Expect Signature() == "\n".funcname."(…, c)"

        g/^/d
        put = 'def '.funcname.'('.repeat('b', 20).', arg2):'
        put = '    pass'
        execute "normal o\<BS>".funcname."( "
        Expect Signature() == "\n".funcname."(…)"
    end

    it 'command line no signature'
        let g:jedi#show_call_signatures = 2
        call jedi#configure_call_signatures()

        exe 'normal ostr '
        redir => msg
        python3 jedi_vim.show_call_signatures()
        redir END
        Expect msg == "\n"
    end
end
