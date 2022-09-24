let g:jedi#completions_command = 'X'
source plugin/jedi.vim

describe 'completions'
    before
        new
        set filetype=python
    end

    after
        " default
        let g:jedi#popup_select_first = 1
        bd!
    end

    it 'longest in completeopt'
        " This gets set up with Vim only on VimEnter.
        if has('nvim')
            Expect stridx(&completeopt, 'longest') > -1
        else
            Expect stridx(&completeopt, 'longest') == -1
            doautocmd VimEnter
            Expect stridx(&completeopt, 'longest') > -1
        endif

        " Do not use it for following tests.
        set completeopt-=longest
    end

    it 'no smart import by default'
        exec "normal ifrom os "
        Expect getline('.') == 'from os '
    end

    it 'import'
        " X is the completion command
        normal oimporX
        Expect getline('.') == 'import'
        normal a subproX
        Expect getline('.') == 'import subprocess'
    end

    it 'exception'
        normal oIndentationErrX
        Expect getline('.') == 'IndentationError'

        " Do not remap keys (".") here, otherwise this triggers completion in
        " Neovim already.
        normal! a().filena

        normal aX
        Expect getline('.') == 'IndentationError().filename'
    end

    it 'multi complete'
        " NOTE: nvim results in "importErr()" here with completeopt+=longest,
        " but Vim is fine.
        " This is due to `pumvisible()` in jedi#complete_opened being true
        " with nvim still, but it is 0 with Vim, i.e. Vim appears to close
        " the pum already (with the tests).
        "
        " This might be a misunderstanding though, since the test might not
        " expect the "import" keyword to be offered for completion?!
        normal oImpXErrX()
        Expect getline('.') == 'ImportError()'
    end

    it 'cycling through entries popup_select_first=0'
        set completeopt+=longest
        let g:jedi#popup_select_first = 0
        execute "normal oraise impX\<C-n>"

        Expect getline('.') == 'raise ImportError'
        set completeopt-=longest
    end

    it 'cycling through entries popup_select_first=1'
        execute "normal oraise impX\<C-n>"
        Expect getline('.') == 'raise ImportWarning'
    end

    it 'cycling through entries popup_select_first=1 and longest'
        set completeopt+=longest
        execute "normal oraise impX"
        Expect getline('.') == 'raise Import'

        " With Neovim pumvisible() is 1 in jedi#complete_opened, which then
        " triggers the <Down>.  This is not the case with Vim.
        if has('nvim')
            execute "normal oraise impX\<C-n>"
            Expect getline('.') == 'raise ImportWarning'

            execute "normal oraise impX\<C-n>\<C-n>"
            Expect getline('.') == 'raise imp'
        else
            execute "normal oraise impX\<C-n>"
            Expect getline('.') == 'raise ImportError'

            execute "normal oraise impX\<C-n>\<C-n>"
            Expect getline('.') == 'raise ImportWarning'
        endif
        set completeopt-=longest
    end
end

describe 'smart completions'
    before
        new
        let g:jedi#smart_auto_mappings = 1
        set filetype=python
    end

    after
        " default
        let g:jedi#smart_auto_mappings = 0
        bd!
    end

    it 'smart import'
        exec "normal ifrom os "
        Expect getline('.') == 'from os import '
    end

    it 'no smart import after space'
        exec "normal! ifrom os "
        exec "normal  a "
        Expect getline('.') == 'from os  '
    end
end

" vim: et:ts=4:sw=4
