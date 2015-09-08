let g:jedi#completions_command = 'X'
source plugin/jedi.vim

describe 'completions'
    before
        new
        set filetype=python
    end

    after
        bd!
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
        normal a().filenaX
        Expect getline('.') == 'IndentationError().filename'
    end

    it 'dot_open'
        normal oraisX ImpXErrX()
        Expect getline('.') == 'raise ImportError()'
    end

    it 'cycling through entries'
        " testing select_first doesn't seem to work in ex mode
        execute "normal oraise impX\<C-n>\<C-n>\<C-n>"
        Expect getline('.') == 'raise ImportWarning'
        let g:jedi#popup_select_first = 0
        execute "normal oraise impX\<C-n>\<C-n>\<C-n>"
        Expect getline('.') == 'raise ImportWarning'
        let g:jedi#popup_select_first = 1

        " -longest completes the first one
        set completeopt -=longest
        execute "normal oraise baseX"
        Expect getline('.') == 'raise BaseException'
        set completeopt +=longest

    end
end

" vim: et:ts=4:sw=4
