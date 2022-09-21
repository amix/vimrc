let mapleader = '\'
source plugin/jedi.vim
source test/_utils.vim

describe 'goto simple:'
    before
        new
        set filetype=python
        put =[
        \   'def a(): pass',
        \   'b = a',
        \   'c = b',
        \ ]
        normal! ggdd
        normal! G$
        Expect line('.') == 3
    end

    after
        bd!
    end

    it 'goto definitions'
        normal \d
        Expect line('.') == 2
        Expect col('.') == 1
    end

    it 'goto assignments'
        normal \g
        Expect line('.') == 2
        Expect col('.') == 1

        " cursor before `=` means that it stays there.
        normal \g
        Expect line('.') == 2
        Expect col('.') == 1

        " going to the last line changes it.
        normal! $
        normal \g
        Expect line('.') == 1
        Expect col('.') == 5
    end
end


describe 'goto with tabs:'
    before
        set filetype=python
        let g:jedi#use_tabs_not_buffers = 1
    end

    after
        try | %bwipeout! | catch | endtry
    end

    it 'follow import'
        put = ['import subprocess', 'subprocess']
        normal G\g
        Expect getline('.') == 'import subprocess'
        Expect line('.') == 2
        Expect col('.') == 8

        normal G\d
        Expect CurrentBufferIsModule('subprocess') == 1
        Expect line('.') == 1
        Expect col('.') == 1
        Expect tabpagenr('$') == 2
        Expect winnr('$') == 1
        bwipe

        Expect tabpagenr('$') == 1
        Expect bufname('%') == ''
    end
end


describe 'goto with buffers'
    before
        set filetype=python
        let g:jedi#use_tabs_not_buffers = 0
    end

    after
        try | %bwipeout! | catch | endtry
        set nohidden
    end

    it 'no new tabs'
        put = ['import os']
        normal G$
        call jedi#goto_assignments()
        python3 jedi_vim.goto()
        Expect CurrentBufferIsModule('os') == 0
        " Without hidden, it's not possible to open a new buffer, when the old
        " one is not saved.
        set hidden
        call jedi#goto_assignments()
        Expect CurrentBufferIsModule('os') == 1
        Expect winnr('$') == 1
        Expect tabpagenr('$') == 1
        Expect line('.') == 1
        Expect col('.') == 1
    end
end



describe 'goto with splits'
    before
        enew!
        set filetype=python
        let g:jedi#use_splits_not_buffers = 'left'
    end

    after
        try | %bwipeout! | catch | endtry
    end

    it 'follow import'
        put = ['import subprocess', 'subprocess']
        normal G\g
        Expect getline('.') == 'import subprocess'
        Expect line('.') == 2
        Expect col('.') == 8

        normal G\d
        Expect CurrentBufferIsModule('subprocess') == 1
        Expect line('.') == 1
        Expect col('.') == 1
        Expect winnr('$') == 2
        wincmd l
        Expect bufname('%') == ''
    end

end


describe 'goto wildignore'
    before
        enew!
        set filetype=python
        set wildignore=*,with\ spaces,*.pyc
        set hidden
        let g:jedi#use_tag_stack = 1
        let g:jedi#use_tabs_not_buffers = 0
        " Need to use splits for code coverage in new_buffer()
        let g:jedi#use_splits_not_buffers = 1

        put = ['from subprocess import Popen', 'Popen']
        Expect CurrentBufferIsModule('subprocess') == 0
        normal G
    end

    after
        try | %bwipeout! | catch | endtry
        set wildignore&vim
    end

    it 'restores wildignore'
        let before = &wildignore
        call jedi#goto()
        Expect getline('.') =~ 'Popen'
        Expect &wildignore == before
    end

    it 'not using tagstack'
        let g:jedi#use_tag_stack = 0
        call jedi#goto()
        Expect CurrentBufferIsModule('subprocess') == 1
        Expect getline('.') =~ 'Popen'
    end
end


" vim: et:ts=4:sw=4
