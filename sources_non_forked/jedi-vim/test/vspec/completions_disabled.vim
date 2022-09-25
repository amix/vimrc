let g:jedi#completions_command = 'X'
let g:jedi#completions_enabled = 0
source plugin/jedi.vim

describe 'completions_disabled'
    before
        set filetype=python
    end

    after
        try | %bwipeout! | catch | endtry
    end

    it 'typing'
        normal oraise ImportErrX
        Expect getline('.') == 'raise ImportErrX'
    end
end

" vim: et:ts=4:sw=4
