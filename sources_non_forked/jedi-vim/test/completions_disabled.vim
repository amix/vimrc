let g:jedi#completions_command = 'X'
let g:jedi#completions_enabled = 0
source plugin/jedi.vim

describe 'completions_disabled'
    before
        new
        set filetype=python
    end

    after
        bd!
    end

    it 'typing'
        normal oraise ImportErrX
        Expect getline('.') == 'raise ImportErrX'
    end
end

" vim: et:ts=4:sw=4
