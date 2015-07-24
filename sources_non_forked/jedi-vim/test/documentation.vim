source plugin/jedi.vim

describe 'documentation docstrings'
    before
        set filetype=python
    end

    after
        bd!
        bd!
    end

    it 'simple'
        put = 'ImportError'
        normal GK
        Expect bufname('%') == "'__doc__'"
        Expect &filetype == 'rst'
        let content = join(getline(1,'$'), "\n")
        Expect stridx(content, "Import can't find module") > 0
        normal K
        Expect bufname('%') == ''
    end

    it 'no documentation'
        put = 'x = 2'
        normal o<ESC>GK
        Expect bufname('%') == ''
    end
end

" vim: et:ts=4:sw=4
