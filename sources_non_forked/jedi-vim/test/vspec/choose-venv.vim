source plugin/jedi.vim
source test/_utils.vim

describe 'simple:'
    before
        new
        normal! ifoo
    end

    after
        bd!
    end

    it 'choose'
        Expect g:jedi#environment_path == 'auto'
        Expect bufname('%') == ''

        JediChooseEnvironment
        " A Python executable needs to be a few letters
        Expect len(getline('.')) > 5
        Expect bufname('%') == 'Hit Enter to Choose an Environment'

        execute "normal \<CR>"
        Expect g:jedi#environment_path != 'auto'
        bd " TODO why is this necessary? There seems to be a random buffer.
        Expect bufname('%') == ''
        Expect getline('.') == 'foo'
    end
end
