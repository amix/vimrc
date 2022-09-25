source plugin/jedi.vim

describe 'JediDebugInfo'
    it 'works'
        redir @a | JediDebugInfo | redir END
		let output = split(@a, '\n')
        Expect output[0] == 'You should run this in a buffer with filetype "python".'
		Expect output[1] == '#### Jedi-vim debug information'
		Expect output[-1] == '</details>'
    end
end

" vim: et:ts=4:sw=4
