source plugin/jedi.vim

describe 'signatures'
    before
        set filetype=python
    end

    after
        bd!
        bd!
    end

    it 'simple'
        normal oabs( 
        " equals doautocmd CursorMovedI
        Python jedi_vim.show_call_signatures()

        Expect getline(1) == '=`=jedi=0, =`=   (*_*number*_*) =`=jedi=`='

        doautocmd InsertLeave 
        Expect getline(1) == ''
    end

    it 'no signature'
        normal ostr 
        Python jedi_vim.show_call_signatures()
        Expect getline(1, '$') == ['', 'str ']
    end

    it 'signatures disabled'
        let g:jedi#show_call_signatures = 0

        normal ostr( 
        Python jedi_vim.show_call_signatures()
        Expect getline(1, '$') == ['', 'str( ']

        let g:jedi#show_call_signatures = 1
    end

    it 'command line simple'
        let g:jedi#show_call_signatures = 2
        call jedi#configure_call_signatures()

        normal oabs( 
        redir => msg
        Python jedi_vim.show_call_signatures()
        redir END
        Expect msg == "\nabs(number)"

        redir => msg
        doautocmd InsertLeave 
        redir END
        Expect msg == "\n\n"
    end

    it 'command line no signature'
        let g:jedi#show_call_signatures = 2
        call jedi#configure_call_signatures()

        normal ostr 
        redir => msg
        Python jedi_vim.show_call_signatures()
        redir END
        Expect msg == "\n"
    end
end
