if !jedi#init_python()
    finish
endif

if g:jedi#auto_initialization
    if g:jedi#completions_enabled
        " We need our own omnifunc, so this overrides the omnifunc set by
        " $VIMRUNTIME/ftplugin/python.vim.
        setlocal omnifunc=jedi#completions

        " map ctrl+space for autocompletion
        if g:jedi#completions_command == "<C-Space>"
            " In terminals, <C-Space> sometimes equals <Nul>.
            imap <buffer> <Nul> <C-Space>
            smap <buffer> <Nul> <C-Space>
        endif
        if g:jedi#completions_command != ""
            execute "inoremap <expr> <buffer> ".g:jedi#completions_command." jedi#complete_string(0)"
            " A separate mapping for select mode: deletes and completes.
            execute "snoremap <expr> <buffer> ".g:jedi#completions_command." '\<C-g>c'.jedi#complete_string(0)"
        endif
    endif
endif
