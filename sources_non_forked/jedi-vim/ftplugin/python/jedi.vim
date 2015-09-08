if !jedi#init_python()
    finish
endif
" ------------------------------------------------------------------------
" Initialization of jedi-vim
" ------------------------------------------------------------------------

if g:jedi#auto_initialization
    " goto / get_definition / usages
    if g:jedi#goto_command != ''
        execute "nnoremap <buffer> ".g:jedi#goto_command." :call jedi#goto()<CR>"
    endif
    if g:jedi#goto_assignments_command != ''
        execute "nnoremap <buffer> ".g:jedi#goto_assignments_command." :call jedi#goto_assignments()<CR>"
    endif
    if g:jedi#goto_definitions_command != ''
        execute "nnoremap <buffer> ".g:jedi#goto_definitions_command." :call jedi#goto_definitions()<CR>"
    endif
    if g:jedi#usages_command != ''
        execute "nnoremap <buffer> ".g:jedi#usages_command." :call jedi#usages()<CR>"
    endif
    " rename
    if g:jedi#rename_command != ''
        execute "nnoremap <buffer> ".g:jedi#rename_command." :call jedi#rename()<CR>"
        execute "vnoremap <buffer> ".g:jedi#rename_command." :call jedi#rename_visual()<CR>"
    endif
    " documentation/pydoc
    if g:jedi#documentation_command != ''
        execute "nnoremap <silent> <buffer>".g:jedi#documentation_command." :call jedi#show_documentation()<CR>"
    endif

    if g:jedi#show_call_signatures > 0 && has('conceal')
        call jedi#configure_call_signatures()
    endif

    if g:jedi#completions_enabled == 1
        inoremap <silent> <buffer> . .<C-R>=jedi#complete_string(1)<CR>
    endif

    if g:jedi#auto_close_doc
        " close preview if its still open after insert
        autocmd InsertLeave <buffer> if pumvisible() == 0|pclose|endif
    endif
endif
