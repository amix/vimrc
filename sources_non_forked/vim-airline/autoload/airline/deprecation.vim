" MIT License. Copyright (c) 2013 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

function! airline#deprecation#check()
  if exists('g:airline_enable_fugitive') || exists('g:airline_fugitive_prefix')
    echom 'The g:airline_enable_fugitive and g:airline_fugitive_prefix variables are obsolete. Please read the documentation about the branch extension.'
  endif

  let tests = [
        \ [ 'g:airline_paste_symbol', 'g:airline_symbols.paste' ],
        \ [ 'g:airline_readonly_symbol', 'g:airline_symbols.readonly' ],
        \ [ 'g:airline_linecolumn_prefix', 'g:airline_symbols.linenr' ],
        \ [ 'g:airline_branch_prefix', 'g:airline_symbols.branch' ],
        \ [ 'g:airline_branch_empty_message', 'g:airline#extensions#branch#empty_message' ],
        \ [ 'g:airline_detect_whitespace', 'g:airline#extensions#whitespace#enabled|show_message' ],
        \ [ 'g:airline_enable_hunks', 'g:airline#extensions#hunks#enabled' ],
        \ [ 'g:airline_enable_tagbar', 'g:airline#extensions#tagbar#enabled' ],
        \ [ 'g:airline_enable_csv', 'g:airline#extensions#csv#enabled' ],
        \ [ 'g:airline_enable_branch', 'g:airline#extensions#branch#enabled' ],
        \ [ 'g:airline_enable_bufferline', 'g:airline#extensions#bufferline#enabled' ],
        \ [ 'g:airline_enable_syntastic', 'g:airline#extensions#syntastic#enabled' ],
        \ [ 'g:airline_enable_eclim', 'g:airline#extensions#eclim#enabled' ],
        \ ]
  for test in tests
    if exists(test[0])
      let max = winwidth(0) - 16
      let msg = printf('The variable %s is deprecated and may not work in the future. It has been replaced with %s. Please read the documentation.', test[0], test[1])
      echom msg[:max].'...'
    endif
  endfor
endfunction

