command! -buffer -range=% RetabSnip <line1>,<line2>call snipMate#RetabSnip()
vnoremap <buffer> <cr> :RetabSnip<cr>

if !exists('g:nsippet_no_indentation_settings')
  setlocal sw=4
  setlocal tabstop=4
  setlocal noexpandtab
endif
