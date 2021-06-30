if exists("g:ack_autofold_results") && g:ack_autofold_results
  setlocal foldlevel=0
  setlocal foldmethod=expr
  setlocal foldexpr=matchstr(getline(v:lnum),'^[^\|]\\+')==#matchstr(getline(v:lnum+1),'^[^\|]\\+')?1:'<1'
  setlocal foldenable
  setlocal foldclose=all
  setlocal foldopen=all
  nnoremap <buffer> j jzz
endif
