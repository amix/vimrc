if !exists('g:go_decls_mode')
  let g:go_decls_mode = ''
endif

function! go#decls#Decls(mode, ...) abort
  if g:go_decls_mode == 'ctrlp'
    call ctrlp#init(call("ctrlp#decls#cmd", [a:mode] + a:000))
  elseif g:go_decls_mode == 'fzf'
    call call("fzf#decls#cmd", [a:mode] + a:000)
  else
    if globpath(&rtp, 'plugin/ctrlp.vim') != ""
      call ctrlp#init(call("ctrlp#decls#cmd", [a:mode] + a:000))
    elseif globpath(&rtp, 'plugin/fzf.vim') != ""
      call call("fzf#decls#cmd", [a:mode] + a:000)
    else
      call go#util#EchoError("neither ctrlp.vim nor fzf.vim are installed. Please install either one")
    end
  end
endfunction

" vim: sw=2 ts=2 et
