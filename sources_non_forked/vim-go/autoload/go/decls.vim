" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#decls#Decls(mode, ...) abort
  let decls_mode = go#config#DeclsMode()
  if decls_mode == 'ctrlp'
    call ctrlp#init(call("ctrlp#decls#cmd", [a:mode] + a:000))
  elseif decls_mode == 'fzf'
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

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
