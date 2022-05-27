" Check if tagbar is installed under plugins or is directly under rtp
" this covers pathogen + Vundle/Bundle
"
" Also make sure the ctags command exists
"
if !executable('ctags')
  finish
elseif globpath(&rtp, 'plugin/tagbar.vim') == ""
  finish
endif

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

if !exists("g:go_gotags_bin")
  let g:go_gotags_bin = "gotags"
endif


function! s:SetTagbar()
  let bin_path = go#path#CheckBinPath(g:go_gotags_bin)
  if empty(bin_path)
    return
  endif

  if !exists("g:tagbar_type_go")
    let g:tagbar_type_go = {
          \ 'ctagstype' : 'go',
          \ 'kinds'     : [
          \ 'p:package',
          \ 'i:imports',
          \ 'c:constants',
          \ 'v:variables',
          \ 't:types',
          \ 'n:interfaces',
          \ 'w:fields',
          \ 'e:embedded',
          \ 'm:methods',
          \ 'r:constructor',
          \ 'f:functions'
          \ ],
          \ 'sro' : '.',
          \ 'kind2scope' : {
          \ 't' : 'ctype',
          \ 'n' : 'ntype'
          \ },
          \ 'scope2kind' : {
          \ 'ctype' : 't',
          \ 'ntype' : 'n'
          \ },
          \ 'ctagsbin'  : bin_path,
          \ 'ctagsargs' : '-sort -silent'
          \ }
  endif
endfunction


call s:SetTagbar()

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
