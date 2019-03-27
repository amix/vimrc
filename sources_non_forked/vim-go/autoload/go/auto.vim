" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#auto#template_autocreate()
  if !go#config#TemplateAutocreate() || !&modifiable
    return
  endif

  " create new template from scratch
  call go#template#create()
endfunction

function! go#auto#echo_go_info()
  if !go#config#EchoGoInfo()
    return
  endif

  if !exists('v:completed_item') || empty(v:completed_item)
    return
  endif
  let item = v:completed_item

  if !has_key(item, "info")
    return
  endif

  if empty(item.info)
    return
  endif

  redraws! | echo "vim-go: " | echohl Function | echon item.info | echohl None
endfunction

function! go#auto#auto_type_info()
  if !go#config#AutoTypeInfo() || !filereadable(expand('%:p'))
    return
  endif

  " GoInfo automatic update
  call go#tool#Info(0)
endfunction

function! go#auto#auto_sameids()
  if !go#config#AutoSameids() || !filereadable(expand('%:p'))
    return
  endif

  " GoSameId automatic update
  call go#guru#SameIds(0)
endfunction

function! go#auto#fmt_autosave()
  if !go#config#FmtAutosave() || !filereadable(expand('%:p'))
    return
  endif

  " Go code formatting on save
  call go#fmt#Format(-1)
endfunction

function! go#auto#metalinter_autosave()
  if !go#config#MetalinterAutosave() || !filereadable(expand('%:p'))
    return
  endif

  " run gometalinter on save
  call go#lint#Gometa(!g:go_jump_to_error, 1)
endfunction

function! go#auto#modfmt_autosave()
  if !go#config#ModFmtAutosave() || !filereadable(expand('%:p'))
    return
  endif

  " go.mod code formatting on save
  call go#mod#Format()
endfunction

function! go#auto#asmfmt_autosave()
  if !go#config#AsmfmtAutosave() || !filereadable(expand('%:p'))
    return
  endif

  " Go asm formatting on save
  call go#asmfmt#Format()
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
