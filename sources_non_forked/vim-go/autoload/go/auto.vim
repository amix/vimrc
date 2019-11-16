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

let s:timer_id = 0

" go#auto#update_autocmd() will be called on BufEnter,CursorHold. This
" configures the augroup according to conditions below.
"
" | # | has_timer | should_enable | do                                 |
" |---|-----------|---------------|------------------------------------|
" | 1 | false     | false         | return early                       |
" | 2 | true      | true          | return early                       |
" | 3 | true      | false         | clear the group and stop the timer |
" | 4 | false     | true          | configure the group                |
function! go#auto#update_autocmd()
  let has_timer = get(b:, 'has_timer')
  let should_enable = go#config#AutoTypeInfo() || go#config#AutoSameids()
  if (!has_timer && !should_enable) || (has_timer && should_enable)
    return
  endif

  if has_timer
    augroup vim-go-buffer-auto
      autocmd! * <buffer>
    augroup END
    let b:has_timer = 0
    call s:timer_stop()
    return
  endif

  augroup vim-go-buffer-auto
    autocmd! * <buffer>
    autocmd CursorMoved <buffer> call s:timer_restart()
    autocmd BufLeave <buffer> call s:timer_stop()
  augroup END
  let b:has_timer = 1
  call s:timer_start()
endfunction

function! s:timer_restart()
  if isdirectory(expand('%:p:h'))
    call s:timer_stop()
    call s:timer_start()
  endif
endfunction

function! s:timer_stop()
  if s:timer_id
    call timer_stop(s:timer_id)
    let s:timer_id = 0
  endif
endfunction

function! s:timer_start()
  let s:timer_id = timer_start(go#config#Updatetime(), function('s:handler'))
endfunction

function! s:handler(timer_id)
  if go#config#AutoTypeInfo()
    call go#tool#Info(0)
  endif
  if go#config#AutoSameids()
    call go#guru#SameIds(0)
  endif
  let s:timer_id = 0
endfunction

function! go#auto#fmt_autosave()
  if !(go#config#FmtAutosave() && isdirectory(expand('%:p:h')) && expand('<afile>:p') == expand('%:p'))
    return
  endif

  " Go code formatting on save
  call go#fmt#Format(-1)
endfunction

function! go#auto#metalinter_autosave()
  if !go#config#MetalinterAutosave() || !isdirectory(expand('%:p:h'))
    return
  endif

  " run gometalinter on save
  call go#lint#Gometa(!g:go_jump_to_error, 1)
endfunction

function! go#auto#modfmt_autosave()
  if !(go#config#ModFmtAutosave() && isdirectory(expand('%:p:h')) && expand('<afile>:p') == expand('%:p'))
    return
  endif

  " go.mod code formatting on save
  call go#mod#Format()
endfunction

function! go#auto#asmfmt_autosave()
  if !(go#config#AsmfmtAutosave() && isdirectory(expand('%:p:h')) && expand('<afile>:p') == expand('%:p'))
    return
  endif

  " Go asm formatting on save
  call go#asmfmt#Format()
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
