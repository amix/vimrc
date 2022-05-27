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

function! go#auto#complete_done()
  if &omnifunc isnot 'go#complete#Complete'
    return
  endif

  call s:echo_go_info()
  call s:ExpandSnippet()
endfunction

function! s:ExpandSnippet() abort
  if !exists('v:completed_item') || empty(v:completed_item) || !go#config#GoplsUsePlaceholders()
    return
  endif


  let l:engine = go#config#SnippetEngine()

  if l:engine is 'ultisnips'
    if !get(g:, 'did_plugin_ultisnips', 0)
      return
    endif
    " the snippet may have a '{\}' in it. For UltiSnips, that should be spelled
    " \{}. fmt.Printf is such a snippet that can be used to demonstrate.
    let l:snippet = substitute(v:completed_item.word, '{\\}', '\{}', 'g')
    call UltiSnips#Anon(l:snippet, v:completed_item.word, '', 'i')
"  elseif l:engine is 'neosnippet'
"    " TODO(bc): make the anonymous expansion for neosnippet work
"
"    if !get(g:, 'loaded_neosnippet') is 1
"      return
"    endif
"
"    " neosnippet#anonymous doesn't need a trigger, so replace the
"    " completed_item.word with an empty string before calling neosnippet#anonymous
"    let l:snippet = substitute(v:completed_item.word, '{\\}', '\{\}', 'g')
"    call setline('.', substitute(getline('.'), substitute(v:completed_item.word, '\', '\\', 'g'), '', ''))
"    call neosnippet#anonymous(l:snippet)
  endif
endfunction

function! s:echo_go_info()
  if !go#config#EchoGoInfo()
    return
  endif

  if !exists('v:completed_item') || empty(v:completed_item)
    return
  endif
  let item = v:completed_item

  if !has_key(item, "user_data")
    return
  endif

  if empty(item.user_data)
    return
  endif

  redraws! | echo "vim-go: " | echohl Function | echon item.user_data | echohl None
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
  if !(isdirectory(expand('%:p:h')) && resolve(expand('<afile>:p')) == expand('%:p'))
    return
  endif

  if !(go#config#FmtAutosave() || go#config#ImportsAutosave())
    return
  endif

  " Order matters when formatting and adjusting imports, because of gopls'
  " support for gofumpt. Gofumpt formatting will group all imports that look
  " like a stdlib package (e.g. there's no '.' in the package path) together.
  " When the local setting is provided, the only way to get the local imports
  " grouped separately when gofumpt is used to format is to format first and
  " then organize imports.

  if go#config#FmtAutosave() && !(go#config#ImportsAutosave() && go#config#ImportsMode() == 'goimports')
    call go#fmt#Format(0)

    " return early when the imports mode is goimports, because there's no need
    " to format again when goimports was run
    if go#config#FmtCommand() == 'goimports'
      return
    endif
  endif

  if !go#config#ImportsAutosave()
    return
  endif

  call go#fmt#Format(1)
endfunction

function! go#auto#metalinter_autosave()
  if !go#config#MetalinterAutosave() || !isdirectory(expand('%:p:h'))
    return
  endif

  " run gometalinter on save
  call go#lint#Gometa(!g:go_jump_to_error, 1)
endfunction

function! go#auto#modfmt_autosave()
  if !(go#config#ModFmtAutosave() && isdirectory(expand('%:p:h')) && resolve(expand('<afile>:p')) == expand('%:p'))
    return
  endif

  " go.mod code formatting on save
  call go#mod#Format()
endfunction

function! go#auto#asmfmt_autosave()
  if !(go#config#AsmfmtAutosave() && isdirectory(expand('%:p:h')) && resolve(expand('<afile>:p')) == expand('%:p'))
    return
  endif

  " Go asm formatting on save
  call go#asmfmt#Format()
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
