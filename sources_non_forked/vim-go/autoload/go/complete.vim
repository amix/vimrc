let s:sock_type = (has('win32') || has('win64')) ? 'tcp' : 'unix'

function! s:gocodeCommand(cmd, args) abort
  let bin_path = go#path#CheckBinPath("gocode")
  if empty(bin_path)
    return []
  endif

  let socket_type = get(g:, 'go_gocode_socket_type', s:sock_type)

  let cmd = [bin_path]
  let cmd = extend(cmd, ['-sock', socket_type])
  let cmd = extend(cmd, ['-f', 'vim'])
  let cmd = extend(cmd, [a:cmd])
  let cmd = extend(cmd, a:args)

  return cmd
endfunction

function! s:sync_gocode(cmd, args, input) abort
  " We might hit cache problems, as gocode doesn't handle different GOPATHs
  " well. See: https://github.com/nsf/gocode/issues/239
  let old_goroot = $GOROOT
  let $GOROOT = go#util#env("goroot")

  try
    let cmd = s:gocodeCommand(a:cmd, a:args)
    " gocode can sometimes be slow, so redraw now to avoid waiting for gocode
    " to return before redrawing automatically.
    redraw

    let [l:result, l:err] = go#util#Exec(cmd, a:input)
  finally
    let $GOROOT = old_goroot
  endtry

  if l:err != 0
    return "[0, []]"
  endif

  if &encoding != 'utf-8'
    let l:result = iconv(l:result, 'utf-8', &encoding)
  endif

  return l:result
endfunction

" TODO(bc): reset when gocode isn't running
let s:optionsEnabled = 0
function! s:gocodeEnableOptions() abort
  if s:optionsEnabled
    return
  endif

  let bin_path = go#path#CheckBinPath("gocode")
  if empty(bin_path)
    return
  endif

  let s:optionsEnabled = 1

  call go#util#System(printf('%s set propose-builtins %s', go#util#Shellescape(bin_path), s:toBool(get(g:, 'go_gocode_propose_builtins', 1))))
  call go#util#System(printf('%s set autobuild %s', go#util#Shellescape(bin_path), s:toBool(get(g:, 'go_gocode_autobuild', 1))))
  call go#util#System(printf('%s set unimported-packages %s', go#util#Shellescape(bin_path), s:toBool(get(g:, 'go_gocode_unimported_packages', 0))))
endfunction

function! s:toBool(val) abort
  if a:val | return 'true ' | else | return 'false' | endif
endfunction

function! s:gocodeAutocomplete() abort
  call s:gocodeEnableOptions()

  " use the offset as is, because the cursor position is the position for
  " which autocomplete candidates are needed.
  return s:sync_gocode('autocomplete',
        \ [expand('%:p'), go#util#OffsetCursor()],
        \ go#util#GetLines())
endfunction

" go#complete#GoInfo returns the description of the identifier under the
" cursor.
function! go#complete#GetInfo() abort
  return s:sync_info(0)
endfunction

function! go#complete#Info(auto) abort
  if go#util#has_job()
    return s:async_info(a:auto)
  else
    return s:sync_info(a:auto)
  endif
endfunction

function! s:async_info(auto)
  if exists("s:async_info_job")
    call job_stop(s:async_info_job)
    unlet s:async_info_job
  endif

  let state = {
        \ 'exited': 0,
        \ 'exit_status': 0,
        \ 'closed': 0,
        \ 'messages': [],
        \ 'auto': a:auto
      \ }

  function! s:callback(chan, msg) dict
    let l:msg = a:msg
    if &encoding != 'utf-8'
      let l:msg = iconv(l:msg, 'utf-8', &encoding)
    endif
    call add(self.messages, l:msg)
  endfunction

  function! s:exit_cb(job, exitval) dict
    let self.exit_status = a:exitval
    let self.exited = 1

    if self.closed
      call self.complete()
    endif
  endfunction

  function! s:close_cb(ch) dict
    let self.closed = 1
    if self.exited
      call self.complete()
    endif
  endfunction

  function state.complete() dict
    if self.exit_status != 0
      return
    endif

    let result = s:info_filter(self.auto, join(self.messages, "\n"))
    call s:info_complete(self.auto, result)
  endfunction

  " add 1 to the offset, so that the position at the cursor will be included
  " in gocode's search
  let offset = go#util#OffsetCursor()+1

  " We might hit cache problems, as gocode doesn't handle different GOPATHs
  " well. See: https://github.com/nsf/gocode/issues/239
  let env = {
    \ "GOROOT": go#util#env("goroot")
    \ }

  let cmd = s:gocodeCommand('autocomplete',
        \ [expand('%:p'), offset])

  " TODO(bc): Don't write the buffer to a file; pass the buffer directrly to
  " gocode's stdin. It shouldn't be necessary to use {in_io: 'file', in_name:
  " s:gocodeFile()}, but unfortunately {in_io: 'buffer', in_buf: bufnr('%')}
  " should work.
  let options = {
        \ 'env': env,
        \ 'in_io': 'file',
        \ 'in_name': s:gocodeFile(),
        \ 'callback': funcref("s:callback", [], state),
        \ 'exit_cb': funcref("s:exit_cb", [], state),
        \ 'close_cb': funcref("s:close_cb", [], state)
      \ }

  let s:async_info_job = job_start(cmd, options)
endfunction

function! s:gocodeFile()
  let file = tempname()
  call writefile(go#util#GetLines(), file)
  return file
endfunction

function! s:sync_info(auto)
  " auto is true if we were called by g:go_auto_type_info's autocmd

  " add 1 to the offset, so that the position at the cursor will be included
  " in gocode's search
  let offset = go#util#OffsetCursor()+1

  let result = s:sync_gocode('autocomplete',
        \ [expand('%:p'), offset],
        \ go#util#GetLines())

  let result = s:info_filter(a:auto, result)
  call s:info_complete(a:auto, result)
endfunction

function! s:info_filter(auto, result) abort
  if empty(a:result)
    return ""
  endif

  let l:result = eval(a:result)
  if len(l:result) != 2
    return ""
  endif

  let l:candidates = l:result[1]
  if len(l:candidates) == 1
    " When gocode panics in vim mode, it returns
    "     [0, [{'word': 'PANIC', 'abbr': 'PANIC PANIC PANIC', 'info': 'PANIC PANIC PANIC'}]]
    if a:auto && l:candidates[0].info ==# "PANIC PANIC PANIC"
      return ""
    endif

    return l:candidates[0].info
  endif

  let filtered = []
  let wordMatch = '\<' . expand("<cword>") . '\>'
  " escape single quotes in wordMatch before passing it to filter
  let wordMatch = substitute(wordMatch, "'", "''", "g")
  let filtered = filter(l:candidates, "v:val.info =~ '".wordMatch."'")

  if len(l:filtered) != 1
    return ""
  endif

  return l:filtered[0].info
endfunction

function! s:info_complete(auto, result) abort
  if !empty(a:result)
    echo "vim-go: " | echohl Function | echon a:result | echohl None
  endif
endfunction

function! s:trim_bracket(val) abort
  let a:val.word = substitute(a:val.word, '[(){}\[\]]\+$', '', '')
  return a:val
endfunction

let s:completions = ""
function! go#complete#Complete(findstart, base) abort
  "findstart = 1 when we need to get the text length
  if a:findstart == 1
    execute "silent let s:completions = " . s:gocodeAutocomplete()
    return col('.') - s:completions[0] - 1
    "findstart = 0 when we need to return the list of completions
  else
    let s = getline(".")[col('.') - 1]
    if s =~ '[(){}\{\}]'
      return map(copy(s:completions[1]), 's:trim_bracket(v:val)')
    endif

    return s:completions[1]
  endif
endfunction

function! go#complete#ToggleAutoTypeInfo() abort
  if get(g:, "go_auto_type_info", 0)
    let g:go_auto_type_info = 0
    call go#util#EchoProgress("auto type info disabled")
    return
  end

  let g:go_auto_type_info = 1
  call go#util#EchoProgress("auto type info enabled")
endfunction

" vim: sw=2 ts=2 et
