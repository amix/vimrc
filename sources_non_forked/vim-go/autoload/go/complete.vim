" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! s:gocodeCommand(cmd, args) abort
  let l:gocode_bin = "gocode"
  let l:gomod = go#util#gomod()
  if filereadable(l:gomod)
    let l:gocode_bin = "gocode-gomod"
  endif

  let bin_path = go#path#CheckBinPath(l:gocode_bin)
  if empty(bin_path)
    return []
  endif

  let socket_type = go#config#GocodeSocketType()

  let cmd = [bin_path]
  let cmd = extend(cmd, ['-sock', socket_type])
  let cmd = extend(cmd, ['-f', 'vim'])

  if go#config#GocodeProposeBuiltins()
    let cmd = extend(cmd, ['-builtin'])
  endif

  if go#config#GocodeProposeSource()
    let cmd = extend(cmd, ['-source'])
  else
    let cmd = extend(cmd, ['-fallback-to-source', '-cache'])
  endif

  if go#config#GocodeUnimportedPackages()
    let cmd = extend(cmd, ['-unimported-packages'])
  endif

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

function! s:gocodeAutocomplete() abort
  " use the offset as is, because the cursor position is the position for
  " which autocomplete candidates are needed.
  return s:sync_gocode('autocomplete',
        \ [expand('%:p'), go#util#OffsetCursor()],
        \ go#util#GetLines())
endfunction

" go#complete#GoInfo returns the description of the identifier under the
" cursor.
function! go#complete#GetInfo() abort
  let l:mode = go#config#InfoMode()
  if l:mode == 'gopls' && go#util#has_job()
    return go#lsp#GetInfo()
  else
    return s:sync_info(0)
  endif
endfunction

function! go#complete#Info(showstatus) abort
  if go#util#has_job(1)
    return s:async_info(1, a:showstatus)
  else
    return s:sync_info(1)
  endif
endfunction

function! s:async_info(echo, showstatus)
  let state = {'echo': a:echo}

  " explicitly bind complete to state so that within it, self will
  " always refer to state. See :help Partial for more information.
  let state.complete = function('s:complete', [], state)

  " add 1 to the offset, so that the position at the cursor will be included
  " in gocode's search
  let offset = go#util#OffsetCursor()+1

  " We might hit cache problems, as gocode doesn't handle different GOPATHs
  " well. See: https://github.com/nsf/gocode/issues/239
  let env = {
    \ "GOROOT": go#util#env("goroot")
    \ }

  let opts = {
        \ 'bang': 1,
        \ 'complete': state.complete,
        \ 'for': '_',
        \ }

  if a:showstatus
    let opts.statustype = 'gocode'
  endif

  let opts = go#job#Options(l:opts)

  let cmd = s:gocodeCommand('autocomplete',
        \ [expand('%:p'), offset])

  " TODO(bc): Don't write the buffer to a file; pass the buffer directly to
  " gocode's stdin. It shouldn't be necessary to use {in_io: 'file', in_name:
  " s:gocodeFile()}, but unfortunately {in_io: 'buffer', in_buf: bufnr('%')}
  " doesn't work.
  call extend(opts, {
        \ 'env': env,
        \ 'in_io': 'file',
        \ 'in_name': s:gocodeFile(),
        \ })

  call go#job#Start(cmd, opts)
endfunction

function! s:complete(job, exit_status, messages) abort dict
  if a:exit_status != 0
    return
  endif

  if &encoding != 'utf-8'
    let i = 0
    while i < len(a:messages)
      let a:messages[i] = iconv(a:messages[i], 'utf-8', &encoding)
      let i += 1
    endwhile
  endif

  let result = s:info_filter(self.echo, join(a:messages, "\n"))
  call s:info_complete(self.echo, result)
endfunction

function! s:gocodeFile()
  let file = tempname()
  call writefile(go#util#GetLines(), file)
  return file
endfunction

function! s:sync_info(echo)
  " add 1 to the offset, so that the position at the cursor will be included
  " in gocode's search
  let offset = go#util#OffsetCursor()+1

  let result = s:sync_gocode('autocomplete',
        \ [expand('%:p'), offset],
        \ go#util#GetLines())

  let result = s:info_filter(a:echo, result)
  return s:info_complete(a:echo, result)
endfunction

function! s:info_filter(echo, result) abort
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
    if a:echo && l:candidates[0].info ==# "PANIC PANIC PANIC"
      return ""
    endif

    return l:candidates[0].info
  endif

  let filtered = []
  let wordMatch = '\<' . expand("<cword>") . '\>'
  " escape single quotes in wordMatch before passing it to filter
  let wordMatch = substitute(wordMatch, "'", "''", "g")
  let filtered = filter(l:candidates, "v:val.info =~ '".wordMatch."'")

  if len(l:filtered) == 0
    return "no matches"
  elseif len(l:filtered) > 1
    return "ambiguous match"
  endif

  return l:filtered[0].info
endfunction

function! s:info_complete(echo, result) abort
  if a:echo
    call go#util#ShowInfo(a:result)
  endif

  return a:result
endfunction

function! s:trim_bracket(val) abort
  echom a:val
  let a:val.word = substitute(a:val.word, '[(){}\[\]]\+$', '', '')
  return a:val
endfunction

let s:completions = []

function! go#complete#GocodeComplete(findstart, base) abort
  "findstart = 1 when we need to get the text length
  if a:findstart == 1
    let l:completions = []
    execute "silent let l:completions = " . s:gocodeAutocomplete()

    if len(l:completions) == 0 || len(l:completions) >= 2 && len(l:completions[1]) == 0
      " no matches. cancel and leave completion mode.
      call go#util#EchoInfo("no matches")
      return -3
    endif

    let s:completions = l:completions[1]
    return col('.') - l:completions[0] - 1
    "findstart = 0 when we need to return the list of completions
  else
    let s = getline(".")[col('.') - 1]
    if s =~ '[(){}\{\}]'
      return map(copy(s:completions), 's:trim_bracket(v:val)')
    endif
    return s:completions
  endif
endfunction

function! go#complete#Complete(findstart, base) abort
  let l:state = {'done': 0, 'matches': [], 'start': -1}

  function! s:handler(state, start, matches) abort dict
    let a:state.start = a:start
    let a:state.matches = a:matches
    let a:state.done = 1
  endfunction

  "findstart = 1 when we need to get the start of the match
  if a:findstart == 1
    let [l:line, l:col] = getpos('.')[1:2]
    let [l:line, l:col] = go#lsp#lsp#Position(l:line, l:col)
    let l:completion = go#lsp#Completion(expand('%:p'), l:line, l:col, funcref('s:handler', [l:state]))
    if l:completion
      return -3
    endif

    while !l:state.done
      sleep 10m
    endwhile

    if len(l:state.matches) == 0
      " no matches. cancel and leave completion mode.
      call go#util#EchoInfo("no matches")
      return -3
    endif

    let s:completions = l:state.matches

    return go#lsp#lsp#PositionOf(getline(l:line+1), l:state.start-1)

  else "findstart = 0 when we need to return the list of completions
    return s:completions
  endif
endfunction

function! go#complete#ToggleAutoTypeInfo() abort
  if go#config#AutoTypeInfo()
    call go#config#SetAutoTypeInfo(0)
    call go#util#EchoProgress("auto type info disabled")
    return
  end

  call go#config#SetAutoTypeInfo(1)
  call go#util#EchoProgress("auto type info enabled")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
