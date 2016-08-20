function! s:gocodeCurrentBuffer()
  let buf = getline(1, '$')
  if &encoding != 'utf-8'
    let buf = map(buf, 'iconv(v:val, &encoding, "utf-8")')
  endif
  if &l:fileformat == 'dos'
    " XXX: line2byte() depend on 'fileformat' option.
    " so if fileformat is 'dos', 'buf' must include '\r'.
    let buf = map(buf, 'v:val."\r"')
  endif
  let file = tempname()
  call writefile(buf, file)

  return file
endfunction

function! s:gocodeCommand(cmd, preargs, args)
  for i in range(0, len(a:args) - 1)
    let a:args[i] = go#util#Shellescape(a:args[i])
  endfor
  for i in range(0, len(a:preargs) - 1)
    let a:preargs[i] = go#util#Shellescape(a:preargs[i])
  endfor

  let bin_path = go#path#CheckBinPath("gocode")
  if empty(bin_path)
    return
  endif

  " we might hit cache problems, as gocode doesn't handle well different
  " GOPATHS: https://github.com/nsf/gocode/issues/239
  let old_gopath = $GOPATH
  let $GOPATH = go#path#Detect()

  let socket_type = get(g:, 'go_gocode_socket_type', 'unix')
  let cmd = printf('%s -sock %s %s %s %s', 
        \ go#util#Shellescape(bin_path), 
        \ socket_type, 
        \ join(a:preargs), 
        \ go#util#Shellescape(a:cmd), 
        \ join(a:args)
        \ )

  let result = go#util#System(cmd)
  let $GOPATH = old_gopath
  if go#util#ShellError() != 0
    return "[\"0\", []]"
  else
    if &encoding != 'utf-8'
      let result = iconv(result, 'utf-8', &encoding)
    endif
    return result
  endif
endfunction

function! s:gocodeCurrentBufferOpt(filename)
  return '-in=' . a:filename
endfunction

let s:optionsEnabled = 0
function! s:gocodeEnableOptions()
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
endfunction

function! s:toBool(val)
  if a:val | return 'true ' | else | return 'false' | endif
endfunction

function! s:gocodeAutocomplete()
  call s:gocodeEnableOptions()

  let filename = s:gocodeCurrentBuffer()
  let result = s:gocodeCommand('autocomplete',
        \ [s:gocodeCurrentBufferOpt(filename), '-f=vim'],
        \ [expand('%:p'), go#util#OffsetCursor()])
  call delete(filename)
  return result
endfunction

function! go#complete#GetInfo()
  let offset = go#util#OffsetCursor()+1
  let filename = s:gocodeCurrentBuffer()
  let result = s:gocodeCommand('autocomplete',
        \ [s:gocodeCurrentBufferOpt(filename), '-f=godit'],
        \ [expand('%:p'), offset])
  call delete(filename)

  " first line is: Charcount,,NumberOfCandidates, i.e: 8,,1
  " following lines are candiates, i.e:  func foo(name string),,foo(
  let out = split(result, '\n')

  " no candidates are found
  if len(out) == 1
    return ""
  endif

  " only one candiate is found
  if len(out) == 2
    return split(out[1], ',,')[0]
  endif

  " to many candidates are available, pick one that maches the word under the
  " cursor
  let infos = []
  for info in out[1:]
    call add(infos, split(info, ',,')[0])
  endfor

  let wordMatch = '\<' . expand("<cword>") . '\>'
  " escape single quotes in wordMatch before passing it to filter
  let wordMatch = substitute(wordMatch, "'", "''", "g")
  let filtered =  filter(infos, "v:val =~ '".wordMatch."'")

  if len(filtered) == 1
    return filtered[0]
  endif

  return ""
endfunction

function! go#complete#Info(auto)
  " auto is true if we were called by g:go_auto_type_info's autocmd
  let result = go#complete#GetInfo()
  if !empty(result)
    " if auto, and the result is a PANIC by gocode, hide it
    if a:auto && result ==# 'PANIC PANIC PANIC' | return | endif
    echo "vim-go: " | echohl Function | echon result | echohl None
  endif
endfunction

function! s:trim_bracket(val)
  let a:val.word = substitute(a:val.word, '[(){}\[\]]\+$', '', '')
  return a:val
endfunction

function! go#complete#Complete(findstart, base)
  "findstart = 1 when we need to get the text length
  if a:findstart == 1
    execute "silent let g:gocomplete_completions = " . s:gocodeAutocomplete()
    return col('.') - g:gocomplete_completions[0] - 1
    "findstart = 0 when we need to return the list of completions
  else
    let s = getline(".")[col('.') - 1]
    if s =~ '[(){}\{\}]'
      return map(copy(g:gocomplete_completions[1]), 's:trim_bracket(v:val)')
    endif
    return g:gocomplete_completions[1]
  endif
endf

function! go#complete#ToggleAutoTypeInfo()
  if get(g:, "go_auto_type_info", 0)
    let g:go_auto_type_info = 0
    call go#util#EchoProgress("auto type info disabled")
    return
  end

  let g:go_auto_type_info = 1
  call go#util#EchoProgress("auto type info enabled")
endfunction


" vim: sw=2 ts=2 et
