" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.

let s:buf_nr = -1

if !exists("g:go_doc_command")
  let g:go_doc_command = ["godoc"]
endif

function! go#doc#OpenBrowser(...) abort
  " check if we have gogetdoc as it gives us more and accurate information.
  " Only supported if we have json_decode as it's not worth to parse the plain
  " non-json output of gogetdoc
  let bin_path = go#path#CheckBinPath('gogetdoc')
  if !empty(bin_path) && exists('*json_decode')
    let json_out = s:gogetdoc(1)
    if go#util#ShellError() != 0
      call go#util#EchoError(json_out)
      return
    endif

    let out = json_decode(json_out)
    if type(out) != type({})
      call go#util#EchoError("gogetdoc output is malformed")
    endif

    let import = out["import"]
    let name = out["name"]
    let decl = out["decl"]

    let godoc_url = s:custom_godoc_url()
    let godoc_url .= "/" . import
    if decl !~ "^package"
      let godoc_url .= "#" . name
    endif

    echo godoc_url

    call go#tool#OpenBrowser(godoc_url)
    return
  endif

  let pkgs = s:godocWord(a:000)
  if empty(pkgs)
    return
  endif

  let pkg = pkgs[0]
  let exported_name = pkgs[1]

  " example url: https://godoc.org/github.com/fatih/set#Set
  let godoc_url = s:custom_godoc_url() . "/" . pkg . "#" . exported_name
  call go#tool#OpenBrowser(godoc_url)
endfunction

function! go#doc#Open(newmode, mode, ...) abort
  " With argument: run "godoc [arg]".
  if len(a:000)
    if empty(go#path#CheckBinPath(g:go_doc_command[0]))
      return
    endif

    let command = printf("%s %s", go#util#Shelljoin(g:go_doc_command), join(a:000, ' '))
    let out = go#util#System(command)
  " Without argument: run gogetdoc on cursor position.
  else
    let out = s:gogetdoc(0)
    if out == -1
      return
    endif
  endif

  if go#util#ShellError() != 0
    call go#util#EchoError(out)
    return
  endif

  call s:GodocView(a:newmode, a:mode, out)
endfunction

function! s:GodocView(newposition, position, content) abort
  " reuse existing buffer window if it exists otherwise create a new one
  let is_visible = bufexists(s:buf_nr) && bufwinnr(s:buf_nr) != -1
  if !bufexists(s:buf_nr)
    execute a:newposition
    sil file `="[Godoc]"`
    let s:buf_nr = bufnr('%')
  elseif bufwinnr(s:buf_nr) == -1
    execute a:position
    execute s:buf_nr . 'buffer'
  elseif bufwinnr(s:buf_nr) != bufwinnr('%')
    execute bufwinnr(s:buf_nr) . 'wincmd w'
  endif

  " if window was not visible then resize it
  if !is_visible
    if a:position == "split"
      " cap window height to 20, but resize it for smaller contents
      let max_height = get(g:, "go_doc_max_height", 20)
      let content_height = len(split(a:content, "\n"))
      if content_height > max_height
        exe 'resize ' . max_height
      else
        exe 'resize ' . content_height
      endif
    else
      " set a sane maximum width for vertical splits. In this case the minimum
      " that fits the godoc for package http without extra linebreaks and line
      " numbers on
      exe 'vertical resize 84'
    endif
  endif

  setlocal filetype=godoc
  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nocursorline
  setlocal nocursorcolumn
  setlocal iskeyword+=:
  setlocal iskeyword-=-

  setlocal modifiable
  %delete _
  call append(0, split(a:content, "\n"))
  sil $delete _
  setlocal nomodifiable
  sil normal! gg

  " close easily with <esc> or enter
  noremap <buffer> <silent> <CR> :<C-U>close<CR>
  noremap <buffer> <silent> <Esc> :<C-U>close<CR>
endfunction

function! s:gogetdoc(json) abort
  " check if we have 'gogetdoc' and use it automatically
  let bin_path = go#path#CheckBinPath('gogetdoc')
  if empty(bin_path)
    return -1
  endif

  let cmd = [go#util#Shellescape(bin_path)]

  let offset = go#util#OffsetCursor()
  let fname = expand("%:p:gs!\\!/!")
  let pos = shellescape(fname.':#'.offset)

  let cmd += ["-pos", pos]
  if a:json
    let cmd += ["-json"]
  endif

  let command = join(cmd, " ")

  if &modified
    let command .= " -modified"
    let out = go#util#System(command, go#util#archive())
  else
    let out = go#util#System(command)
  endif

  return out
endfunction

" returns the package and exported name. exported name might be empty.
" ie: fmt and Println
" ie: github.com/fatih/set and New
function! s:godocWord(args) abort
  if !executable('godoc')
    let msg = "godoc command not found."
    let msg .= "  install with: go get golang.org/x/tools/cmd/godoc"
    call go#util#EchoWarning(msg)
    return []
  endif

  if !len(a:args)
    let oldiskeyword = &iskeyword
    setlocal iskeyword+=.
    let word = expand('<cword>')
    let &iskeyword = oldiskeyword
    let word = substitute(word, '[^a-zA-Z0-9\\/._~-]', '', 'g')
    let words = split(word, '\.\ze[^./]\+$')
  else
    let words = a:args
  endif

  if !len(words)
    return []
  endif

  let pkg = words[0]
  if len(words) == 1
    let exported_name = ""
  else
    let exported_name = words[1]
  endif

  let packages = go#tool#Imports()

  if has_key(packages, pkg)
    let pkg = packages[pkg]
  endif

  return [pkg, exported_name]
endfunction

function! s:custom_godoc_url() abort
  let godoc_url = get(g:, 'go_doc_url', 'https://godoc.org')
  if godoc_url isnot 'https://godoc.org'
    " strip last '/' character if available
    let last_char = strlen(godoc_url) - 1
    if godoc_url[last_char] == '/'
      let godoc_url = strpart(godoc_url, 0, last_char)
    endif
    " custom godoc installations expect /pkg before package names
    let godoc_url .= "/pkg"
  endif
  return godoc_url
endfunction

" vim: sw=2 ts=2 et
