" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.

let s:buf_nr = -1

if !exists("g:go_doc_command")
    let g:go_doc_command = "godoc"
endif

if !exists("g:go_doc_options")
    let g:go_doc_options = ""
endif

" returns the package and exported name. exported name might be empty.
" ie: fmt and Println
" ie: github.com/fatih/set and New
function! s:godocWord(args)
    if !executable('godoc')
        let msg = "godoc command not found."
        let msg .= "  install with: go get golang.org/x/tools/cmd/godoc"
        call go#util#echoWarning(msg)
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

function! s:godocNotFound(content)
    if len(a:content) == 0
        return 1
    endif

    return a:content =~# '^.*: no such file or directory\n$'
endfunction

function! go#doc#OpenBrowser(...)
    let pkgs = s:godocWord(a:000)
    if empty(pkgs)
        return
    endif

    let pkg = pkgs[0]
    let exported_name = pkgs[1]

    " example url: https://godoc.org/github.com/fatih/set#Set
    let godoc_url = "https://godoc.org/" . pkg . "#" . exported_name
    call go#tool#OpenBrowser(godoc_url)
endfunction

function! go#doc#Open(newmode, mode, ...)
    " check if we have 'gogetdoc' and use it automatically
    let bin_path = go#path#CheckBinPath('gogetdoc')
    if empty(bin_path)
        return
    endif

    let offset = go#util#OffsetCursor()
    let fname = expand("%:p")

    let command = printf("%s -pos %s:#%s", bin_path, fname, offset)

    let out = go#util#System(command)
    if go#util#ShellError() != 0
        call go#util#EchoError(out)
        return
    endif

    call s:GodocView(a:newmode, a:mode, out)
endfunction

function! s:GodocView(newposition, position, content)
    " reuse existing buffer window if it exists otherwise create a new one
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

    " cap buffer height to 20, but resize it for smaller contents
    let max_height = 20
    let content_height = len(split(a:content, "\n"))
    if content_height > max_height
        exe 'resize ' . max_height
    else
        exe 'resize ' . content_height
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

    " close easily with <esc> or enter
    noremap <buffer> <silent> <CR> :<C-U>close<CR>
    noremap <buffer> <silent> <Esc> :<C-U>close<CR>
endfunction

" vim:ts=2:sw=2:et
