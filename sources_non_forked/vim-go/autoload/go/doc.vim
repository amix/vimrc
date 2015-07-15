" Copyright 2011 The Go Authors. All rights reserved.
" Use of this source code is governed by a BSD-style
" license that can be found in the LICENSE file.
"
" godoc.vim: Vim command to see godoc.
"
"
" Commands:
"
"   :GoDoc
"
"       Open the relevant Godoc for either the word[s] passed to the command or
"       the, by default, the word under the cursor.
"
" Options:
"
"   g:go_godoc_commands [default=1]
"
"       Flag to indicate whether to enable the commands listed above.

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
        echohl WarningMsg
        echo "godoc command not found."
        echo "  install with: go get golang.org/x/tools/cmd/godoc"
        echohl None
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
    let pkgs = s:godocWord(a:000)
    if empty(pkgs)
        return
    endif

    let pkg = pkgs[0]
    let exported_name = pkgs[1]

    let command = g:go_doc_command . ' ' . g:go_doc_options . ' ' . pkg

    silent! let content = system(command)
    if v:shell_error || s:godocNotFound(content)
        echo 'No documentation found for "' . pkg . '".'
        return -1
    endif

    call s:GodocView(a:newmode, a:mode, content)

    if exported_name == ''
        silent! normal gg
        return -1
    endif

    " jump to the specified name
    if search('^func ' . exported_name . '(')
        silent! normal zt
        return -1
    endif

    if search('^type ' . exported_name)
        silent! normal zt
        return -1
    endif

    if search('^\%(const\|var\|type\|\s\+\) ' . pkg . '\s\+=\s')
        silent! normal zt
        return -1
    endif

    " nothing found, jump to top
    silent! normal gg
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
endfunction


" vim:ts=4:sw=4:et
