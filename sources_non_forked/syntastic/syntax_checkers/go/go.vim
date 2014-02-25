"============================================================================
"File:        go.vim
"Description: Check go syntax using 'gofmt -l' followed by 'go [build|test]'
"Maintainer:  Kamil Kisiel <kamil@kamilkisiel.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" This syntax checker does not reformat your source code.
" Use a BufWritePre autocommand to that end:
"   autocmd FileType go autocmd BufWritePre <buffer> Fmt
"============================================================================
if exists("g:loaded_syntastic_go_go_checker")
    finish
endif
let g:loaded_syntastic_go_go_checker=1

function! SyntaxCheckers_go_go_IsAvailable()
    return executable('go')
endfunction

function! SyntaxCheckers_go_go_GetLocList()
    " Check with gofmt first, since `go build` and `go test` might not report
    " syntax errors in the current file if another file with syntax error is
    " compiled first.
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'gofmt',
        \ 'args': '-l',
        \ 'tail': '1>' . syntastic#util#DevNull(),
        \ 'filetype': 'go',
        \ 'subchecker': 'go' })

    let errorformat =
        \ '%f:%l:%c: %m,' .
        \ '%-G%.%#'

    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'type': 'e'} })
    if !empty(errors)
        return errors
    endif

    " Test files, i.e. files with a name ending in `_test.go`, are not
    " compiled by `go build`, therefore `go test` must be called for those.
    if match(expand('%'), '_test.go$') == -1
        let makeprg = 'go build ' . syntastic#c#GetNullDevice()
    else
        let makeprg = 'go test -c ' . syntastic#c#GetNullDevice()
    endif

    let errorformat =
        \ '%f:%l:%c:%m,' .
        \ '%f:%l%m,' .
        \ '%-G#%.%#'

    " The go compiler needs to either be run with an import path as an
    " argument or directly from the package directory. Since figuring out
    " the proper import path is fickle, just cwd to the package.

    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': expand('%:p:h'),
        \ 'defaults': {'type': 'e'} })

    return errors
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'go'})
