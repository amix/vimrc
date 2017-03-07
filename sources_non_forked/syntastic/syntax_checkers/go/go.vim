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
"============================================================================
"
" This syntax checker does not reformat your source code.
" Use a BufWritePre autocommand to that end:
"   autocmd FileType go autocmd BufWritePre <buffer> Fmt

if exists('g:loaded_syntastic_go_go_checker')
    finish
endif
let g:loaded_syntastic_go_go_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_go_IsAvailable() dict
    return executable(self.getExec()) && executable('gofmt')
endfunction

function! SyntaxCheckers_go_go_GetLocList() dict
    if !exists('s:go_new')
        let s:go_new = syntastic#util#versionIsAtLeast(self.getVersion(self.getExecEscaped() . ' version'), [1, 5])
    endif
    let buf = bufnr('')

    " Check with gofmt first, since `go build` and `go test` might not report
    " syntax errors in the current file if another file with syntax error is
    " compiled first.
    let makeprg = self.makeprgBuild({
        \ 'exe': 'gofmt',
        \ 'args': '-l',
        \ 'tail': '> ' . syntastic#util#DevNull() })

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
    if bufname(buf) =~# '\m_test\.go$'
        let cmd = 'test -c'
        let opts = syntastic#util#bufVar(buf, 'go_go_test_args', s:go_new ? '-buildmode=archive' : '')
        let cleanup = 1
    else
        let cmd = 'build'
        let opts = syntastic#util#bufVar(buf, 'go_go_build_args', s:go_new ? '-buildmode=archive' : '')
        let cleanup = 0
    endif
    let opt_str = (type(opts) != type('') || opts !=# '') ? join(syntastic#util#argsescape(opts)) : opts
    let makeprg = self.getExecEscaped() . ' ' . cmd . ' ' . opt_str

    " The first pattern is for warnings from C compilers.
    let errorformat =
        \ '%W%f:%l: warning: %m,' .
        \ '%E%f:%l:%c:%m,' .
        \ '%E%f:%l:%m,' .
        \ '%C%\s%\+%m,' .
        \ '%+Ecan''t load package: %m,' .
        \ '%+Einternal error: %m,' .
        \ '%-G#%.%#'

    " The go compiler needs to either be run with an import path as an
    " argument or directly from the package directory. Since figuring out
    " the proper import path is fickle, just cwd to the package.

    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': fnamemodify(bufname(buf), ':p:h'),
        \ 'env': {'GOGC': 'off'},
        \ 'defaults': {'type': 'e'} })

    if cleanup
        call delete(fnamemodify(bufname(buf), ':p:h') . syntastic#util#Slash() . fnamemodify(bufname(buf), ':p:h') . '.test')
    endif

    return errors
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'go'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
