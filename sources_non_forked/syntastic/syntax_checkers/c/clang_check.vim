"============================================================================
"File:        clang_check.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Benjamin Bannier <bbannier at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_c_clang_check_checker')
    finish
endif
let g:loaded_syntastic_c_clang_check_checker = 1

if !exists('g:syntastic_clang_check_config_file')
    let g:syntastic_clang_check_config_file = '.syntastic_clang_check_config'
endif

if !exists('g:syntastic_c_clang_check_sort')
    let g:syntastic_c_clang_check_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_clang_check_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'post_args':
        \   '-- ' .
        \   syntastic#c#ReadConfig(g:syntastic_clang_check_config_file) . ' ' .
        \   '-fshow-column ' .
        \   '-fshow-source-location ' .
        \   '-fno-caret-diagnostics ' .
        \   '-fno-color-diagnostics ' .
        \   '-fdiagnostics-format=clang' })

    let errorformat =
        \ '%E%f:%l:%c: fatal error: %m,' .
        \ '%E%f:%l:%c: error: %m,' .
        \ '%W%f:%l:%c: warning: %m,' .
        \ '%-G%\m%\%%(LLVM ERROR:%\|No compilation database found%\)%\@!%.%#,' .
        \ '%E%m'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'clang_check',
    \ 'exec': 'clang-check'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
