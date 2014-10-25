"============================================================================
"File:        clang_tidy.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Benjamin Bannier <bbannier at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_c_clang_tidy_checker")
  finish
endif
let g:loaded_syntastic_c_clang_tidy_checker = 1

if !exists('g:syntastic_clang_tidy_config_file')
    let g:syntastic_clang_tidy_config_file = '.syntastic_clang_tidy_config'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_clang_tidy_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'post_args':
        \   '-- ' .
        \   syntastic#c#ReadConfig(g:syntastic_clang_tidy_config_file) . ' ' .
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

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 1] })

    call self.setWantSort(1)

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'clang_tidy',
    \ 'exec': 'clang-tidy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
