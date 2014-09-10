"============================================================================
"File:        python.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_python_python_checker")
    finish
endif
let g:loaded_syntastic_python_python_checker = 1

let s:save_cpo = &cpo
set cpo&vim

let s:compiler = expand('<sfile>:p:h') . syntastic#util#Slash() . 'compile.py'

function! SyntaxCheckers_python_python_IsAvailable() dict
    return executable(self.getExec()) &&
        \ syntastic#util#versionIsAtLeast(syntastic#util#getVersion(self.getExecEscaped() . ' --version'), [2, 6])
endfunction

function! SyntaxCheckers_python_python_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'exe': [self.getExec(), s:compiler] })

    let errorformat = '%E%f:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'python'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
