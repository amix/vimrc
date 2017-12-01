"============================================================================
"File:        python.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_python_python_checker')
    finish
endif
let g:loaded_syntastic_python_python_checker = 1

if !exists('g:syntastic_python_python_use_codec')
    let g:syntastic_python_python_use_codec = 0
endif

let s:save_cpo = &cpo
set cpo&vim

let s:base_path = expand('<sfile>:p:h', 1) . syntastic#util#Slash()

function! SyntaxCheckers_python_python_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [2, 6])
endfunction

function! SyntaxCheckers_python_python_GetLocList() dict
    let compiler = s:base_path . (g:syntastic_python_python_use_codec ? 'codec.py' : 'compile.py')
    call self.log('using compiler script', compiler)
    let makeprg = self.makeprgBuild({ 'exe': [self.getExec(), compiler] })

    let errorformat = '%E%f:%l:%c: %m'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'python'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
