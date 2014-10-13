"============================================================================
"File:        checkstyle.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Dmitry Geurkov <d.geurkov at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" Tested with checkstyle 5.5
"============================================================================

if exists("g:loaded_syntastic_java_checkstyle_checker")
    finish
endif
let g:loaded_syntastic_java_checkstyle_checker = 1

if !exists("g:syntastic_java_checkstyle_classpath")
    let g:syntastic_java_checkstyle_classpath = 'checkstyle-5.5-all.jar'
endif

if !exists("g:syntastic_java_checkstyle_conf_file")
    let g:syntastic_java_checkstyle_conf_file = 'sun_checks.xml'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_java_checkstyle_IsAvailable() dict
    return
        \ executable(self.getExec()) &&
        \ filereadable(expand(g:syntastic_java_checkstyle_classpath)) &&
        \ filereadable(expand(g:syntastic_java_checkstyle_conf_file))
endfunction

function! SyntaxCheckers_java_checkstyle_GetLocList() dict

    let fname = syntastic#util#shescape( expand('%:p:h') . syntastic#util#Slash() . expand('%:t') )

    if has('win32unix')
        let fname = substitute(system('cygpath -m ' . fname), '\m\%x00', '', 'g')
    endif

    let makeprg = self.makeprgBuild({
        \ 'args_after': [
        \       '-cp', expand(g:syntastic_java_checkstyle_classpath),
        \       'com.puppycrawl.tools.checkstyle.Main',
        \       '-c', expand(g:syntastic_java_checkstyle_conf_file),
        \       '-f', 'xml'],
        \ 'fname': fname })

    let errorformat = '%f:%t:%l:%c:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'checkstyle',
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'java',
    \ 'name': 'checkstyle',
    \ 'exec': 'java'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
