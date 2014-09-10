"============================================================================
"File:        closurecompiler.vim
"Description: Javascript syntax checker - using Google Closure Compiler
"Maintainer:  Motohiro Takayama <mootoh at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_javascript_closurecompiler_checker")
    finish
endif
let g:loaded_syntastic_javascript_closurecompiler_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_closurecompiler_IsAvailable() dict
    call syntastic#log#deprecationWarn('javascript_closure_compiler_path', 'javascript_closurecompiler_path')

    return
        \ executable("java") &&
        \ exists("g:syntastic_javascript_closurecompiler_path") &&
        \ filereadable(g:syntastic_javascript_closurecompiler_path)
endfunction

function! SyntaxCheckers_javascript_closurecompiler_GetLocList() dict
    call syntastic#log#deprecationWarn('javascript_closure_compiler_options', 'javascript_closurecompiler_args')
    call syntastic#log#deprecationWarn('javascript_closure_compiler_file_list', 'javascript_closurecompiler_file_list')

    if exists("g:syntastic_javascript_closurecompiler_file_list")
        let file_list = join(readfile(g:syntastic_javascript_closurecompiler_file_list))
    else
        let file_list = syntastic#util#shexpand('%')
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe_after': '-jar ' . g:syntastic_javascript_closurecompiler_path,
        \ 'args_after': '--js' ,
        \ 'fname': file_list })

    let errorformat =
        \ '%-GOK,'.
        \ '%E%f:%l: ERROR - %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: WARNING - %m,'.
        \ '%Z%p^'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'closurecompiler',
    \ 'exec': 'java'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
