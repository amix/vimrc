"============================================================================
"File:        objcpp.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Gregor Uhlenheuer <kongo2002 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_objcpp_gcc_checker')
    finish
endif
let g:loaded_syntastic_objcpp_gcc_checker = 1

if !exists('g:syntastic_objcpp_compiler_options')
    let g:syntastic_objcpp_compiler_options = '-std=gnu99'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_objcpp_gcc_IsAvailable() dict
    if !exists('g:syntastic_c_compiler')
        let g:syntastic_objcpp_compiler = executable(self.getExec()) ? self.getExec() : 'clang'
    endif
    call self.log('g:syntastic_objcpp_compiler =', g:syntastic_objcpp_compiler)
    return executable(expand(g:syntastic_objcpp_compiler, 1))
endfunction

function! SyntaxCheckers_objcpp_gcc_GetLocList() dict
    return syntastic#c#GetLocList('objcpp', 'gcc', {
        \ 'errorformat':
        \     '%-G%f:%s:,' .
        \     '%-G%f:%l: %#error: %#(Each undeclared identifier is reported only%.%#,' .
        \     '%-G%f:%l: %#error: %#for each function it appears%.%#,' .
        \     '%-GIn file included%.%#,'.
        \     '%-G %#from %f:%l\,,' .
        \     '%f:%l:%c: %trror: %m,' .
        \     '%f:%l:%c: %tarning: %m,' .
        \     '%f:%l:%c: %m,' .
        \     '%f:%l: %trror: %m,' .
        \     '%f:%l: %tarning: %m,' .
        \     '%f:%l: %m',
        \ 'main_flags': '-x objective-c++ -fsyntax-only',
        \ 'header_flags': '-x objective-c++-header -lobjc',
        \ 'header_names': '\m\.h$' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'objcpp',
    \ 'name': 'gcc' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
