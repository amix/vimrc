"============================================================================
"File:        cobc.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================


if exists('g:loaded_syntastic_cobol_cobc_checker')
    finish
endif
let g:loaded_syntastic_cobol_cobc_checker = 1

if !exists('g:syntastic_cobol_compiler_options')
    let g:syntastic_cobol_compiler_options = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_cobol_cobc_IsAvailable() dict
    if !exists('g:syntastic_cobol_compiler')
        let g:syntastic_cobol_compiler = self.getExec()
    endif
    call self.log('g:syntastic_cobol_compiler =', g:syntastic_cobol_compiler)
    return executable(expand(g:syntastic_cobol_compiler, 1))
endfunction

function! SyntaxCheckers_cobol_cobc_GetLocList() dict
    return syntastic#c#GetLocList('cobol', 'cobc', {
        \ 'errorformat': '%f:%\s%#%l: %trror: %m',
        \ 'main_flags': '-fsyntax-only' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cobol',
    \ 'name': 'cobc' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
