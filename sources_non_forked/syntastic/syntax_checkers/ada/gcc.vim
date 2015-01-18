"============================================================================
"File:        ada.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Alfredo Di Napoli <alfredo.dinapoli@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law.
"
"============================================================================

if exists('g:loaded_syntastic_ada_gcc_checker')
    finish
endif
let g:loaded_syntastic_ada_gcc_checker = 1

if !exists('g:syntastic_ada_compiler_options')
    let g:syntastic_ada_compiler_options = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ada_gcc_IsAvailable() dict
    if !exists('g:syntastic_ada_compiler')
        let g:syntastic_ada_compiler = self.getExec()
    endif
    return executable(expand(g:syntastic_ada_compiler, 1))
endfunction

function! SyntaxCheckers_ada_gcc_GetLocList() dict
    return syntastic#c#GetLocList('ada', 'gcc', {
        \ 'errorformat':
        \     '%-G%f:%s:,' .
        \     '%f:%l:%c: %m,' .
        \     '%f:%l: %m',
        \ 'main_flags': '-c -x ada -fsyntax-only',
        \ 'header_flags': '-x ada',
        \ 'header_names': '\.ads$' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ada',
    \ 'name': 'gcc' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
