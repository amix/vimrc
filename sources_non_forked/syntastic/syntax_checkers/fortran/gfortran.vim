"============================================================================
"File:        fortran.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Karl Yngve Lervåg <karl.yngve@lervag.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"Note:        This syntax checker uses gfortran with the option -fsyntax-only
"             to check for errors and warnings. Additional flags may be
"             supplied through both local and global variables,
"               b:syntastic_fortran_flags,
"               g:syntastic_fortran_flags.
"             This is particularly useful when the source requires module files
"             in order to compile (that is when it needs modules defined in
"             separate files).
"
"============================================================================

if exists("g:loaded_syntastic_fortran_gfortran_checker")
    finish
endif
let g:loaded_syntastic_fortran_gfortran_checker=1

if !exists('g:syntastic_fortran_flags')
    let g:syntastic_fortran_flags = ''
endif

function! SyntaxCheckers_fortran_gfortran_IsAvailable()
    return executable('gfortran')
endfunction

function! SyntaxCheckers_fortran_gfortran_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'gfortran',
        \ 'args': s:args(),
        \ 'filetype': 'fortran',
        \ 'subchecker': 'gfortran' })

    let errorformat =
        \ '%-C %#,'.
        \ '%-C  %#%.%#,'.
        \ '%A%f:%l.%c:,'.
        \ '%Z%m,'.
        \ '%G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

function s:args()
    let rv  = '-fsyntax-only ' . g:syntastic_fortran_flags
    if exists('b:syntastic_fortran_flags')
        let rv .= " " . b:syntastic_fortran_flags
    endif
    return rv
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'fortran',
    \ 'name': 'gfortran'})
