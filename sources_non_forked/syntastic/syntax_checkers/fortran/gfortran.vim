"============================================================================
"File:        fortran.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Karl Yngve Lervåg <karl.yngve@lervag.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_fortran_gfortran_checker')
    finish
endif
let g:loaded_syntastic_fortran_gfortran_checker = 1

if !exists('g:syntastic_fortran_compiler_options')
    let g:syntastic_fortran_compiler_options = ''
endif

let s:type_map = {}

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_fortran_gfortran_IsAvailable() dict " {{{1
    if !exists('g:syntastic_fortran_compiler')
        let g:syntastic_fortran_compiler = self.getExec()
    endif
    call self.log('g:syntastic_fortran_compiler = ', g:syntastic_fortran_compiler)
    return executable(expand(g:syntastic_fortran_compiler, 1))
endfunction " }}}1

" @vimlint(EVL104, 1, l:errorformat)
function! SyntaxCheckers_fortran_gfortran_GetLocList() dict " {{{1
    call s:SetCompilerType(g:syntastic_fortran_compiler)
    if !has_key(s:type_map, g:syntastic_fortran_compiler)
        call syntastic#log#error("checker fortran/gfortran: can't parse version string (abnormal termination?)")
        return []
    endif

    if s:type_map[g:syntastic_fortran_compiler] ==# 'gfortran'
        let errorformat =
            \ '%-C %#,'.
            \ '%-C  %#%.%#,'.
            \ '%A%f:%l%[.:]%c:,'.
            \ '%Z%\m%\%%(Fatal %\)%\?%trror: %m,'.
            \ '%Z%tarning: %m,'.
            \ '%-G%.%#'
        if !exists('g:syntastic_fortran_gfortran_sort')
            let g:syntastic_fortran_gfortran_sort = 0
        endif
    elseif s:type_map[g:syntastic_fortran_compiler] ==# 'ifort'
        let errorformat =
            \ '%E%f(%l): error #%n: %m,'.
            \ '%W%f(%l): warning #%n: %m,'.
            \ '%W%f(%l): remark #%n: %m,'.
            \ '%-Z%p^,'.
            \ '%-G%.%#'
        if !exists('g:syntastic_fortran_gfortran_sort')
            let g:syntastic_fortran_gfortran_sort = 1
        endif
    endif

    return syntastic#c#GetLocList('fortran', 'gfortran', {
        \ 'errorformat': errorformat,
        \ 'main_flags': '-fsyntax-only' })
endfunction " }}}1
" @vimlint(EVL104, 0, l:errorformat)

" Utilities {{{1

function! s:SetCompilerType(exe) " {{{2
    if !has_key(s:type_map, a:exe)
        try
            let ver = filter( split(syntastic#util#system(syntastic#util#shescape(a:exe) . ' --version'), '\n'),
                \ 'v:val =~# "\\v^%(GNU Fortran|ifort) "' )[0]
            if ver =~# '\m^GNU Fortran '
                let s:type_map[a:exe] = 'gfortran'
            elseif ver =~# '\m^ifort '
                let s:type_map[a:exe] = 'ifort'
            endif
        catch /\m^Vim\%((\a\+)\)\=:E684/
            " do nothing
        endtry
    endif
endfunction " }}}2

" }}}

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'fortran',
    \ 'name': 'gfortran' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
