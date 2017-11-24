"============================================================================
"File:        make.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Gregor Uhlenheuer <kongo2002 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_c_make_checker')
    finish
endif
let g:loaded_syntastic_c_make_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_make_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-sk', 'fname': '' })

    let errorformat =
        \ '%-G%f:%s:,' .
        \ '%-G%f:%l: %#error: %#(Each undeclared identifier is reported only%.%#,' .
        \ '%-G%f:%l: %#error: %#for each function it appears%.%#,' .
        \ '%-GIn file included%.%#,' .
        \ '%-G %#from %f:%l\,,' .
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l:%c: %m,' .
        \ '%f:%l: %trror: %m,' .
        \ '%f:%l: %tarning: %m,'.
        \ '%f:%l: %m'

    if exists('g:syntastic_c_errorformat')
        let errorformat = g:syntastic_c_errorformat
    endif

    " process makeprg
    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    " filter the processed errors if desired
    if exists('g:syntastic_c_remove_include_errors') && g:syntastic_c_remove_include_errors != 0
        return filter(errors, 'has_key(v:val, "bufnr") && v:val["bufnr"] == ' . bufnr(''))
    else
        return errors
    endif
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'make'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
