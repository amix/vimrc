"============================================================================
"File:        avrgcc.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Karel <karelishere at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_c_avrgcc_checker')
    finish
endif
let g:loaded_syntastic_c_avrgcc_checker = 1

if !exists('g:syntastic_avrgcc_config_file')
    let g:syntastic_avrgcc_config_file = '.syntastic_avrgcc_config'
endif

let s:save_cpo = &cpo
set cpo&vim

let s:opt_x = { 'c': 'c', 'cpp': 'c++' }

function! SyntaxCheckers_c_avrgcc_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_before': syntastic#c#ReadConfig(g:syntastic_avrgcc_config_file),
        \ 'args_after': '-x ' . get(s:opt_x, self.getFiletype(), '')  . ' -fsyntax-only' })

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

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'avrgcc',
    \ 'exec': 'avr-gcc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
