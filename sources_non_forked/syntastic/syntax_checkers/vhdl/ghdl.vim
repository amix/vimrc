"============================================================================
"File:        ghdl.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Jan Wagner <jaydyou at janidom dot de>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
if exists("g:loaded_syntastic_vhdl_ghdl_checker")
    finish
endif
let g:loaded_syntastic_vhdl_ghdl_checker = 1

function! SyntaxCheckers_vhdl_ghdl_IsAvailable()
    return executable("ghdl")
endfunction

function! SyntaxCheckers_vhdl_ghdl_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'ghdl',
        \ 'args': '-s',
        \ 'filetype': 'vhdl',
        \ 'subchecker': 'ghdl' })

    let errorformat =  '%f:%l:%c: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vhdl',
    \ 'name': 'ghdl'})
