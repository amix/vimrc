"============================================================================
"File:        cppcheck.vim
"Description: Syntax checking plugin for syntastic using cppcheck.pl
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_c_cppcheck_checker')
    finish
endif
let g:loaded_syntastic_c_cppcheck_checker = 1

if !exists('g:syntastic_cppcheck_config_file')
    let g:syntastic_cppcheck_config_file = '.syntastic_cppcheck_config'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_cppcheck_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': syntastic#c#ReadConfig(g:syntastic_cppcheck_config_file),
        \ 'args_after': '-q --enable=style' })

    let errorformat =
        \ '[%f:%l]: (%trror) %m,' .
        \ '[%f:%l]: (%tarning) %m,' .
        \ '[%f:%l]: (%ttyle) %m,' .
        \ '[%f:%l]: (%terformance) %m,' .
        \ '[%f:%l]: (%tortability) %m,' .
        \ '[%f:%l]: (%tnformation) %m,' .
        \ '[%f:%l]: (%tnconclusive) %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'cppcheck',
        \ 'returns': [0] })

    for e in loclist
        if e['type'] =~? '\m^[SPI]'
            let e['type'] = 'w'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'cppcheck'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
