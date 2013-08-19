"============================================================================
"File:        pep8.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" For details about pep8 see: https://github.com/jcrocholl/pep8

if exists("g:loaded_syntastic_python_pep8_checker")
    finish
endif
let g:loaded_syntastic_python_pep8_checker=1

function! SyntaxCheckers_python_pep8_IsAvailable()
    return executable('pep8')
endfunction

function! SyntaxCheckers_python_pep8_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pep8',
        \ 'filetype': 'python',
        \ 'subchecker': 'pep8' })

    let errorformat = '%f:%l:%c: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    for n in range(len(loclist))
        let loclist[n]['type'] = loclist[n]['text'] =~? '^W' ? 'W' : 'E'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pep8'})
