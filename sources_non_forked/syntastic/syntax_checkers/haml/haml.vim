"============================================================================
"File:        haml.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_haml_haml_checker")
    finish
endif
let g:loaded_syntastic_haml_haml_checker=1

if !exists("g:syntastic_haml_interpreter")
    let g:syntastic_haml_interpreter = "haml"
endif

function! SyntaxCheckers_haml_haml_IsAvailable()
    return executable(g:syntastic_haml_interpreter)
endfunction

function! SyntaxCheckers_haml_haml_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': g:syntastic_haml_interpreter,
        \ 'args': '-c',
        \ 'filetype': 'haml',
        \ 'subchecker': 'haml' })

    let errorformat =
        \ 'Haml error on line %l: %m,' .
        \ 'Syntax error on line %l: %m,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haml',
    \ 'name': 'haml'})
