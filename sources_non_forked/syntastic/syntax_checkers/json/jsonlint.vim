"============================================================================
"File:        jsonlint.vim
"Description: JSON syntax checker - using jsonlint
"Maintainer:  Miller Medeiros <contact at millermedeiros dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_json_jsonlint_checker")
    finish
endif
let g:loaded_syntastic_json_jsonlint_checker=1

function! SyntaxCheckers_json_jsonlint_IsAvailable()
    return executable('jsonlint')
endfunction

function! SyntaxCheckers_json_jsonlint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'jsonlint',
        \ 'post_args': '--compact',
        \ 'filetype': 'json',
        \ 'subchecker': 'jsonlint' })

    let errorformat =
        \ '%ELine %l:%c,'.
        \ '%Z\\s%#Reason: %m,'.
        \ '%C%.%#,'.
        \ '%f: line %l\, col %c\, %m,'.
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'json',
    \ 'name': 'jsonlint'})
