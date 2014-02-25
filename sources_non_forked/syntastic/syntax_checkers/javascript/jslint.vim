"============================================================================
"File:        jslint.vim
"Description: Javascript syntax checker - using jslint
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"Tested with jslint 0.1.4.
"============================================================================
if exists("g:loaded_syntastic_javascript_jslint_checker")
    finish
endif
let g:loaded_syntastic_javascript_jslint_checker=1

if !exists("g:syntastic_javascript_jslint_conf")
    let g:syntastic_javascript_jslint_conf = "--white --undef --nomen --regexp --plusplus --bitwise --newcap --sloppy --vars"
endif

function! SyntaxCheckers_javascript_jslint_IsAvailable()
    return executable('jslint')
endfunction

function! SyntaxCheckers_javascript_jslint_HighlightTerm(error)
    let unexpected = matchstr(a:error['text'], 'Expected.*and instead saw \'\zs.*\ze\'')
    if len(unexpected) < 1 | return '' | end
    return '\V'.split(unexpected, "'")[1]
endfunction

function! SyntaxCheckers_javascript_jslint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'jslint',
        \ 'args': g:syntastic_javascript_jslint_conf,
        \ 'filetype': 'javascript',
        \ 'subchecker': 'jslint' })

    let errorformat =
        \ '%E %##%n %m,'.
        \ '%-Z%.%#Line %l\, Pos %c,'.
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'jslint'})

