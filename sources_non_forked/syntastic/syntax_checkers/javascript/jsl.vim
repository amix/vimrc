"============================================================================
"File:        jsl.vim
"Description: Javascript syntax checker - using jsl
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================
if exists("g:loaded_syntastic_javascript_jsl_checker")
    finish
endif
let g:loaded_syntastic_javascript_jsl_checker=1

if !exists("g:syntastic_javascript_jsl_conf")
    let g:syntastic_javascript_jsl_conf = ""
endif

function s:ConfFlag()
    if !empty(g:syntastic_javascript_jsl_conf)
        return "-conf " . g:syntastic_javascript_jsl_conf
    endif

    return ""
endfunction

function! SyntaxCheckers_javascript_jsl_IsAvailable()
    return executable('jsl')
endfunction

function! SyntaxCheckers_javascript_jsl_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'jsl',
        \ 'args': s:ConfFlag() . " -nologo -nofilelisting -nosummary -nocontext -process",
        \ 'filetype': 'javascript',
        \ 'subchecker': 'jsl' })

    let errorformat =
        \ '%W%f(%l): lint warning: %m,'.
        \ '%-Z%p^,'.
        \ '%W%f(%l): warning: %m,'.
        \ '%-Z%p^,'.
        \ '%E%f(%l): SyntaxError: %m,'.
        \ '%-Z%p^,'.
        \ '%-G'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'jsl'})

