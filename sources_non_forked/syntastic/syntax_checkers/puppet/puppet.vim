"============================================================================
"File:        puppet.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Eivind Uggedal <eivind at uggedal dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_puppet_puppet_checker")
    finish
endif
let g:loaded_syntastic_puppet_puppet_checker=1

function! SyntaxCheckers_puppet_puppet_IsAvailable()
    return executable("puppet")
endfunction

function! SyntaxCheckers_puppet_puppet_GetLocList()

    let ver = syntastic#util#parseVersion('puppet --version 2>' . syntastic#util#DevNull())

    if syntastic#util#versionIsAtLeast(ver, [2,7,0])
        let args = 'parser validate --color=false'
    else
        let args = '--color=false --parseonly'
    endif

    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'puppet',
        \ 'args': args,
        \ 'filetype': 'puppet',
        \ 'subchecker': 'puppet' })

    let errorformat =
        \ '%-Gerr: Try ''puppet help parser validate'' for usage,' .
        \ '%-GError: Try ''puppet help parser validate'' for usage,' .
        \ '%Eerr: Could not parse for environment %*[a-z]: %m at %f:%l,' .
        \ '%EError: Could not parse for environment %*[a-z]: %m at %f:%l'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'puppet',
    \ 'name': 'puppet'})
