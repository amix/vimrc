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
let g:loaded_syntastic_puppet_puppet_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_puppet_puppet_GetLocList() dict
    let ver = syntastic#util#getVersion(self.getExecEscaped() . ' --version 2>' . syntastic#util#DevNull())

    if syntastic#util#versionIsAtLeast(ver, [2,7,0])
        let args = 'parser validate --color=false'
    else
        let args = '--color=false --parseonly'
    endif

    let makeprg = self.makeprgBuild({ 'args_before': args })

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

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
