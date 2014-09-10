"============================================================================
"File:        puppetlint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Eivind Uggedal <eivind at uggedal dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_puppet_puppetlint_checker")
    finish
endif
let g:loaded_syntastic_puppet_puppetlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

if exists("g:syntastic_puppet_lint_arguments")
    let g:syntastic_puppet_puppetlint_args = g:syntastic_puppet_lint_arguments
    call syntastic#log#deprecationWarn("variable g:syntastic_puppet_lint_arguments is deprecated, please use g:syntastic_puppet_puppetlint_args instead")
endif

function! SyntaxCheckers_puppet_puppetlint_IsAvailable() dict
    return
        \ executable("puppet") &&
        \ executable(self.getExec()) &&
        \ syntastic#util#versionIsAtLeast(syntastic#util#getVersion(
        \       self.getExecEscaped() . ' --version 2>' . syntastic#util#DevNull()), [0,1,10])
endfunction

function! SyntaxCheckers_puppet_puppetlint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--log-format "%{KIND} [%{check}] %{message} at %{fullpath}:%{linenumber}"' })

    let errorformat = '%t%*[a-zA-Z] %m at %f:%l'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'puppet',
    \ 'name': 'puppetlint',
    \ 'exec': 'puppet-lint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
