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

if exists('g:loaded_syntastic_puppet_puppetlint_checker')
    finish
endif
let g:loaded_syntastic_puppet_puppetlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_puppet_puppetlint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    let s:puppetlint_new = syntastic#util#versionIsAtLeast(self.getVersion(), [1])
    return syntastic#util#versionIsAtLeast(self.getVersion(), [0, 2])
endfunction

function! SyntaxCheckers_puppet_puppetlint_GetLocList() dict
    call syntastic#log#deprecationWarn('puppet_lint_arguments', 'puppet_puppetlint_args')

    let makeprg = self.makeprgBuild({
        \ 'args_after':
        \       '--log-format "%{KIND} [%{check}] %{message} at %{fullpath}:' .
        \       (s:puppetlint_new ? '%{line}' : '%{linenumber}') . '"' })

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

" vim: set sw=4 sts=4 et fdm=marker:
