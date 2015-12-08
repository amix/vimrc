"============================================================================
"File:        ansible_lint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Erik Zaadi <erik.zaadi at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ansible_ansible_lint_checker')
    finish
endif
let g:loaded_syntastic_ansible_ansible_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ansible_ansible_lint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [2, 0, 4])
endfunction

function! SyntaxCheckers_ansible_ansible_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-p' })

    let errorformat = '%f:%l: [ANSIBLE%n] %m'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'defaults': {'type': 'E'},
        \ 'subtype': 'Style',
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ansible',
    \ 'name': 'ansible_lint',
    \ 'exec': 'ansible-lint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
