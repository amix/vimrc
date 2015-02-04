"============================================================================
"File:        jshint.vim
"Description: Javascript syntax checker for HTML - using jshint
"Maintainer:  LCD 47 <lcd047@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_html_jshint_checker')
    finish
endif
let g:loaded_syntastic_html_jshint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_jshint_IsAvailable() dict
    call syntastic#log#deprecationWarn('jshint_exec', 'html_jshint_exec')
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [2, 4])
endfunction

function! SyntaxCheckers_html_jshint_GetLocList() dict
    call syntastic#log#deprecationWarn('html_jshint_conf', 'html_jshint_args',
        \ "'--config ' . syntastic#util#shexpand(OLD_VAR)")

    let makeprg = self.makeprgBuild({ 'args_after': '--verbose --extract always' })

    let errorformat = '%A%f: line %l\, col %v\, %m \(%t%*\d\)'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'jshint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
