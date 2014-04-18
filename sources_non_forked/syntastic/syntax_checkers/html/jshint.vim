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

if !exists('g:syntastic_jshint_exec')
    let g:syntastic_jshint_exec = 'jshint'
endif

if !exists('g:syntastic_html_jshint_conf')
    let g:syntastic_html_jshint_conf = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_jshint_IsAvailable() dict
    let exe = expand(g:syntastic_jshint_exec)
    return executable(exe) &&
        \ syntastic#util#versionIsAtLeast(syntastic#util#getVersion(exe . ' --version'), [2,4])
endfunction

function! SyntaxCheckers_html_jshint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe': expand(g:syntastic_jshint_exec),
        \ 'args': (g:syntastic_html_jshint_conf != '' ?
        \       '--config ' . syntastic#util#shexpand(g:syntastic_html_jshint_conf) : ''),
        \ 'args_after': '--verbose --extract always' })

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

" vim: set et sts=4 sw=4:
