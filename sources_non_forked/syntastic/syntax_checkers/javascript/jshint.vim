"============================================================================
"File:        jshint.vim
"Description: Javascript syntax checker - using jshint
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_jshint_checker')
    finish
endif
let g:loaded_syntastic_javascript_jshint_checker = 1

if !exists('g:syntastic_javascript_jshint_conf')
    let g:syntastic_javascript_jshint_conf = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_jshint_IsAvailable() dict
    if !exists('g:syntastic_jshint_exec')
        let g:syntastic_jshint_exec = self.getExec()
    endif
    if !executable(expand(g:syntastic_jshint_exec))
        return 0
    endif
    let s:jshint_version = syntastic#util#getVersion(syntastic#util#shexpand(g:syntastic_jshint_exec) . ' --version')
    return syntastic#util#versionIsAtLeast(s:jshint_version, [1])
endfunction

function! SyntaxCheckers_javascript_jshint_GetLocList() dict
    if !exists('s:jshint_new')
        let s:jshint_new = syntastic#util#versionIsAtLeast(s:jshint_version, [1, 1])
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': syntastic#util#shexpand(g:syntastic_jshint_exec),
        \ 'args': (g:syntastic_javascript_jshint_conf != '' ?
        \       '--config ' . syntastic#util#shexpand(g:syntastic_javascript_jshint_conf) : ''),
        \ 'args_after': (s:jshint_new ? '--verbose ' : '') })

    let errorformat = s:jshint_new ?
        \ '%A%f: line %l\, col %v\, %m \(%t%*\d\)' :
        \ '%E%f: line %l\, col %v\, %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'jshint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
