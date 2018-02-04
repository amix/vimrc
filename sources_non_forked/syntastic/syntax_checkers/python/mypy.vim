"============================================================================
"File:        mypy.vim
"Description: Syntax checking plugin for syntastic
"Author:      Russ Hewgill <Russ dot Hewgill at gmail dot com>
"
"============================================================================

if exists('g:loaded_syntastic_python_mypy_checker')
    finish
endif
let g:loaded_syntastic_python_mypy_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_mypy_GetLocList() dict
    if !exists('s:mypy_new')
        " creative versioning: version string has changed from v0.4.6 to v0.470
        " XXX the test should be fine for now, since 470 > 4
        let s:mypy_new = syntastic#util#versionIsAtLeast(self.getVersion(), [0, 4, 5])
    endif

    let makeprg = self.makeprgBuild({ 'args_after': (s:mypy_new ? '--show-column-numbers' : '') })

    let errorformat =
        \ '%f:%l:%c:%t:%m,' .
        \ '%f:%l:%t:%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1],
        \ 'preprocess': 'mypy' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'mypy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
