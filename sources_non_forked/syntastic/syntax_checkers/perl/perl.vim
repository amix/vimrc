"============================================================================
"File:        perl.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Anthony Carapetis <anthony.carapetis at gmail dot com>,
"             Eric Harmon <http://eharmon.net>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" Checker options:
"
" - g:syntastic_perl_interpreter (string; default: 'perl')
"   The perl interpreter to use.
"
" - g:syntastic_perl_lib_path (list; default: [])
"   List of include directories to be added to the perl command line. Example:
"
"       let g:syntastic_perl_lib_path = [ './lib', './lib/auto' ]

if exists('g:loaded_syntastic_perl_perl_checker')
    finish
endif
let g:loaded_syntastic_perl_perl_checker=1

if !exists('g:syntastic_perl_interpreter')
    let g:syntastic_perl_interpreter = 'perl'
endif

if !exists('g:syntastic_perl_lib_path')
    let g:syntastic_perl_lib_path = []
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_perl_perl_IsAvailable() dict
    " don't call executable() here, to allow things like
    " let g:syntastic_perl_interpreter='/usr/bin/env perl'
    silent! call system(syntastic#util#shexpand(g:syntastic_perl_interpreter) . ' -e ' . syntastic#util#shescape('exit(0)'))
    return v:shell_error == 0
endfunction

function! SyntaxCheckers_perl_perl_GetLocList() dict
    let exe = expand(g:syntastic_perl_interpreter)
    if type(g:syntastic_perl_lib_path) == type('')
        call syntastic#log#deprecationWarn('variable g:syntastic_perl_lib_path should be a list')
        let includes = split(g:syntastic_perl_lib_path, ',')
    else
        let includes = copy(syntastic#util#var('perl_lib_path'))
    endif
    let shebang = syntastic#util#parseShebang()
    let extra = join(map(includes, '"-I" . v:val')) .
        \ (index(shebang['args'], '-T') >= 0 ? ' -T' : '') .
        \ (index(shebang['args'], '-t') >= 0 ? ' -t' : '')
    let errorformat = '%f:%l:%m'

    let makeprg = self.makeprgBuild({
        \ 'exe': exe,
        \ 'args_before': '-c -X ' . extra })

    let errors = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'perl',
        \ 'defaults': {'type': 'E'} })
    if !empty(errors)
        return errors
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': exe,
        \ 'args_before': '-c -Mwarnings ' . extra })

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'perl',
        \ 'defaults': {'type': 'W'} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'perl',
    \ 'name': 'perl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
