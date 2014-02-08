"============================================================================
"File:        perlcritic.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" For details about perlcritic see:
"
" - http://perlcritic.tigris.org/
" - https://metacpan.org/module/Perl::Critic
"
" Checker options:
"
" - g:syntastic_perl_perlcritic_thres (integer; default: 5)
"   error threshold: policy violations with a severity above this
"   value are highlighted as errors, the others are warnings
"
" - g:syntastic_perl_perlcritic_args (string; default: empty)
"   command line options to pass to perlcritic

if exists("g:loaded_syntastic_perl_perlcritic_checker")
    finish
endif
let g:loaded_syntastic_perl_perlcritic_checker = 1

if !exists('g:syntastic_perl_perlcritic_thres')
    let g:syntastic_perl_perlcritic_thres = 5
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_perl_perlcritic_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--quiet --nocolor --verbose "\%s:\%f:\%l:\%c:(\%s) \%m (\%e)\n"' })

    let errorformat = '%t:%f:%l:%c:%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 2],
        \ 'subtype': 'Style' })

    " change error types according to the prescribed threshold
    for e in loclist
        let e['type'] = e['type'] < g:syntastic_perl_perlcritic_thres ? 'W' : 'E'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'perl',
    \ 'name': 'perlcritic'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
