"============================================================================
"File:        cpplint.vim
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
" For details about cpplint see:
"    https://code.google.com/p/google-styleguide/
"
" Checker options:
"
" - g:syntastic_cpp_cpplint_thres (integer; default: 5)
"   error threshold: policy violations with a severity above this
"   value are highlighted as errors, the others are warnings
"
" - g:syntastic_cpp_cpplint_args (string; default: '--verbose=3')
"   command line options to pass to cpplint

if exists("g:loaded_syntastic_cpp_cpplint_checker")
    finish
endif
let g:loaded_syntastic_cpp_cpplint_checker = 1

if !exists('g:syntastic_cpp_cpplint_thres')
    let g:syntastic_cpp_cpplint_thres = 5
endif

if ! exists('g:syntastic_cpp_cpplint_args')
    let g:syntastic_cpp_cpplint_args = '--verbose=3'
endif

function! SyntaxCheckers_cpp_cpplint_IsAvailable()
    return executable('cpplint.py')
endfunction

function! SyntaxCheckers_cpp_cpplint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'cpplint.py',
        \ 'filetype': 'cpp',
        \ 'subchecker': 'cpplint' })

    let errorformat = '%A%f:%l:  %m [%t],%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    " change error types according to the prescribed threshold
    for n in range(len(loclist))
        let loclist[n]['type'] = loclist[n]['type'] < g:syntastic_cpp_cpplint_thres ? 'W' : 'E'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'cpplint'})
