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
" In order to add some custom lib directories that should be added to the
" perl command line you can add those as a comma-separated list to the variable
" g:syntastic_perl_lib_path.
"
"   let g:syntastic_perl_lib_path = './lib,./lib/auto'
"
" To use your own perl error output munger script, use the
" g:syntastic_perl_efm_program option. Any command line parameters should be
" included in the variable declaration. The program should expect a single
" parameter; the fully qualified filename of the file to be checked.
"
"   let g:syntastic_perl_efm_program = "foo.pl -o -m -g"
"

if exists("g:loaded_syntastic_perl_perl_checker")
    finish
endif
let g:loaded_syntastic_perl_perl_checker=1

if !exists("g:syntastic_perl_interpreter")
    let g:syntastic_perl_interpreter = "perl"
endif

function! SyntaxCheckers_perl_perl_IsAvailable()
    return executable(g:syntastic_perl_interpreter)
endfunction

if !exists("g:syntastic_perl_efm_program")
    let g:syntastic_perl_efm_program =
        \ g:syntastic_perl_interpreter . ' ' .
        \ syntastic#util#shescape(expand('<sfile>:p:h') . '/efm_perl.pl') .
        \ ' -c -w'
endif

function! SyntaxCheckers_perl_perl_GetLocList()
    let makeprg = exists("b:syntastic_perl_efm_program") ? b:syntastic_perl_efm_program : g:syntastic_perl_efm_program
    if exists("g:syntastic_perl_lib_path")
        let makeprg .= ' -I' . g:syntastic_perl_lib_path
    endif
    let makeprg .= ' ' . syntastic#util#shexpand('%') . s:ExtraMakeprgArgs()

    let errorformat =  '%t:%f:%l:%m'

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

function! s:ExtraMakeprgArgs()
    let shebang = syntastic#util#parseShebang()
    if index(shebang['args'], '-T') != -1
        return ' -Tc'
    endif

    return ''
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'perl',
    \ 'name': 'perl'})
