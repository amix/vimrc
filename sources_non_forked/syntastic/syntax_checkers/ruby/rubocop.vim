"============================================================================
"File:        rubocop.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Recai Oktaş <roktas@bil.omu.edu.tr>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" In order to use rubocop with the default ruby checker (mri):
"     let g:syntastic_ruby_checkers = ['mri', 'rubocop']

if exists("g:loaded_syntastic_ruby_rubocop_checker")
    finish
endif
let g:loaded_syntastic_ruby_rubocop_checker=1

function! SyntaxCheckers_ruby_rubocop_IsAvailable()
    return
        \ executable('rubocop') &&
        \ syntastic#util#versionIsAtLeast(syntastic#util#parseVersion('rubocop --version'), [0,9,0])
endfunction

function! SyntaxCheckers_ruby_rubocop_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'rubocop',
        \ 'args': '--format emacs --silent',
        \ 'filetype': 'ruby',
        \ 'subchecker': 'rubocop' })

    let errorformat = '%f:%l:%c: %t: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style'})

    " convert rubocop severities to error types recognized by syntastic
    for n in range(len(loclist))
        if loclist[n]['type'] == 'F'
            let loclist[n]['type'] = 'E'
        elseif loclist[n]['type'] != 'W' && loclist[n]['type'] != 'E'
            let loclist[n]['type'] = 'W'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'rubocop'})
