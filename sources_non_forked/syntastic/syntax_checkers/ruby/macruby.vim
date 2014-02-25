"============================================================================
"File:        macruby.vim
"Description: Syntax checking plugin for syntastic.vim
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
if exists("g:loaded_syntastic_ruby_macruby_checker")
    finish
endif
let g:loaded_syntastic_ruby_macruby_checker=1

function! SyntaxCheckers_ruby_macruby_IsAvailable()
    return executable('macruby')
endfunction

function! SyntaxCheckers_ruby_macruby_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'RUBYOPT= macruby',
        \ 'args': '-W1 -c',
        \ 'filetype': 'ruby',
        \ 'subchecker': 'macruby' })

    let errorformat =
        \ '%-GSyntax OK,'.
        \ '%E%f:%l: syntax error\, %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: %m,'.
        \ '%-C%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'macruby'})
