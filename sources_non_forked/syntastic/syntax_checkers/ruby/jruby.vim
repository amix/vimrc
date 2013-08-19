"============================================================================
"File:        jruby.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Leonid Shevtsov <leonid at shevtsov dot me>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
if exists("g:loaded_syntastic_ruby_jruby_checker")
    finish
endif
let g:loaded_syntastic_ruby_jruby_checker=1

function! SyntaxCheckers_ruby_jruby_IsAvailable()
    return executable('jruby')
endfunction

function! SyntaxCheckers_ruby_jruby_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': s:exe(),
        \ 'args': s:args(),
        \ 'filetype': 'ruby',
        \ 'subchecker': 'jruby' })

    let errorformat =
        \ '%-GSyntax OK for %f,'.
        \ '%ESyntaxError in %f:%l: syntax error\, %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: %m,'.
        \ '%-C%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

function s:args()
    return has('win32') ? '-W1 -T1 -c' : '-W1 -c'
endfunction

function s:exe()
    return has('win32') ? 'jruby' : 'RUBYOPT= jruby'
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'jruby'})
