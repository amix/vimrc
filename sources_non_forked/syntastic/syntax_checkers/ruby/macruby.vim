"============================================================================
"File:        macruby.vim
"Description: Syntax checking plugin for syntastic
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ruby_macruby_checker')
    finish
endif
let g:loaded_syntastic_ruby_macruby_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_macruby_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '-W1',
        \ 'args_after': '-c' })

    let errorformat =
        \ '%-GSyntax OK,'.
        \ '%E%f:%l: syntax error\, %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: %m,'.
        \ '%-C%.%#'

    let env = { 'RUBYOPT': '' }

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'macruby'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
