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

if exists('g:loaded_syntastic_ruby_jruby_checker')
    finish
endif
let g:loaded_syntastic_ruby_jruby_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_jruby_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': (syntastic#util#isRunningWindows() ? '-T1 -W1' : '-W1'),
        \ 'args_after': '-c' })

    let errorformat =
        \ '%-GSyntax OK for %f,'.
        \ '%ESyntaxError in %f:%l: syntax error\, %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: %m,'.
        \ '%-C%.%#'

    let env = syntastic#util#isRunningWindows() ? {} : { 'RUBYOPT': '' }

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'jruby'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
