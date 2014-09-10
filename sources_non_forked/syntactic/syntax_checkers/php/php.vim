"============================================================================
"File:        php.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_php_php_checker")
    finish
endif
let g:loaded_syntastic_php_php_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_php_php_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], "\\munexpected '\\zs[^']\\+\\ze'")
    return term != '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_php_php_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '-d error_reporting=E_ALL',
        \ 'args_after': '-l -d display_errors=1 -d log_errors=0 -d xdebug.cli_color=0' })

    let errorformat =
        \ '%-GNo syntax errors detected in%.%#,'.
        \ 'Parse error: %#syntax %trror\, %m in %f on line %l,'.
        \ 'Parse %trror: %m in %f on line %l,'.
        \ 'Fatal %trror: %m in %f on line %l,'.
        \ '%-G\s%#,'.
        \ '%-GErrors parsing %.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'php',
    \ 'name': 'php'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
