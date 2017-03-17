"============================================================================
"File:        phpmd.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_php_phpmd_checker')
    finish
endif
let g:loaded_syntastic_php_phpmd_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_php_phpmd_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m\C^The \S\+ \w\+\(()\)\= \(has\|is not\|utilizes\)')
    if term !=# ''
        return '\V'.substitute(term, '\m\C^The \S\+ \(\w\+\)\(()\)\= .*', '\1', '')
    endif
    let term = matchstr(a:item['text'], '\m\C^Avoid \(variables with short\|excessively long variable\) names like \S\+\.')
    if term !=# ''
        return '\V'.substitute(term, '\m\C^Avoid \(variables with short\|excessively long variable\) names like \(\S\+\)\..*', '\2', '')
    endif
    let term = matchstr(a:item['text'], '\m\C^Avoid using short method names like \S\+::\S\+()\.')
    if term !=# ''
        return '\V'.substitute(term, '\m\C^Avoid using short method names like \S\+::\(\S\+\)()\..*', '\1', '')
    endif
    let term = matchstr(a:item['text'], '\m\C^\S\+ accesses the super-global variable ')
    if term !=# ''
        return '\V'.substitute(term, '\m\C accesses the super-global variable .*$', '', '')
    endif
    let term = matchstr(a:item['text'], '\m\C^Constant \S\+ should be defined in uppercase')
    if term !=# ''
        return '\V'.substitute(term, '\m\C^Constant \(\S\+\) should be defined in uppercase', '\1', '')
    endif
    let term = matchstr(a:item['text'], "\\m\\C^The '\\S\\+()' method which returns ")
    if term !=# ''
        return '\V'.substitute(term, "\\m\\C^The '\\(\\S\\+\\)()' method which returns.*", '\1', '')
    endif
    let term = matchstr(a:item['text'], '\m\C variable \S\+ should begin with ')
    if term !=# ''
        return '\V'.substitute(term, '\m\C.* variable \(\S\+\) should begin with .*', '\1', '')
    endif
    let term = matchstr(a:item['text'], "\\m\\C^Avoid unused \\(private fields\\|local variables\\|private methods\\|parameters\\) such as '\\S\\+'")
    if term !=# ''
        return '\V'.substitute(term, "\\m\\C^Avoid unused \\(private fields\\|local variables\\|private methods\\|parameters\\) such as '\\(\\S\\+\\)'.*", '\2', '')
    endif
    return ''
endfunction

function! SyntaxCheckers_php_phpmd_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'post_args_before': 'text',
        \ 'post_args': 'codesize,design,unusedcode,naming' })

    let errorformat = '%E%f:%l%\s%#%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype' : 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'php',
    \ 'name': 'phpmd'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
