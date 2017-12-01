"============================================================================
"File:        phplint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_php_phplint_checker')
    finish
endif
let g:loaded_syntastic_php_phplint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_php_phplint_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\munresolved function \zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\m\(class\|function\|method\) \zs\S\+\ze was declared as')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\maccess forbidden to \(private\|protected\) \(class\|constant\|method\|variable\|\(private\|protected\) property\) \zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\musing deprecated \(class\|constant\|method\|property\|variable\) \zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\munresolved function \zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\munresolved function \zs\S\+\ze')
    if term !=# ''
        return '\V' . escape(term, '\')
    endif
    let term = matchstr(a:item['text'], '\munresolved function \zs\S\+\ze')
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_php_phplint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after':
        \   '--print-file-name ' .
        \   '--print-line-numbers ' .
        \   '--print-column-number ' .
        \   '--print-errors ' .
        \   '--print-warnings ' .
        \   '--no-print-notices ' .
        \   '--no-print-context ' .
        \   '--no-print-source ' .
        \   '--tab-size ' . &tabstop })

    let errorformat =
        \ '%E%f:%l:%v: %tRROR: %m,' .
        \ '%W%f:%l:%v: %tarning: %m,' .
        \ '%+C%\t%.%#,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'],
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })

    for e in loclist
        let e['text'] = substitute(e['text'], '\m \(Hint\|Examples\):.*', '', '')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'php',
    \ 'name': 'phplint',
    \ 'exec': 'phpl' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
