"============================================================================
"File:        prettycss.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_css_prettycss_checker')
    finish
endif
let g:loaded_syntastic_css_prettycss_checker = 1

if !exists('g:syntastic_css_prettycss_sort')
    let g:syntastic_css_prettycss_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_css_prettycss_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m (\zs[^)]\+\ze)$')
    if term !=# ''
        let term = '\V' . escape(term, '\')
    endif
    return term
endfunction

function! SyntaxCheckers_css_prettycss_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    " Print CSS Lint's error/warning messages from compact format. Ignores blank lines.
    let errorformat =
        \ '%EError:  %m\, line %l\, char %c),' .
        \ '%WWarning:  %m\, line %l\, char %c),' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')} })

    for e in loclist
        let e['text'] .= ')'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'css',
    \ 'name': 'prettycss'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
