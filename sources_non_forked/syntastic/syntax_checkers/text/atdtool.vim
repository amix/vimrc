"============================================================================
"File:        atdtool.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_text_atdtool_checker")
    finish
endif
let g:loaded_syntastic_text_atdtool_checker = 1

function! SyntaxCheckers_text_atdtool_IsAvailable()
    return executable('atdtool')
endfunction

function! SyntaxCheckers_text_atdtool_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m "\zs[^"]\+\ze"\($\| | suggestions:\)')
    if term != ''
        let col = get(a:item, 'col', 0)
        let term = (col != 0 ? '\%' . col . 'c' : '') . '\V' . term
    endif
    return term
endfunction

function! SyntaxCheckers_text_atdtool_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'atdtool',
        \ 'tail': '2>' . syntastic#util#DevNull(),
        \ 'filetype': 'text',
        \ 'subchecker': 'atdtool' })

    let errorformat =
        \ '%W%f:%l:%c: %m,'.
        \ '%+C  suggestions:%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0],
        \ 'subtype': 'Style' })

    for n in range(len(loclist))
        let loclist[n]['text'] = substitute(loclist[n]['text'], '\n\s\+', ' | ', 'g')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'text',
    \ 'name': 'atdtool'})
