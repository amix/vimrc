"============================================================================
"File:        pylama.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
if exists("g:loaded_syntastic_python_pylama_checker")
    finish
endif
let g:loaded_syntastic_python_pylama_checker=1

function! SyntaxCheckers_python_pylama_IsAvailable()
    return executable('pylama')
endfunction

function! SyntaxCheckers_python_pylama_GetHighlightRegex(i)
    return SyntaxCheckers_python_pyflakes_GetHighlightRegex(a:i)
endfunction

function! SyntaxCheckers_python_pylama_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pylama',
        \ 'post_args': ' -f pep8',
        \ 'filetype': 'python',
        \ 'subchecker': 'pylama' })

    let errorformat = '%A%f:%l:%c: %m'

    let loclist=SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['sort'] })

    for n in range(len(loclist))
        let loclist[n]['type'] = match(['R', 'C', 'W'], loclist[n]['text'][0]) >= 0 ? 'W' : 'E'
        if loclist[n]['text'] =~# '\v\[%(pep8|pep257|mccabe)\]$'
            let loclist[n]['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylama' })

runtime! syntax_checkers/python/pyflakes.vim
