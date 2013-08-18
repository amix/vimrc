"============================================================================
"File:        rst.vim
"Description: Syntax checking plugin for docutil's reStructuredText files
"Maintainer:  James Rowe <jnrowe at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

" We use rst2pseudoxml.py, as it is ever so marginally faster than the other
" rst2${x} tools in docutils.

if exists("g:loaded_syntastic_rst_rst2pseudoxml_checker")
    finish
endif
let g:loaded_syntastic_rst_rst2pseudoxml_checker=1

function! SyntaxCheckers_rst_rst2pseudoxml_IsAvailable()
    return executable("rst2pseudoxml.py") || executable("rst2pseudoxml")
endfunction

function! SyntaxCheckers_rst_rst2pseudoxml_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': s:exe(),
        \ 'args': '--report=2 --exit-status=1',
        \ 'tail': syntastic#util#DevNull(),
        \ 'filetype': 'rst',
        \ 'subchecker': 'rst2pseudoxml' })

    let errorformat =
        \ '%f:%l: (%tNFO/1) %m,'.
        \ '%f:%l: (%tARNING/2) %m,'.
        \ '%f:%l: (%tRROR/3) %m,'.
        \ '%f:%l: (%tEVERE/4) %m,'.
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    for n in range(len(loclist))
        if loclist[n]['type'] ==? 'S'
            let loclist[n]['type'] = 'E'
        elseif loclist[n]['type'] ==? 'I'
            let loclist[n]['type'] = 'W'
            let loclist[n]['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

function s:exe()
    return executable("rst2pseudoxml.py") ? "rst2pseudoxml.py" : "rst2pseudoxml"
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rst',
    \ 'name': 'rst2pseudoxml'})
