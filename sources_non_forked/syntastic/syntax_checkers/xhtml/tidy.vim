"============================================================================
"File:        xhtml.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" Checker option:
"
" - g:syntastic_xhtml_tidy_ignore_errors (list; default: [])
"   list of errors to ignore

if exists("g:loaded_syntastic_xhtml_tidy_checker")
    finish
endif
let g:loaded_syntastic_xhtml_tidy_checker = 1

if !exists('g:syntastic_xhtml_tidy_ignore_errors')
    let g:syntastic_xhtml_tidy_ignore_errors = []
endif

let s:save_cpo = &cpo
set cpo&vim

" TODO: join this with html.vim DRY's sake?
function! s:TidyEncOptByFenc()
    let tidy_opts = {
            \ 'utf-8':        '-utf8',
            \ 'ascii':        '-ascii',
            \ 'latin1':       '-latin1',
            \ 'iso-2022-jp':  '-iso-2022',
            \ 'cp1252':       '-win1252',
            \ 'macroman':     '-mac',
            \ 'utf-16le':     '-utf16le',
            \ 'utf-16':       '-utf16',
            \ 'big5':         '-big5',
            \ 'cp932':        '-shiftjis',
            \ 'sjis':         '-shiftjis',
            \ 'cp850':        '-ibm858',
        \ }
    return get(tidy_opts, &fileencoding, '-utf8')
endfunction

function! s:IgnoreError(text)
    for item in g:syntastic_xhtml_tidy_ignore_errors
        if stridx(a:text, item) != -1
            return 1
        endif
    endfor
    return 0
endfunction

function! SyntaxCheckers_xhtml_tidy_GetLocList() dict
    let encopt = s:TidyEncOptByFenc()
    let makeprg = self.makeprgBuild({ 'args_after': encopt . ' -xml -e' })

    let errorformat=
        \ '%Wline %l column %v - Warning: %m,' .
        \ '%Eline %l column %v - Error: %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")},
        \ 'returns': [0, 1, 2] })

    for e in loclist
        if e['valid'] && s:IgnoreError(e['text']) == 1
            let e['valid'] = 0
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'xhtml',
    \ 'name': 'tidy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
