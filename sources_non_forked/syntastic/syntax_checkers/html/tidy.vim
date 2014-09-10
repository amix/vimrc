"============================================================================
"File:        tidy.vim
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
" Note: if you need to check HTML5 sources, you might consider installing a
" fork of HTML Tidy, named "HTML Tidy for HTML5":
"
"   http://w3c.github.io/tidy-html5/
"
" HTML Tidy for HTML5 can be used without changes by this checker, just install
" it and point g:syntastic_html_tidy_exec to the executable.
"
" Checker options:
"
" - g:syntastic_html_tidy_ignore_errors (list; default: [])
"   list of errors to ignore
" - g:syntastic_html_tidy_blocklevel_tags (list; default: [])
"   list of additional blocklevel tags, to be added to "--new-blocklevel-tags"
" - g:syntastic_html_tidy_inline_tags (list; default: [])
"   list of additional inline tags, to be added to "--new-inline-tags"
" - g:syntastic_html_tidy_empty_tags (list; default: [])
"   list of additional empty tags, to be added to "--new-empty-tags"

if exists("g:loaded_syntastic_html_tidy_checker")
    finish
endif
let g:loaded_syntastic_html_tidy_checker = 1

if !exists('g:syntastic_html_tidy_ignore_errors')
    let g:syntastic_html_tidy_ignore_errors = []
endif

if !exists('g:syntastic_html_tidy_blocklevel_tags')
    let g:syntastic_html_tidy_blocklevel_tags = []
endif

if !exists('g:syntastic_html_tidy_inline_tags')
    let g:syntastic_html_tidy_inline_tags = []
endif

if !exists('g:syntastic_html_tidy_empty_tags')
    let g:syntastic_html_tidy_empty_tags = []
endif

let s:save_cpo = &cpo
set cpo&vim

" TODO: join this with xhtml.vim for DRY's sake?
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

let s:ignore_errors = [
        \ "<table> lacks \"summary\" attribute",
        \ "not approved by W3C",
        \ "<input> proprietary attribute \"placeholder\"",
        \ "<meta> proprietary attribute \"charset\"",
        \ "<meta> lacks \"content\" attribute",
        \ "inserting \"type\" attribute",
        \ "proprietary attribute \"data-",
        \ "missing <!DOCTYPE> declaration",
        \ "inserting implicit <body>",
        \ "inserting missing 'title' element",
        \ "unescaped & or unknown entity",
        \ "<input> attribute \"type\" has invalid value",
        \ "proprietary attribute \"role\"",
        \ "proprietary attribute \"aria-activedescendant\"",
        \ "proprietary attribute \"aria-atomic\"",
        \ "proprietary attribute \"aria-autocomplete\"",
        \ "proprietary attribute \"aria-busy\"",
        \ "proprietary attribute \"aria-checked\"",
        \ "proprietary attribute \"aria-controls\"",
        \ "proprietary attribute \"aria-describedby\"",
        \ "proprietary attribute \"aria-disabled\"",
        \ "proprietary attribute \"aria-dropeffect\"",
        \ "proprietary attribute \"aria-expanded\"",
        \ "proprietary attribute \"aria-flowto\"",
        \ "proprietary attribute \"aria-grabbed\"",
        \ "proprietary attribute \"aria-haspopup\"",
        \ "proprietary attribute \"aria-hidden\"",
        \ "proprietary attribute \"aria-invalid\"",
        \ "proprietary attribute \"aria-label\"",
        \ "proprietary attribute \"aria-labelledby\"",
        \ "proprietary attribute \"aria-level\"",
        \ "proprietary attribute \"aria-live\"",
        \ "proprietary attribute \"aria-multiline\"",
        \ "proprietary attribute \"aria-multiselectable\"",
        \ "proprietary attribute \"aria-orientation\"",
        \ "proprietary attribute \"aria-owns\"",
        \ "proprietary attribute \"aria-posinset\"",
        \ "proprietary attribute \"aria-pressed\"",
        \ "proprietary attribute \"aria-readonly\"",
        \ "proprietary attribute \"aria-relevant\"",
        \ "proprietary attribute \"aria-relevant\"",
        \ "proprietary attribute \"aria-required\"",
        \ "proprietary attribute \"aria-selected\"",
        \ "proprietary attribute \"aria-setsize\"",
        \ "proprietary attribute \"aria-sort\"",
        \ "proprietary attribute \"aria-valuemax\"",
        \ "proprietary attribute \"aria-valuemin\"",
        \ "proprietary attribute \"aria-valuenow\"",
        \ "proprietary attribute \"aria-valuetext\""
    \ ]
lockvar! s:ignore_errors

let s:blocklevel_tags = [
        \ "main",
        \ "section",
        \ "article",
        \ "aside",
        \ "header",
        \ "footer",
        \ "nav",
        \ "figure",
        \ "figcaption"
    \ ]
lockvar! s:blocklevel_tags

let s:inline_tags = [
        \ "video",
        \ "audio",
        \ "source",
        \ "embed",
        \ "mark",
        \ "progress",
        \ "meter",
        \ "time",
        \ "ruby",
        \ "rt",
        \ "rp",
        \ "canvas",
        \ "command",
        \ "details",
        \ "datalist"
    \ ]
lockvar! s:inline_tags

let s:empty_tags = [
        \ "wbr",
        \ "keygen"
    \ ]
lockvar! s:empty_tags

function! s:IgnoreError(text)
    for i in s:ignore_errors + g:syntastic_html_tidy_ignore_errors
        if stridx(a:text, i) != -1
            return 1
        endif
    endfor
    return 0
endfunction

function! s:NewTags(name)
    return syntastic#util#shescape(join( s:{a:name} + g:syntastic_html_tidy_{a:name}, ',' ))
endfunction

function! s:Args()
    let args = s:TidyEncOptByFenc() .
        \ ' --new-blocklevel-tags ' . s:NewTags('blocklevel_tags') .
        \ ' --new-inline-tags ' . s:NewTags('inline_tags') .
        \ ' --new-empty-tags ' . s:NewTags('empty_tags') .
        \ ' -e'
    return args
endfunction

function! SyntaxCheckers_html_tidy_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': s:Args() })

    let errorformat =
        \ '%Wline %l column %v - Warning: %m,' .
        \ '%Eline %l column %v - Error: %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")},
        \ 'returns': [0, 1, 2] })

    " filter out valid HTML5 from the errors
    for e in loclist
        if e['valid'] && s:IgnoreError(e['text']) == 1
            let e['valid'] = 0
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'tidy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
