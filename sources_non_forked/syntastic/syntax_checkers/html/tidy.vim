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

if exists('g:loaded_syntastic_html_tidy_checker')
    finish
endif
let g:loaded_syntastic_html_tidy_checker = 1

let s:save_cpo = &cpo
set cpo&vim

" Checker options {{{1

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

" }}}1

" Constants {{{1

let s:IGNORE_ERRORS = [
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
lockvar! s:IGNORE_ERRORS

let s:BLOCKLEVEL_TAGS = [
        \ 'main',
        \ 'section',
        \ 'article',
        \ 'aside',
        \ 'header',
        \ 'footer',
        \ 'nav',
        \ 'figure',
        \ 'figcaption'
    \ ]
lockvar! s:BLOCKLEVEL_TAGS

let s:INLINE_TAGS = [
        \ 'video',
        \ 'audio',
        \ 'source',
        \ 'embed',
        \ 'mark',
        \ 'progress',
        \ 'meter',
        \ 'time',
        \ 'ruby',
        \ 'rt',
        \ 'rp',
        \ 'canvas',
        \ 'command',
        \ 'details',
        \ 'datalist'
    \ ]
lockvar! s:INLINE_TAGS

let s:EMPTY_TAGS = [
        \ 'wbr',
        \ 'keygen'
    \ ]
lockvar! s:EMPTY_TAGS

" }}}1

function! SyntaxCheckers_html_tidy_GetLocList() dict " {{{1
    let makeprg = self.makeprgBuild({ 'args_after': s:Args() })

    let errorformat =
        \ '%Wline %l column %v - Warning: %m,' .
        \ '%Eline %l column %v - Error: %m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr('')},
        \ 'returns': [0, 1, 2] })

    " filter out valid HTML5 from the errors
    for e in loclist
        if e['valid'] && s:IgnoreError(e['text']) == 1
            let e['valid'] = 0
        endif
    endfor

    return loclist
endfunction " }}}1

" Utilities {{{1

" TODO: join this with xhtml.vim for DRY's sake?
function! s:TidyEncOptByFenc() " {{{2
    let TIDY_OPTS = {
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
    return get(TIDY_OPTS, &fileencoding, '-utf8')
endfunction " }}}2

function! s:IgnoreError(text) " {{{2
    for item in s:IGNORE_ERRORS + g:syntastic_html_tidy_ignore_errors
        if stridx(a:text, item) != -1
            return 1
        endif
    endfor
    return 0
endfunction " }}}2

function! s:NewTags(name) " {{{2
    return syntastic#util#shescape(join( s:{toupper(a:name)} + g:syntastic_html_tidy_{a:name}, ',' ))
endfunction " }}}2

function! s:Args() " {{{2
    let args = s:TidyEncOptByFenc() .
        \ ' --new-blocklevel-tags ' . s:NewTags('blocklevel_tags') .
        \ ' --new-inline-tags ' . s:NewTags('inline_tags') .
        \ ' --new-empty-tags ' . s:NewTags('empty_tags') .
        \ ' -e'
    return args
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'tidy'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
