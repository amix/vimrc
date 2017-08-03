"============================================================================
"File:        dartanalyzer.vim
"Description: Dart syntax checker - using dartanalyzer
"Maintainer:  Maksim Ryzhikov <rv.maksim at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_dart_dartanalyzer_checker')
    finish
endif
let g:loaded_syntastic_dart_dartanalyzer_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_dart_dartanalyzer_GetHighlightRegex(error)
    if a:error['len']
        let lcol = a:error['col'] - 1
        let rcol = a:error['col'] + a:error['len']
        let ret = '\%>' . lcol . 'c\%<' . rcol . 'c'
    else
        let ret = ''
    endif

    return ret
endfunction

function! SyntaxCheckers_dart_dartanalyzer_GetLocList() dict
    if !exists('s:format_machine')
        let s:format_machine = syntastic#util#versionIsAtLeast(self.getVersion(), [1, 23]) ? '--format=machine' : '--machine'
    endif
    let makeprg = self.makeprgBuild({ 'args_after': s:format_machine })

    " Machine readable format looks like:
    " SEVERITY|TYPE|ERROR_CODE|FILENAME|LINE_NUMBER|COLUMN|LENGTH|MESSAGE
    " SEVERITY: (WARNING|ERROR)
    " TYPE: (RESOLVER|STATIC_TYPE|...)
    " ERROR_CODE: (NO_SUCH_TYPE|...)
    " FILENAME: String
    " LINE_NUMBER: int
    " COLUMN: int
    " LENGTH: int
    " MESSAGE: String

    " We use %n to grab the error length, for the syntax highlighter
    let commonformat = '|%.%#|%.%#|%f|%l|%c|%n|%m'

    let errorformat =
        \ '%EERROR'   . commonformat . ',' .
        \ '%WWARNING' . commonformat

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1, 2, 3] })

    for e in loclist
        let e['text'] = substitute(e['text'], '\m\\\([\\|]\)', '\1', 'g')

        " Undo the %n hack
        let e['len'] = e['nr']
        call remove(e, 'nr')
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'dart',
    \ 'name': 'dartanalyzer' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
