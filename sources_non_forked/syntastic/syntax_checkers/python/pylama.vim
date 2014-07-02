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

if exists('g:loaded_syntastic_python_pylama_checker')
    finish
endif
let g:loaded_syntastic_python_pylama_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pylama_GetHighlightRegex(item)
    return SyntaxCheckers_python_pyflakes_GetHighlightRegex(a:item)
endfunction

function! SyntaxCheckers_python_pylama_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe_before': (syntastic#util#isRunningWindows() ? '' : 'TERM=dumb'),
        \ 'args_after': '-f pep8' })

    " TODO: "WARNING:pylama:..." messages are probably a logging bug
    let errorformat =
        \ '%-GWARNING:pylama:%.%#,' .
        \ '%A%f:%l:%c: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })

    " adjust for weirdness in each checker
    for e in loclist
        let e['type'] = e['text'] =~? '\m^[RCW]' ? 'W' : 'E'
        if e['text'] =~# '\v\[%(mccabe|pep257|pylint)\]$'
            if has_key(e, 'col')
                let e['col'] += 1
            endif
        endif
        if e['text'] =~# '\v\[pylint\]$'
            if has_key(e, 'vcol')
                let e['vcol'] = 0
            endif
        endif
        if e['text'] =~# '\v\[%(mccabe|pep257|pep8)\]$'
            let e['subtype'] = 'Style'
        endif
    endfor

    call self.setWantSort(1)

    return loclist
endfunction

runtime! syntax_checkers/python/pyflakes.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylama' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
