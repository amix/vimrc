"============================================================================
"File:        reek.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Mindaugas Mozūras
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ruby_reek_checker')
    finish
endif
let g:loaded_syntastic_ruby_reek_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_reek_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [1, 3, 0])
endfunction

function! SyntaxCheckers_ruby_reek_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_before': '--no-color --line-number --single-line' })

    let errorformat =
        \ '%E%.%#: Racc::ParseError: %f:%l :: %m,' .
        \ '%W%f:%l: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 2] })

    for e in loclist
        if e['type'] ==? 'W'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'reek'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
