"============================================================================
"File:        rubocop.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Recai Oktaş <roktas@bil.omu.edu.tr>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ruby_rubocop_checker')
    finish
endif
let g:loaded_syntastic_ruby_rubocop_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_rubocop_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [0, 12, 0])
endfunction

function! SyntaxCheckers_ruby_rubocop_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--format emacs' })

    let errorformat = '%f:%l:%c: %t: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style'})

    " convert rubocop severities to error types recognized by syntastic
    for e in loclist
        if e['type'] ==# 'F'
            let e['type'] = 'E'
        elseif e['type'] !=# 'W' && e['type'] !=# 'E'
            let e['type'] = 'W'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'rubocop'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
