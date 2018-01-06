"============================================================================
"File:        code-ayatollah.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_racket_code_ayatollah_checker')
    finish
endif
let g:loaded_syntastic_racket_code_ayatollah_checker = 1

if !exists('g:syntastic_racket_code_ayatollah_script')
    let g:syntastic_racket_code_ayatollah_script = 'code-ayatollah.rkt'
endif

if !exists('g:syntastic_racket_code_ayatollah_sort')
    let g:syntastic_racket_code_ayatollah_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_racket_code_ayatollah_IsAvailable() dict
    let s:script = expand(g:syntastic_racket_code_ayatollah_script, 1)
    return executable(self.getExec()) && filereadable(s:script)
endfunction

function! SyntaxCheckers_racket_code_ayatollah_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'exe': [self.getExec(), s:script] })

    let errorformat =
        \ '  %l:%v: %m,' .
        \ '%PErrors in %f:,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    for e in loclist
        let e['col'] += 1
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'racket',
    \ 'name': 'code_ayatollah',
    \ 'exec': 'racket' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
