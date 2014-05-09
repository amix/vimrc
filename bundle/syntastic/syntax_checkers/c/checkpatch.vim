"============================================================================
"File:        checkpatch.vim
"Description: Syntax checking plugin for syntastic.vim using checkpatch.pl
"Maintainer:  Daniel Walker <dwalker at fifo99 dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_c_checkpatch_checker")
    finish
endif
let g:loaded_syntastic_c_checkpatch_checker = 1

" Bail if the user doesn't have `checkpatch.pl` or ./scripts/checkpatch.pl installed.
if executable("checkpatch.pl")
    let g:syntastic_c_checker_checkpatch_location = 'checkpatch.pl'
elseif executable("./scripts/checkpatch.pl")
    let g:syntastic_c_checker_checkpatch_location = './scripts/checkpatch.pl'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_checkpatch_IsAvailable() dict
    return exists("g:syntastic_c_checker_checkpatch_location")
endfunction

function! SyntaxCheckers_c_checkpatch_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe': g:syntastic_c_checker_checkpatch_location,
        \ 'args_after': '--no-summary --no-tree --terse --file' })

    let errorformat =
        \ '%f:%l: %tARNING: %m,' .
        \ '%f:%l: %tRROR: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1],
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'checkpatch',
    \ 'exec': 'checkpatch.pl'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
