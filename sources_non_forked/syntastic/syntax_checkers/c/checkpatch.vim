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

if exists('g:loaded_syntastic_c_checkpatch_checker')
    finish
endif
let g:loaded_syntastic_c_checkpatch_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_c_checkpatch_IsAvailable() dict
    call syntastic#log#deprecationWarn('c_checker_checkpatch_location', 'c_checkpatch_exec')

    if !exists('g:syntastic_c_checkpatch_exec') && !executable(self.getExec())
        if executable('checkpatch')
            let g:syntastic_c_checkpatch_exec = 'checkpatch'
        elseif executable('./scripts/checkpatch.pl')
            let g:syntastic_c_checkpatch_exec = fnamemodify('./scripts/checkpatch.pl', ':p')
        elseif executable('./scripts/checkpatch')
            let g:syntastic_c_checkpatch_exec = fnamemodify('./scripts/checkpatch', ':p')
        endif
    endif

    call self.log('exec =', self.getExec())

    return executable(self.getExec())
endfunction

function! SyntaxCheckers_c_checkpatch_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--no-summary --no-tree --terse --file' })

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

" vim: set sw=4 sts=4 et fdm=marker:
