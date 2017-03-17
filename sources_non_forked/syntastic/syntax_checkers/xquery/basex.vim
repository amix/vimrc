"============================================================================
"File:        basex.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  James Wright <james dot jw at hotmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"
"============================================================================

if exists('g:loaded_syntastic_xquery_basex_checker')
    finish
endif
let g:loaded_syntastic_xquery_basex_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_xquery_basex_GetLocList() dict
    let buf = bufnr('')
    let makeprg = self.makeprgBuild({
        \ 'args_after': '-z',
        \ 'fname_before': '-q',
        \ 'fname': syntastic#util#shescape('inspect:module("' . escape(fnamemodify(bufname(buf), ':p'), '"') . '")') })

    let errorformat =
        \ '%f:%l:%c:%t:%n:%m,' .
        \ '%m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'basex' })

    for e in loclist
        if e['type'] !=# 'W' && e['type'] !=# 'E'
            let e['type'] = 'E'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'xquery',
    \ 'name': 'basex'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
