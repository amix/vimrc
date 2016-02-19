"============================================================================
"File:        iverilog.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Psidium <psiidium at gmail dot com>
"License:     The MIT License
"============================================================================

if exists('g:loaded_syntastic_verilog_iverilog_checker')
    finish
endif
let g:loaded_syntastic_verilog_iverilog_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_verilog_iverilog_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_before': '-t null',
        \ 'args': '-Wall' })

    let errorformat =
        \ '%f:%l: %trror: %m,' .
        \ '%f:%l: %tarning: %m,' .
        \ '%E%f:%l:      : %m,' .
        \ '%W%f:%l:        : %m,' .
        \ '%f:%l: %m'

    return SyntasticMake({'makeprg': makeprg, 'errorformat': errorformat})
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'verilog',
    \ 'name': 'iverilog'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
