"============================================================================
"File:        verilator.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Kocha <kocha dot lsifrontend at gmail dot com>
"============================================================================

if exists('g:loaded_syntastic_verilog_verilator_checker')
    finish
endif
let g:loaded_syntastic_verilog_verilator_checker = 1

if !exists('g:syntastic_verilog_compiler_options')
    let g:syntastic_verilog_compiler_options = '-Wall'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_verilog_verilator_IsAvailable() dict
    if !exists('g:syntastic_verilog_compiler')
        let g:syntastic_verilog_compiler = self.getExec()
    endif
    call self.log('g:syntastic_verilog_compiler =', g:syntastic_verilog_compiler)
    return executable(expand(g:syntastic_verilog_compiler, 1))
endfunction

function! SyntaxCheckers_verilog_verilator_GetLocList() dict
    return syntastic#c#GetLocList('verilog', 'verilator', {
        \ 'errorformat':
        \     '%%%trror-%\=%\w%#: %f:%l: %m,' .
        \     '%%%tarning-%\=%\w%#: %f:%l: %m',
        \ 'main_flags': '--lint-only' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'verilog',
    \ 'name': 'verilator' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
