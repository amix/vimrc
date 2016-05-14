"============================================================================
"File:        cuda.vim
"Description: Syntax checking plugin for syntastic
"Author:      Hannes Schulz <schulz at ais dot uni-bonn dot de>
"
"============================================================================

if exists('g:loaded_syntastic_cuda_nvcc_checker')
    finish
endif
let g:loaded_syntastic_cuda_nvcc_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_cuda_nvcc_GetLocList() dict
    if syntastic#util#var('cuda_arch') !=# ''
        let arch_flag = '-arch=' . g:syntastic_cuda_arch
    else
        let arch_flag = ''
    endif
    let makeprg =
        \ self.getExecEscaped() . ' ' . arch_flag .
        \ ' --cuda -O0 -I . -Xcompiler -fsyntax-only ' .
        \ syntastic#util#shexpand('%') . ' ' . syntastic#c#NullOutput()

    let errorformat =
        \ '%*[^"]"%f"%*\D%l: %m,'.
        \ '"%f"%*\D%l: %m,'.
        \ '%-G%f:%l: (Each undeclared identifier is reported only once,'.
        \ '%-G%f:%l: for each function it appears in.),'.
        \ '%f:%l:%c:%m,'.
        \ '%f(%l):%m,'.
        \ '%f:%l:%m,'.
        \ '"%f"\, line %l%*\D%c%*[^ ] %m,'.
        \ '%D%*\a[%*\d]: Entering directory `%f'','.
        \ '%X%*\a[%*\d]: Leaving directory `%f'','.
        \ '%D%*\a: Entering directory `%f'','.
        \ '%X%*\a: Leaving directory `%f'','.
        \ '%DMaking %*\a in %f,'.
        \ '%f|%l| %m'

    if index(['h', 'hpp', 'cuh'], expand('%:e', 1), 0, 1) >= 0
        if syntastic#util#var('cuda_check_header', 0)
            let makeprg =
                \ 'echo > .syntastic_dummy.cu ; ' .
                \ self.getExecEscaped() . ' ' . arch_flag .
                \ ' --cuda -O0 -I . .syntastic_dummy.cu -Xcompiler -fsyntax-only -include ' .
                \ syntastic#util#shexpand('%') . ' ' . syntastic#c#NullOutput()
        else
            return []
        endif
    endif

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cuda',
    \ 'name': 'nvcc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
