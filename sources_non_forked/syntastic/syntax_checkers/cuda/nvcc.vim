"============================================================================
"File:        cuda.vim
"Description: Syntax checking plugin for syntastic.vim
"
"Author:      Hannes Schulz <schulz at ais dot uni-bonn dot de>
"
"============================================================================

" in order to also check header files add this to your .vimrc:
" (this creates an empty .syntastic_dummy.cu file in your source directory)
"
"   let g:syntastic_cuda_check_header = 1

" By default, nvcc and thus syntastic, defaults to the most basic architecture.
" This can produce false errors if the developer intends to compile for newer
" hardware and use newer features, eg. double precision numbers. To pass a
" specific target arch to nvcc, e.g. add the following to your .vimrc:
"
"   let g:syntastic_cuda_arch = "sm_20"


if exists("g:loaded_syntastic_cuda_nvcc_checker")
    finish
endif
let g:loaded_syntastic_cuda_nvcc_checker=1

function! SyntaxCheckers_cuda_nvcc_IsAvailable()
    return executable('nvcc')
endfunction

function! SyntaxCheckers_cuda_nvcc_GetLocList()
    if exists('g:syntastic_cuda_arch')
        let arch_flag = '-arch=' . g:syntastic_cuda_arch
    else
        let arch_flag = ''
    endif
    let makeprg =
        \ 'nvcc ' . arch_flag . ' --cuda -O0 -I . -Xcompiler -fsyntax-only ' .
        \ syntastic#util#shexpand('%') . ' ' . syntastic#c#GetNullDevice()
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

    if expand('%') =~? '\%(.h\|.hpp\|.cuh\)$'
        if exists('g:syntastic_cuda_check_header')
            let makeprg =
                \ 'echo > .syntastic_dummy.cu ; ' .
                \ 'nvcc ' . arch_flag . ' --cuda -O0 -I . .syntastic_dummy.cu -Xcompiler -fsyntax-only -include ' .
                \ syntastic#util#shexpand('%') . ' ' . syntastic#c#GetNullDevice()
        else
            return []
        endif
    endif

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cuda',
    \ 'name': 'nvcc'})
