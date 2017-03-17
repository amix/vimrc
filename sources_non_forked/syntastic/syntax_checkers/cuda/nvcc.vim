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

if !exists('g:syntastic_cuda_config_file')
    let g:syntastic_cuda_config_file = '.syntastic_cuda_config'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_cuda_nvcc_GetLocList() dict
    let buf = bufnr('')
    let arch_flag = syntastic#util#bufVar(buf, 'cuda_arch')
    if arch_flag !=# ''
        let arch_flag = '-arch=' . arch_flag
        call syntastic#log#oneTimeWarn('variable g:syntastic_cuda_arch is deprecated, ' .
            \ 'please add ' . string(arch_flag) . ' to g:syntastic_cuda_nvcc_args instead')
    endif

    let build_opts = {}
    let dummy = ''
    if index(['h', 'hpp', 'cuh'], fnamemodify(bufname(buf), ':e'), 0, 1) >= 0
        if syntastic#util#bufVar(buf, 'cuda_check_header', 0)
            let dummy = fnamemodify(bufname(buf), ':p:h') . syntastic#util#Slash() . '.syntastic_dummy.cu'
            let build_opts = {
                \ 'exe_before': 'echo > ' . syntastic#util#shescape(dummy) . ' ;',
                \ 'fname_before': '.syntastic_dummy.cu -include' }
        else
            return []
        endif
    endif

    call extend(build_opts, {
        \ 'args_before': arch_flag . ' --cuda -O0 -I .',
        \ 'args': syntastic#c#ReadConfig(g:syntastic_cuda_config_file),
        \ 'args_after': '-Xcompiler -fsyntax-only',
        \ 'tail_after': syntastic#c#NullOutput() })

    let makeprg = self.makeprgBuild(build_opts)

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

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'type': 'E'} })

    for e in loclist
        let pat = matchstr(e['text'], '\m\c^\s*warning:\s*\zs.*')
        if pat !=# ''
            let e['text'] = pat
            let e['type'] = 'W'
        endif
    endfor

    if dummy !=# ''
        call delete(dummy)
    endif

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cuda',
    \ 'name': 'nvcc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
