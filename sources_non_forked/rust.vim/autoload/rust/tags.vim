" Tagbar support code, for the sake of not automatically overriding its
" configuration in case Universal Ctags is detected.

let s:ctags_is_uctags = 0
let s:checked_ctags = 0

function! rust#tags#IsUCtags() abort
    if s:checked_ctags == 0
        let l:ctags_bin = get(g:, 'tagbar_ctags_bin', 'ctags')
        if system(l:ctags_bin.' --version') =~? 'universal ctags'
            let s:ctags_is_uctags = 1
        endif
        let s:checked_ctags = 1
    endif
    return s:ctags_is_uctags
endfunction

" vim: set et sw=4 sts=4 ts=8:
