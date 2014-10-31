"============================================================================
"File:        typescript.vim
"Description: TypeScript syntax checker
"Maintainer:  Bill Casarin <bill@casarin.ca>
"============================================================================

if exists("g:loaded_syntastic_typescript_tsc_checker")
    finish
endif
let g:loaded_syntastic_typescript_tsc_checker = 1

if !exists('g:syntastic_typescript_tsc_sort')
    let g:syntastic_typescript_tsc_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_typescript_tsc_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '--module commonjs',
        \ 'args_after': '--out ' . syntastic#util#DevNull() })

    let errorformat =
        \ '%E%f %#(%l\,%c): error %m,' .
        \ '%E%f %#(%l\,%c): %m,' .
        \ '%Eerror %m,' .
        \ '%C%\s%\+%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'typescript',
    \ 'name': 'tsc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
