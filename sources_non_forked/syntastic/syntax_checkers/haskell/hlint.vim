"============================================================================
"File:        hlint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Nicolas Wu <nicolas.wu at gmail dot com>
"License:     BSD
"============================================================================

if exists("g:loaded_syntastic_haskell_hlint_checker")
    finish
endif
let g:loaded_syntastic_haskell_hlint_checker=1

function! SyntaxCheckers_haskell_hlint_IsAvailable()
    return executable('hlint')
endfunction

function! SyntaxCheckers_haskell_hlint_GetLocList()
    let errorformat =
        \ '%E%f:%l:%c: Error: %m,' .
        \ '%W%f:%l:%c: Warning: %m,' .
        \ '%C%m'

    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'hlint',
        \ 'filetype': 'haskell',
        \ 'subchecker': 'hlint' })

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'hlint'})
