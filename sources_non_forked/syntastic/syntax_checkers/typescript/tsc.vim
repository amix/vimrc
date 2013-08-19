"============================================================================
"File:        typescript.vim
"Description: TypeScript syntax checker
"Maintainer:  Bill Casarin <bill@casarin.ca>
"============================================================================

if exists("g:loaded_syntastic_typescript_tsc_checker")
    finish
endif
let g:loaded_syntastic_typescript_tsc_checker=1

function! SyntaxCheckers_typescript_tsc_IsAvailable()
    return executable("tsc")
endfunction


function! SyntaxCheckers_typescript_tsc_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'tsc',
        \ 'args': '--module commonjs',
        \ 'post_args': '--out ' . syntastic#util#DevNull(),
        \ 'filetype': 'typescript',
        \ 'subchecker': 'tsc' })

    let errorformat =
        \ '%E%f %#(%l\,%c): error %m,' .
        \ '%E%f %#(%l\,%c): %m,' .
        \ '%Eerror %m,' .
        \ '%C%\s%\+%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")},
        \ 'postprocess': ['sort'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'typescript',
    \ 'name': 'tsc'})
