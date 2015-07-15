"============================================================================
"File:        typescript/tslint.vim
"Description: TypeScript linter
"Maintainer:  Seon-Wook Park <seon.wook@swook.net>
"============================================================================

if exists('g:loaded_syntastic_typescript_tslint_checker')
    finish
endif
let g:loaded_syntastic_typescript_tslint_checker = 1

if !exists('g:syntastic_typescript_tslint_sort')
    let g:syntastic_typescript_tslint_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_typescript_tslint_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], "\\m\\s'\\zs.\\{-}\\ze'\\s")
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_typescript_tslint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': '--format verbose',
        \ 'fname_before': '-f' })

    " (comment-format) ts/app.ts[12, 36]: comment must start with lowercase letter
    let errorformat = '%f[%l\, %c]: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'tslint',
        \ 'subtype': 'Style',
        \ 'returns': [0, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'typescript',
    \ 'name': 'tslint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
