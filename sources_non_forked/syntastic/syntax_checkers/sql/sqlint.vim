"============================================================================
"File:        sqlint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Steve Purcell <steve@sanityinc.com>
"License:     MIT
"============================================================================

if exists('g:loaded_syntastic_sql_sqlint_checker')
    finish
endif
let g:loaded_syntastic_sql_sqlint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sql_sqlint_GetHighlightRegex(i)
    let term = matchstr(a:i['text'], '\m at or near "\zs[^"]\+\ze"')
    return term !=# '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_sql_sqlint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    return syntastic#util#versionIsAtLeast(self.getVersion(), [0, 0, 3])
endfunction

function! SyntaxCheckers_sql_sqlint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%E%f:%l:%c:ERROR %m,' .
        \ '%W%f:%l:%c:WARNING %m,' .
        \ '%C %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sql',
    \ 'name': 'sqlint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
