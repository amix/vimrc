"============================================================================
"File:        cabal.vim
"Description: Haskell package description (.cabal file) linting and syntax
"             validation via 'cabal check'
"Maintainer: Ian D. Bollinger <ian.bollinger@gmail.com>
"License:    This program is free software. It comes without any warranty,
"            to the extent permitted by applicable law. You can redistribute
"            it and/or modify it under the terms of the Do What The Fuck You
"            Want To Public License, Version 2, as published by Sam Hocevar.
"            See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_cabal_cabal_checker')
    finish
endif
let g:loaded_syntastic_cabal_cabal_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_cabal_cabal_GetHighlightRegex(item)
    let field = matchstr(a:item['text'], "\\vParse of field '\\zs[^']+")
    if field !=# ''
        return '\v\c^\s*' . field . '\s*:\s*\zs.*$'
    endif
    let field = matchstr(a:item['text'], "\\v(^|\\s)'\\zs[^']+\\ze'")
    if field !=# ''
        return '\V\c\<' . escape(field, '\') . '\>'
    endif
    return ''
endfunction

function! SyntaxCheckers_cabal_cabal_GetLocList() dict
    let buf = bufnr('')
    let makeprg = self.getExecEscaped() . ' check'

    let errorformat =
        \ '%Ecabal: %f:%l: %m,' .
        \ '%W* %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': fnamemodify(bufname(buf), ':p:h'),
        \ 'preprocess': 'cabal',
        \ 'defaults': {'bufnr': buf} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cabal',
    \ 'name': 'cabal'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
