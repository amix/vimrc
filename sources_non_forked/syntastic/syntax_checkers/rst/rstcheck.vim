"============================================================================
"File:        rstcheck.vim
"Description: Syntax checking for reStructuredText and embedded code blocks
"Authors:     Steven Myint <git@stevenmyint.com>
"
"============================================================================

if exists('g:loaded_syntastic_rst_rstcheck_checker')
    finish
endif
let g:loaded_syntastic_rst_rstcheck_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_rst_rstcheck_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%f:%l: (%tNFO/1) %m,'.
        \ '%f:%l: (%tARNING/2) %m,'.
        \ '%f:%l: (%tRROR/3) %m,'.
        \ '%f:%l: (%tEVERE/4) %m,'.
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })

    for e in loclist
        if e['type'] ==? 'S'
            let e['type'] = 'E'
        elseif e['type'] ==? 'I'
            let e['type'] = 'W'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'rst',
    \ 'name': 'rstcheck'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
