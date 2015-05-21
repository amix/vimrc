"============================================================================
"File:        haxe.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  David Bernard <david.bernard.31 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_haxe_haxe_checker")
    finish
endif
let g:loaded_syntastic_haxe_haxe_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haxe_haxe_GetLocList() dict
    if exists('b:vaxe_hxml')
        let hxml = b:vaxe_hxml
    elseif exists('g:vaxe_hxml')
        let hxml = g:vaxe_hxml
    else
        let hxml = syntastic#util#findInParent('*.hxml', expand('%:p:h', 1))
    endif
    let hxml = fnamemodify(hxml, ':p')

    call self.log('hxml =', hxml)

    if hxml != ''
        let makeprg = self.makeprgBuild({
            \ 'fname': syntastic#util#shescape(fnamemodify(hxml, ':t')) })

        let errorformat = '%E%f:%l: characters %c-%n : %m'

        let loclist = SyntasticMake({
            \ 'makeprg': makeprg,
            \ 'errorformat': errorformat,
            \ 'cwd': fnamemodify(hxml, ':h') })

        for e in loclist
            let e['hl'] = '\%>' . e['col'] . 'c\%<' . (e['nr'] + 1) . 'c'
            let e['col'] += 1
            let e['nr'] = 0
        endfor

        return loclist
    endif

    return []
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haxe',
    \ 'name': 'haxe'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
