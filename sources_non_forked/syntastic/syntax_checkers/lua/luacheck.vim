"============================================================================
"File:        luacheck.vim
"Description: Lua static analysis using luacheck
"Maintainer:  Thiago Bastos <tbastos@tbastos.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_lua_luacheck_checker")
    finish
endif
let g:loaded_syntastic_lua_luacheck_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_lua_luacheck_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m''\zs\S\+\ze''')
    if term != ''
        return '\V\<' . escape(term, '\') . '\>'
    endif

    let term = matchstr(a:item['text'], '\m\(accessing undefined\|setting non-standard global\|' .
                \ 'setting non-module global\|unused global\) variable \zs\S\+')
    if term == ''
        let term = matchstr(a:item['text'], '\mvariable \zs\S\+\ze was previously defined')
    endif
    if term == ''
        let term = matchstr(a:item['text'], '\munused \(variable\|argument\|loop variable\) \zs\S\+')
    endif
    if term == ''
        let term = matchstr(a:item['text'], '\m\(value assigned to variable\|value of argument\|' .
                \ 'value of loop variable\) \zs\S\+')
    endif
    if term == ''
        let term = matchstr(a:item['text'], '\mvariable \zs\S\+\ze is never set')
    endif

    return term != '' ? '\V\<' . escape(term, '\') . '\>' : ''
endfunction

function! SyntaxCheckers_lua_luacheck_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '--no-color' })

    let errorformat =
        \ '%f:%l:%c: %m,'.
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'defaults': { 'type': 'W' },
        \ 'returns': [0, 1, 2] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'lua',
    \ 'name': 'luacheck' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
