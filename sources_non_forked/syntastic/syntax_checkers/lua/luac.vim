"============================================================================
"File:        lua.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Gregor Uhlenheuer <kongo2002 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_lua_luac_checker")
    finish
endif
let g:loaded_syntastic_lua_luac_checker=1

function! SyntaxCheckers_lua_luac_IsAvailable()
    return executable('luac')
endfunction

function! SyntaxCheckers_lua_luac_GetHighlightRegex(pos)
    let near = matchstr(a:pos['text'], "near '[^']\\+'")
    let result = ''
    if len(near) > 0
        let near = split(near, "'")[1]
        if near == '<eof>'
            let p = getpos('$')
            let a:pos['lnum'] = p[1]
            let a:pos['col'] = p[2]
            let result = '\%'.p[2].'c'
        else
            let result = '\V'.near
        endif
        let open = matchstr(a:pos['text'], "(to close '[^']\\+' at line [0-9]\\+)")
        if len(open) > 0
            let oline = split(open, "'")[1:2]
            let line = 0+strpart(oline[1], 9)
            call matchadd('SpellCap', '\%'.line.'l\V'.oline[0])
        endif
    endif
    return result
endfunction


function! SyntaxCheckers_lua_luac_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'luac',
        \ 'args': '-p',
        \ 'filetype': 'lua',
        \ 'subchecker': 'luac' })

    let errorformat =  'luac: %#%f:%l: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': { 'bufnr': bufnr(''), 'type': 'E' } })

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'lua',
    \ 'name': 'luac'})
