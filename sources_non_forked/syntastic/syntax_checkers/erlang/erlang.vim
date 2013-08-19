"============================================================================
"File:        erlang.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Pawel Salata <rockplayer.pl at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_erlang_erlang_checker")
    finish
endif
let g:loaded_syntastic_erlang_erlang_checker=1

let s:check_file = expand('<sfile>:p:h') . '/erlang_check_file.erl'
if !exists("g:syntastic_erlc_include_path")
    let g:syntastic_erlc_include_path=""
endif

function! SyntaxCheckers_erlang_escript_IsAvailable()
    return executable('escript')
endfunction

function! SyntaxCheckers_erlang_escript_GetLocList()
    let extension = expand('%:e')
    if match(extension, 'hrl') >= 0
        return []
    endif
    let shebang = getbufline(bufnr('%'), 1)[0]
    if len(shebang) > 0
        if match(shebang, 'escript') >= 0
            let makeprg = 'escript -s ' . syntastic#util#shexpand('%:p')
        else
            let makeprg = 'escript ' . s:check_file . ' ' . syntastic#util#shexpand('%:p') . ' ' . g:syntastic_erlc_include_path
        endif
    else
        let makeprg =  'escript ' . s:check_file . ' ' . syntastic#util#shexpand('%:p') . ' '. g:syntastic_erlc_include_path
    endif
    let errorformat =
        \ '%f:%l:\ %tarning:\ %m,'.
        \ '%E%f:%l:\ %m'

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'erlang',
    \ 'name': 'escript'})
