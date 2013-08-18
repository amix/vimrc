"============================================================================
"File:        yaml.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"
"Installation: $ npm install -g js-yaml
"
"============================================================================

if exists("g:loaded_syntastic_yaml_jsyaml_checker")
    finish
endif
let g:loaded_syntastic_yaml_jsyaml_checker=1

function! SyntaxCheckers_yaml_jsyaml_IsAvailable()
    return executable("js-yaml")
endfunction

function! SyntaxCheckers_yaml_jsyaml_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'js-yaml',
        \ 'args': '--compact',
        \ 'filetype': 'yaml',
        \ 'subchecker': 'jsyaml' })

    let errorformat='Error on line %l\, col %c:%m,%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'yaml',
    \ 'name': 'jsyaml'})
