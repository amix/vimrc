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
let g:loaded_syntastic_yaml_jsyaml_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_yaml_jsyaml_GetLocList() dict
    if !exists('s:js_yaml_new')
        let ver = syntastic#util#getVersion(self.getExecEscaped() . ' --version')
        call self.log(self.getExec() . ' version =', ver)
        let s:js_yaml_new = syntastic#util#versionIsAtLeast(ver, [2])
    endif

    let makeprg = self.makeprgBuild({ 'args_after': (s:js_yaml_new ? '' : '--compact') })

    let errorformat =
        \ 'Error on line %l\, col %c:%m,' .
        \ 'JS-YAML: %m at line %l\, column %c:,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'yaml',
    \ 'name': 'jsyaml',
    \ 'exec': 'js-yaml'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
