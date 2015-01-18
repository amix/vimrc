"============================================================================
"File:        yamlxs.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_yaml_yamlxs_checker")
    finish
endif
let g:loaded_syntastic_yaml_yamlxs_checker = 1

if !exists('g:syntastic_perl_lib_path')
    let g:syntastic_perl_lib_path = []
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_yaml_yamlxs_IsAvailable() dict
    if !exists('g:syntastic_yaml_yamlxs_exec') && exists('g:syntastic_perl_interpreter')
        let g:syntastic_yaml_yamlxs_exec = g:syntastic_perl_interpreter
    endif

    " don't call executable() here, to allow things like
    " let g:syntastic_perl_interpreter='/usr/bin/env perl'
    silent! call system(self.getExecEscaped() . ' ' . s:Modules() . ' -e ' . syntastic#util#shescape('exit(0)'))
    return v:shell_error == 0
endfunction

function! SyntaxCheckers_yaml_yamlxs_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_before': s:Modules() . ' -e ' . syntastic#util#shescape('YAML::XS::LoadFile($ARGV[0])') })

    let errorformat =
        \ '%EYAML::XS::Load Error: The problem:,' .
        \ '%-C,' .
        \ '%C    %m,' .
        \ '%Cwas found at document: %\d%\+\, line: %l\, column: %c,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'],
        \ 'defaults': {'bufnr': bufnr("")} })
endfunction

function s:Modules()
    if type(g:syntastic_perl_lib_path) == type('')
        call syntastic#log#oneTimeWarn('variable g:syntastic_perl_lib_path should be a list')
        let includes = split(g:syntastic_perl_lib_path, ',')
    else
        let includes = copy(syntastic#util#var('perl_lib_path'))
    endif
    return join(map(includes, '"-I" . v:val') + ['-MYAML::XS'])
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'yaml',
    \ 'name': 'yamlxs',
    \ 'exec': 'perl' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
