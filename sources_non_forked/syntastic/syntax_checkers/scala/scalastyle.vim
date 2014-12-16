"============================================================================
"File:        scalastyle.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_scala_scalastyle_checker')
    finish
endif
let g:loaded_syntastic_scala_scalastyle_checker = 1

if !exists('g:syntastic_scala_scalastyle_jar')
    let g:syntastic_scala_scalastyle_jar = 'scalastyle-batch_2.10.jar'
endif

if !exists('g:syntastic_scala_scalastyle_config_file')
    let g:syntastic_scala_scalastyle_config_file = 'scalastyle_config.xml'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_scala_scalastyle_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    let jar = expand(g:syntastic_scala_scalastyle_jar)
    let conf_file = expand(g:syntastic_scala_scalastyle_config_file)
    call self.log('filereadable(' . string(jar) . ') = ' . filereadable(jar) . ', ' .
        \ 'filereadable(' . string(conf_file) . ') = ' . filereadable(conf_file))

    return filereadable(jar) && filereadable(conf_file)
endfunction

function! SyntaxCheckers_scala_scalastyle_GetLocList() dict

    let makeprg = self.makeprgBuild({
        \ 'exe_after': ['-jar', expand(g:syntastic_scala_scalastyle_jar)],
        \ 'args_before': ['-q', 'true', '-c', expand(g:syntastic_scala_scalastyle_config_file)] })

    let errorformat =
        \ '%trror file=%f message=%m line=%l column=%c,' .
        \ '%trror file=%f message=%m line=%l,' .
        \ '%tarning file=%f message=%m line=%l column=%c,' .
        \ '%tarning file=%f message=%m line=%l'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'returns': [0, 1] })

    for e in loclist
        if has_key(e, 'col')
            let e['col'] += 1
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'scala',
    \ 'name': 'scalastyle',
    \ 'exec': 'java'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
