"============================================================================
"File:        eslint.vim
"Description: Javascript syntax checker - using eslint
"Maintainer:  Maksim Ryzhikov <rv.maksim at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_javascript_eslint_checker')
    finish
endif
let g:loaded_syntastic_javascript_eslint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_eslint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    let ver = syntastic#util#getVersion(self.getExecEscaped() . ' --version')
    call self.log(self.getExec() . ' version =', ver)

    return syntastic#util#versionIsAtLeast(ver, [0, 1])
endfunction

function! SyntaxCheckers_javascript_eslint_GetLocList() dict
    call syntastic#log#deprecationWarn('javascript_eslint_conf', 'javascript_eslint_args',
        \ "'--config ' . syntastic#util#shexpand(OLD_VAR)")

    let makeprg = self.makeprgBuild({ 'args_before': '-f compact' })

    let errorformat =
        \ '%E%f: line %l\, col %c\, Error - %m,' .
        \ '%W%f: line %l\, col %c\, Warning - %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['guards'] })

    for e in loclist
        let e['col'] += 1
    endfor

    call self.setWantSort(1)

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'eslint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
