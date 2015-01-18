"============================================================================
"File:        scss_lint.vim
"Description: SCSS style and syntax checker plugin for Syntastic
"Maintainer:  Shane da Silva <shane@dasilva.io>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_scss_scss_lint_checker")
    finish
endif
let g:loaded_syntastic_scss_scss_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_scss_scss_lint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    let ver = syntastic#util#getVersion(self.getExecEscaped() . ' --version')
    call self.log(self.getExec() . ' version =', ver)

    return syntastic#util#versionIsAtLeast(ver, [0, 12])
endfunction

function! SyntaxCheckers_scss_scss_lint_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat = '%f:%l [%t] %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1, 2, 65, 66] })

    let cutoff = strlen('Syntax Error: ')
    for e in loclist
        if e['text'][: cutoff-1] ==# 'Syntax Error: '
            let e['text'] = e['text'][cutoff :]
        else
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'scss',
    \ 'name': 'scss_lint',
    \ 'exec': 'scss-lint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
