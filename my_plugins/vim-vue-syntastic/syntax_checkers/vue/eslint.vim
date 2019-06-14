"============================================================================
"File:        eslint.vim
"Description: Vue syntax checker - using eslint.
"Maintainer:  Sebastian Kellgren sebastian at thriver dot io 
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists('g:loaded_syntastic_vue_eslint_checker')
    finish
endif
let g:loaded_syntastic_vue_eslint_checker = 1

if !exists('g:syntastic_vue_eslint_sort')
    let g:syntastic_vue_eslint_sort = 1
endif

if !exists('g:syntastic_vue_eslint_generic')
    let g:syntastic_vue_eslint_generic = 0
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vue_eslint_IsAvailable() dict
    if g:syntastic_vue_eslint_generic
        call self.log('generic eslint, exec =', self.getExec())
    endif

    if !executable(self.getExec())
        return 0
    endif
    return g:syntastic_vue_eslint_generic || syntastic#util#versionIsAtLeast(self.getVersion(), [0, 1])
endfunction

function! SyntaxCheckers_vue_eslint_GetLocList() dict
    if !g:syntastic_vue_eslint_generic
        call syntastic#log#deprecationWarn('vue_eslint_conf', 'vue_eslint_args',
            \ "'--config ' . syntastic#util#shexpand(OLD_VAR)")
    endif

    let makeprg = self.makeprgBuild({ 'args_before': (g:syntastic_vue_eslint_generic ? '' : '-f compact') })

    let errorformat =
        \ '%E%f: line %l\, col %c\, Error - %m,' .
        \ '%W%f: line %l\, col %c\, Warning - %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['guards'] })

    if !g:syntastic_vue_eslint_generic
        if !exists('s:eslint_new')
            let s:eslint_new = syntastic#util#versionIsAtLeast(self.getVersion(), [1])
        endif

        if !s:eslint_new
            for e in loclist
                let e['col'] += 1
            endfor
        endif
    endif

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vue',
    \ 'name': 'eslint'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
