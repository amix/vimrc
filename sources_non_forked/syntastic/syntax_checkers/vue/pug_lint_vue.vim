"============================================================================
"File:        pug_lint_vue.vim
"Description: Syntax checking plugin for syntastic using pug-lint-vue
"             (https://github.com/sourceboat/pug-lint-vue)
"Maintainer:  Tim Carry <tim at pixelastic dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_vue_pug_lint_vue_checker')
    finish
endif
let g:loaded_syntastic_vue_pug_lint_vue_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vue_pug_lint_vue_GetLocList() dict
    let buf = bufnr('')
    let makeprg = self.makeprgBuild({ 'fname': syntastic#util#shescape(fnamemodify(bufname(buf), ':p')) })

    let errorformat = '%\s%#%l:%c %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': { 'bufnr': buf, 'type': 'E' } })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vue',
    \ 'name': 'pug_lint_vue',
    \ 'exec': 'pug-lint-vue' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
