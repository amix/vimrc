"============================================================================
"File:        less.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Julien Blanchard <julien at sideburns dot eu>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

" To send additional options to less use the variable g:syntastic_less_options.
" The default is
"   let g:syntastic_less_options = "--no-color"
"
" To use less-lint instead of less set the variable
" g:syntastic_less_use_less_lint.

if exists("g:loaded_syntastic_less_lessc_checker")
    finish
endif
let g:loaded_syntastic_less_lessc_checker = 1

if !exists("g:syntastic_less_options")
    let g:syntastic_less_options = ""
endif

if !exists("g:syntastic_less_use_less_lint")
    let g:syntastic_less_use_less_lint = 0
endif

let s:save_cpo = &cpo
set cpo&vim

let s:node_file = 'node ' . syntastic#util#shescape(expand('<sfile>:p:h') . syntastic#util#Slash() . 'less-lint.js')

function! SyntaxCheckers_less_lessc_IsAvailable() dict
    return g:syntastic_less_use_less_lint ? executable('node') : executable(self.getExec())
endfunction

function! SyntaxCheckers_less_lessc_GetLocList() dict
    if !exists('s:check_file')
        let s:check_file = g:syntastic_less_use_less_lint ? s:node_file : self.getExecEscaped()
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': s:check_file,
        \ 'args': g:syntastic_less_options,
        \ 'args_after': '--no-color',
        \ 'tail': '> ' . syntastic#util#DevNull() })

    let errorformat =
        \ '%m in %f on line %l\, column %c:,' .
        \ '%m in %f:%l:%c,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': bufnr(""), 'text': "Syntax error"} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'less',
    \ 'name': 'lessc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
