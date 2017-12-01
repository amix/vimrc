"============================================================================
"File:        lint.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_julia_lint_checker')
    finish
endif
let g:loaded_syntastic_julia_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_julia_lint_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m^\S\+\ze:')
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_julia_lint_IsAvailable() dict
    return
        \ executable(self.getExec()) &&
        \ syntastic#util#system(self.getExecEscaped() . ' -e ' . syntastic#util#shescape('import Lint')) ==# '' &&
        \ v:shell_error == 0
endfunction

function! SyntaxCheckers_julia_lint_GetLocList() dict
    let buf = bufnr('')

    let makeprg = self.getExecEscaped() . ' -e ' . syntastic#util#shescape('using Lint; display(filter(err -> !isinfo(err), lintfile("' . escape(bufname(buf), '\"') . '")))')

    let errorformat = '%f:%l %t%n %m'

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'julia',
    \ 'name': 'lint',
    \ 'exec': 'julia' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
