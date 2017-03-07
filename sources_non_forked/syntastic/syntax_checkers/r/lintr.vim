"============================================================================
"File:        lintr.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Jim Hester <james.f.hester at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" Security:
"
" This checker runs the code in your file.  This is probably fine if you
" wrote the file yourself, but it can be a problem if you're trying to
" check third party files.  If you are 100% willing to let Vim run the
" code in your file, set g:syntastic_enable_r_lintr_checker to 1 in
" your vimrc to enable this checker:
"
" let g:syntastic_enable_r_lintr_checker = 1

if exists("g:loaded_syntastic_r_lintr_checker")
    finish
endif
let g:loaded_syntastic_r_lintr_checker = 1

if !exists('g:syntastic_r_lintr_linters')
    let g:syntastic_r_lintr_linters = 'default_linters'
endif

if !exists('g:syntastic_r_lintr_cache')
    let g:syntastic_r_lintr_cache = 'FALSE'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_r_lintr_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], "\\m'\\zs[^']\\+\\ze'")
    return term !=# '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_r_lintr_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    call system(self.getExecEscaped() . ' --slave --no-restore --no-save -e ' . syntastic#util#shescape('library(lintr)'))
    return v:shell_error == 0
endfunction

function! SyntaxCheckers_r_lintr_GetLocList() dict
    let buf = bufnr('')

    let setwd = syntastic#util#isRunningWindows() ? 'setwd("' . escape(getcwd(), '"\') . '"); ' : ''
    let makeprg = self.getExecEscaped() . ' --slave --no-restore --no-save' .
        \ ' -e ' . syntastic#util#shescape(setwd . 'suppressPackageStartupMessages(library(lintr)); ' .
        \       'lint(cache = ' . g:syntastic_r_lintr_cache . ', commandArgs(TRUE), ' . g:syntastic_r_lintr_linters . ')') .
        \ ' --args ' . syntastic#util#shescape(bufname(buf))

    let errorformat =
        \ '%W%f:%l:%c: style: %m,' .
        \ '%W%f:%l:%c: warning: %m,' .
        \ '%E%f:%l:%c: error: %m,'

    call self.setWantSort(1)

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'r',
    \ 'name': 'lintr',
    \ 'exec': 'R',
    \ 'enable': 'enable_r_lintr_checker'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
