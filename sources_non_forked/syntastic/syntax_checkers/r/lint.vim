"============================================================================
"File:        lint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_r_lint_checker")
    finish
endif
let g:loaded_syntastic_r_lint_checker = 1

if !exists('g:syntastic_r_lint_styles')
    let g:syntastic_r_lint_styles = 'lint.style'
endif

if !exists('g:syntastic_r_lint_sort')
    let g:syntastic_r_lint_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_r_lint_GetHighlightRegex(item)
    let term = matchstr(a:item['text'], '\m`\zs[^`]\+\ze`')
    if term == ''
        let term = matchstr(a:item['text'], "\\m'\\zs[^']\\+\\ze'")
    endif
    return term != '' ? '\V' . escape(term, '\') : ''
endfunction

function! SyntaxCheckers_r_lint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif
    call system(self.getExecEscaped() . ' --slave --restore --no-save -e ' . syntastic#util#shescape('library(lint)'))
    return v:shell_error == 0
endfunction

function! SyntaxCheckers_r_lint_GetLocList() dict
    let setwd = syntastic#util#isRunningWindows() ? 'setwd("' . escape(getcwd(), '"\') . '"); ' : ''
    let setwd = 'setwd("' . escape(getcwd(), '"\') . '"); '
    let makeprg = self.getExecEscaped() . ' --slave --restore --no-save' .
        \ ' -e ' . syntastic#util#shescape(setwd . 'library(lint); ' .
        \       'try(lint(commandArgs(TRUE), ' . g:syntastic_r_lint_styles . '))') .
        \ ' --args ' . syntastic#util#shexpand('%')

    let errorformat =
        \ '%t:%f:%l:%v: %m,' .
        \ '%t:%f:%l: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'preprocess': 'rparse',
        \ 'returns': [0] })

    for e in loclist
        if e['type'] == 'F'
            " parse error
            let e['type'] = 'E'
            call remove(e, 'subtype')
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'r',
    \ 'name': 'lint',
    \ 'exec': 'R' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
