"============================================================================
"File:        mri.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_ruby_mri_checker")
    finish
endif
let g:loaded_syntastic_ruby_mri_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_mri_GetHighlightRegex(i)
    if stridx(a:i['text'], 'assigned but unused variable') >= 0
        let term = split(a:i['text'], ' - ')[1]
        return '\V\<'.term.'\>'
    endif

    return ''
endfunction

function! SyntaxCheckers_ruby_mri_GetLocList() dict
    if !exists('g:syntastic_ruby_exec')
        let g:syntastic_ruby_exec = self.getExec()
    endif

    let exe = syntastic#util#shexpand(g:syntastic_ruby_exec)
    if !syntastic#util#isRunningWindows()
        let exe = 'RUBYOPT= ' . exe
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe': exe,
        \ 'args_after': '-w -T1 -c' })

    "this is a hack to filter out a repeated useless warning in rspec files
    "containing lines like
    "
    "  foo.should == 'bar'
    "
    "Which always generate the warning below. Note that ruby >= 1.9.3 includes
    "the word "possibly" in the warning
    let errorformat = '%-G%.%#warning: %\(possibly %\)%\?useless use of == in void context,'

    " filter out lines starting with ...
    " long lines are truncated and wrapped in ... %p then returns the wrong
    " column offset
    let errorformat .= '%-G%\%.%\%.%\%.%.%#,'

    let errorformat .=
        \ '%-GSyntax OK,'.
        \ '%E%f:%l: syntax error\, %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: warning: %m,'.
        \ '%Z%p^,'.
        \ '%W%f:%l: %m,'.
        \ '%-C%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'mri',
    \ 'exec': 'ruby'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
