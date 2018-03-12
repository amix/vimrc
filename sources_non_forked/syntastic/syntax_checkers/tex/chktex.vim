"============================================================================
"File:        chktex.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_tex_chktex_checker')
    finish
endif
let g:loaded_syntastic_tex_chktex_checker = 1

if !exists('g:syntastic_tex_chktex_showmsgs')
    let g:syntastic_tex_chktex_showmsgs = 1
endif

if !exists('g:syntastic_tex_chktex_sort')
    let g:syntastic_tex_chktex_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_tex_chktex_GetLocList() dict
    if !exists('s:sumb_quoting')
        let s:dumb_quoting = syntastic#util#isRunningWindows() && exists('+shellslash') && !&shellslash
    endif
    let makeprg = self.makeprgBuild({ 'args_after': (s:dumb_quoting ? ['-q', '-v1'] : ['-q', '-f', "%k:%n:%f:%l:%c:%m\n"]) })

    let errorformat =
        \ '%EError:%n:%f:%l:%v:%m,' .
        \ '%WWarning:%n:%f:%l:%v:%m,' .
        \ (g:syntastic_tex_chktex_showmsgs ? '%WMessage:%n:%f:%l:%v:%m,' : '') .
        \ '%EError %n in %f line %l: %m,' .
        \ '%WWarning %n in %f line %l: %m,' .
        \ (g:syntastic_tex_chktex_showmsgs ? '%WMessage %n in %f line %l: %m,' : '') .
        \ '%Z%p^,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'tex',
    \ 'name': 'chktex'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
