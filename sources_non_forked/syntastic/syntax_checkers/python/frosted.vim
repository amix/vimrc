"============================================================================
"File:        frosted.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_python_frosted_checker')
    finish
endif
let g:loaded_syntastic_python_frosted_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_frosted_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-vb' })

    let errorformat =
        \ '%f:%l:%c:%m,' .
        \ '%E%f:%l: %m,' .
        \ '%-Z%p^,' .
        \ '%-G%.%#'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'returns': [0, 1] })

    for e in loclist
        let e["col"] += 1

        let parts = matchlist(e.text, '\v^([EW]\d+):([^:]*):(.+)')
        if len(parts) >= 4
            let e["type"] = parts[1][0]
            let e["text"] = parts[3] . ' [' . parts[1] . ']'
            let e["hl"] = '\V\<' . escape(parts[2], '\') . '\>'
        elseif e["text"] =~? '\v^I\d+:'
            let e["valid"] = 0
        else
            let e["vcol"] = 0
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'frosted' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
