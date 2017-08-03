"============================================================================
"File:        pc_lint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Steve Bragg <steve at empresseffects dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_c_pc_lint_checker')
    finish
endif
let g:loaded_syntastic_c_pc_lint_checker = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:syntastic_pc_lint_config_file')
    let g:syntastic_pc_lint_config_file = 'options.lnt'
endif

function! SyntaxCheckers_c_pc_lint_GetLocList() dict
    let buf = bufnr('')
    let config = syntastic#util#findFileInParent(g:syntastic_pc_lint_config_file, fnamemodify(bufname(buf), ':p:h'))
    call self.log('config =', config)

    " -hFs1         - show filename, add space after messages, try to make message 1 line
    " -width(0,0)   - make sure there are no line breaks
    " -t            - set tab size
    " -v            - turn off verbosity
    let makeprg = self.makeprgBuild({
        \ 'args': (filereadable(config) ? syntastic#util#shescape(fnamemodify(config, ':p')) : ''),
        \ 'args_after': ['-hFs1', '-width(0,0)', '-t' . &tabstop, '-format=%f:%l:%C:%t:%n:%m'] })

    let errorformat =
        \ '%E%f:%l:%v:Error:%n:%m,' .
        \ '%W%f:%l:%v:Warning:%n:%m,' .
        \ '%I%f:%l:%v:Info:%n:%m,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['cygwinRemoveCR'] })

    for e in loclist
        if e['type'] ==? 'I'
            let e['type'] = 'W'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'c',
    \ 'name': 'pc_lint',
    \ 'exec': 'lint-nt'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
