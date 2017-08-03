"============================================================================
"File:        gotype.vim
"Description: Perform syntactic and semantic checking of Go code using 'gotype'
"Maintainer:  luz <ne.tetewi@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_go_gotype_checker')
    finish
endif
let g:loaded_syntastic_go_gotype_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_go_gotype_GetLocList() dict
    let buf = bufnr('')

    if !exists('s:go_new')
        let command = syntastic#util#shescape(syntastic#util#bufVar(buf, 'go_go_exec', 'go')) . ' version'
        let version_output = syntastic#util#system(command)
        call self.log('finding go version: ' . string(command) . ': ' .
            \ string(split(version_output, "\n", 1)) .
            \ (v:shell_error ? ' (exit code ' . v:shell_error . ')' : ''))
        let parsed_ver = syntastic#util#parseVersion(version_output)
        if len(parsed_ver)
            let s:go_new = syntastic#util#versionIsAtLeast(parsed_ver, [1, 8])
        else
            call syntastic#log#error("checker " . self.getCName() . ": can't parse go version (abnormal termination?)")
            return []
        endif
    endif

    let makeprg = self.makeprgBuild({
        \ 'args': (bufname(buf) =~# '\m_test\.go$' ? (s:go_new ? '-t' : '-a') : ''),
        \ 'fname': '.' })

    let errorformat =
        \ '%f:%l:%c: %m,' .
        \ '%-G%.%#'

    " gotype needs the full go package to test types properly. Just cwd to
    " the package for the same reasons specified in go.vim ("figuring out
    " the import path is fickle").

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'cwd': fnamemodify(bufname(buf), ':p:h'),
        \ 'defaults': {'type': 'e'} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'go',
    \ 'name': 'gotype'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
