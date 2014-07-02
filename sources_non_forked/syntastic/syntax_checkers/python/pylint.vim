"============================================================================
"File:        pylint.vim
"Description: Syntax checking plugin for syntastic.vim
"Author:      Parantapa Bhattacharya <parantapa at gmail dot com>
"
"============================================================================

if exists("g:loaded_syntastic_python_pylint_checker")
    finish
endif
let g:loaded_syntastic_python_pylint_checker = 1

let s:pylint_new = -1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pylint_IsAvailable() dict
    let exe = self.getExec()
    let s:pylint_new = executable(exe) ? s:PylintNew(exe) : -1
    return s:pylint_new >= 0
endfunction

function! SyntaxCheckers_python_pylint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe_before': (syntastic#util#isRunningWindows() ? '' : 'TERM=dumb'),
        \ 'args_after': (s:pylint_new ? '-f text --msg-template="{path}:{line}:{column}:{C}: [{symbol}] {msg}" -r n' : '-f parseable -r n -i y') })

    let errorformat =
        \ '%A%f:%l:%c:%t: %m,' .
        \ '%A%f:%l: %m,' .
        \ '%A%f:(%l): %m,' .
        \ '%-Z%p^%.%#,' .
        \ '%-G%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': range(32) })

    for e in loclist
        if !s:pylint_new
            let e['type'] = e['text'][1]
        endif

        if e['type'] =~? '\m^[EF]'
            let e['type'] = 'E'
        elseif e['type'] =~? '\m^[CRW]'
            let e['type'] = 'W'
        else
            let e['valid'] = 0
        endif

        let e['col'] += 1
        let e['vcol'] = 0
    endfor

    call self.setWantSort(1)

    return loclist
endfunction

function! s:PylintNew(exe)
    let exe = syntastic#util#shescape(a:exe)
    try
        " On Windows the version is shown as "pylint-script.py 1.0.0".
        " On Gentoo Linux it's "pylint-python2.7 0.28.0".
        " On NixOS, that would be ".pylint-wrapped 0.26.0".
        " On Arch Linux it's "pylint2 1.1.0".
        " Have you guys considered switching to creative writing yet? ;)
        let pylint_version = filter(split(system(exe . ' --version'), '\m, \=\|\n'), 'v:val =~# ''\m^\.\=pylint[-0-9]*\>''')[0]
        let pylint_version = substitute(pylint_version, '\v^\S+\s+', '', '')
        let ret = syntastic#util#versionIsAtLeast(syntastic#util#parseVersion(pylint_version), [1])
    catch /\m^Vim\%((\a\+)\)\=:E684/
        call syntastic#log#error("checker python/pylint: can't parse version string (abnormal termination?)")
        let ret = -1
    endtry
    return ret
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
