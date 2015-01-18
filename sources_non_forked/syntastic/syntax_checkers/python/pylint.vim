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

if !exists('g:syntastic_python_pylint_sort')
    let g:syntastic_python_pylint_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

let s:pylint_new = -1

function! SyntaxCheckers_python_pylint_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    try
        " On Windows the version is shown as "pylint-script.py 1.0.0".
        " On Gentoo Linux it's "pylint-python2.7 0.28.0".
        " On NixOS, that would be ".pylint-wrapped 0.26.0".
        " On Arch Linux it's "pylint2 1.1.0".
        " On new-ish Fedora it's "python3-pylint 1.2.0".
        " Have you guys considered switching to creative writing yet? ;)

        let pylint_version = filter( split(system(self.getExecEscaped() . ' --version'), '\m, \=\|\n'),
            \ 'v:val =~# ''\m^\(python[-0-9]*-\|\.\)\=pylint[-0-9]*\>''' )[0]
        let ver = syntastic#util#parseVersion(substitute(pylint_version, '\v^\S+\s+', '', ''))

        call self.log(self.getExec() . ' version =', ver)

        let s:pylint_new = syntastic#util#versionIsAtLeast(ver, [1])
    catch /\m^Vim\%((\a\+)\)\=:E684/
        call syntastic#log#error("checker python/pylint: can't parse version string (abnormal termination?)")
        let s:pylint_new = -1
    endtry

    return s:pylint_new >= 0
endfunction

function! SyntaxCheckers_python_pylint_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_after': (s:pylint_new ?
        \       '-f text --msg-template="{path}:{line}:{column}:{C}: [{symbol}] {msg}" -r n' :
        \       '-f parseable -r n -i y') })

    let errorformat =
        \ '%A%f:%l:%c:%t: %m,' .
        \ '%A%f:%l: %m,' .
        \ '%A%f:(%l): %m,' .
        \ '%-Z%p^%.%#,' .
        \ '%-G%.%#'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
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

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylint' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
