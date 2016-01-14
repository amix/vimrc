"============================================================================
"File:        ghc-mod.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Anthony Carapetis <anthony.carapetis at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_haskell_ghc_mod_checker')
    finish
endif
let g:loaded_syntastic_haskell_ghc_mod_checker = 1

let s:ghc_mod_new = -1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_haskell_ghc_mod_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    " ghc-mod 5.0.0 and later needs the "version" command to print the
    " version.  But the "version" command appeared in 4.1.0.  Thus, we need to
    " know the version in order to know how to find out the version. :)

    " Try "ghc-mod version".
    let version_output = split(syntastic#util#system(self.getExecEscaped() . ' version'), '\n', 1)
    let ver = filter(copy(version_output), 'v:val =~# ''\m\sversion''')
    if !len(ver)
        " That didn't work.  Try "ghc-mod" alone.
        let version_output = split(syntastic#util#system(self.getExecEscaped()), '\n', 1)
        let ver = filter(copy(version_output), 'v:val =~# ''\m\sversion''')
    endif
    let parsed_ver = len(ver) ? syntastic#util#parseVersion(ver[0]) : []

    if len(parsed_ver)
        " Encouraged by the great success in finding out the version, now we
        " need either a Vim that can handle NULs in system() output, or a
        " ghc-mod that has the "--boundary" option.
        call self.setVersion(parsed_ver)
        let s:ghc_mod_new = syntastic#util#versionIsAtLeast(parsed_ver, [2, 1, 2])
    else
        call syntastic#log#ndebug(g:_SYNTASTIC_DEBUG_LOCLIST, 'checker output:', version_output)
        call syntastic#log#error("checker haskell/ghc_mod: can't parse version string (abnormal termination?)")
        let s:ghc_mod_new = -1
    endif

    " ghc-mod 5.4.0 wants to run in the root directory of the project;
    " syntastic can't cope with the resulting complications
    "
    " References:
    " https://hackage.haskell.org/package/ghc-mod-5.4.0.0/changelog
    let s:ghc_mod_bailout = syntastic#util#versionIsAtLeast(parsed_ver, [5, 4])

    return (s:ghc_mod_new >= 0) && (v:version >= 704 || s:ghc_mod_new) && !s:ghc_mod_bailout
endfunction

function! SyntaxCheckers_haskell_ghc_mod_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'exe': self.getExecEscaped() . ' check' . (s:ghc_mod_new ? ' --boundary=""' : '') })

    let errorformat =
        \ '%-G%\s%#,' .
        \ '%f:%l:%c:%trror: %m,' .
        \ '%f:%l:%c:%tarning: %m,'.
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l:%c:%m,' .
        \ '%E%f:%l:%c:,' .
        \ '%Z%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'iconv',
        \ 'postprocess': ['compressWhitespace'],
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'ghc_mod',
    \ 'exec': 'ghc-mod' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
