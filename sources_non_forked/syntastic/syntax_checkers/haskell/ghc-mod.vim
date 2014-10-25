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
    let ver = filter(split(system(self.getExecEscaped() . ' version'), '\n'), 'v:val =~# ''\m^ghc-mod version''')
    if !len(ver)
        " That didn't work.  Try "ghc-mod" alone.
        let ver = filter(split(system(self.getExecEscaped()), '\n'), 'v:val =~# ''\m^ghc-mod version''')
    endif

    if len(ver)
        " Encouraged by the great success in finding out the version, now we
        " need either a Vim that can handle NULs in system() output, or a
        " ghc-mod that has the "--boundary" option.
        let parsed_ver = syntastic#util#parseVersion(ver[0])
        call self.log(self.getExec() . ' version =', parsed_ver)
        let s:ghc_mod_new = syntastic#util#versionIsAtLeast(parsed_ver, [2, 1, 2])
    else
        call syntastic#log#error("checker haskell/ghc_mod: can't parse version string (abnormal termination?)")
        let s:ghc_mod_new = -1
    endif

    return (s:ghc_mod_new >= 0) && (v:version >= 704 || s:ghc_mod_new)
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
        \ 'postprocess': ['compressWhitespace'],
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'haskell',
    \ 'name': 'ghc_mod',
    \ 'exec': 'ghc-mod' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
