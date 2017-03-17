"============================================================================
"File:        checkbashisms.vim
"Description: Shell script syntax/style checking plugin for syntastic.vim
"Notes:       checkbashisms.pl can be downloaded from
"             http://debian.inode.at/debian/pool/main/d/devscripts/
"             as part of the devscripts package.
"============================================================================

if exists('g:loaded_syntastic_sh_checkbashisms_checker')
    finish
endif
let g:loaded_syntastic_sh_checkbashisms_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sh_checkbashisms_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-fx' })

    let errorformat =
        \ '%-Gscript %f is already a bash script; skipping,' .
        \ '%Eerror: %f: %m\, opened in line %l,' .
        \ '%Eerror: %f: %m,' .
        \ '%Ecannot open script %f for reading: %m,' .
        \ '%Wscript %f %m,%C%.# lines,' .
        \ '%Wpossible bashism in %f line %l (%m):,%C%.%#,%Z.%#,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'checkbashisms' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
