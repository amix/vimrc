"============================================================================
"File:        shellcheck.vim
"Description: Shell script syntax/style checking plugin for syntastic.vim
"============================================================================

if exists('g:loaded_syntastic_sh_shellcheck_checker')
    finish
endif
let g:loaded_syntastic_sh_shellcheck_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sh_shellcheck_GetLocList() dict " {{{1
    let makeprg = self.makeprgBuild({
        \ 'args': s:GetShell(),
        \ 'args_after': '-f gcc' })

    let errorformat =
        \ '%f:%l:%c: %trror: %m,' .
        \ '%f:%l:%c: %tarning: %m,' .
        \ '%f:%l:%c: %tote: %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'returns': [0, 1] })

    for e in loclist
        if e['type'] ==? 'n'
            let e['type'] = 'w'
            let e['subtype'] = 'Style'
        endif
    endfor

    return loclist
endfunction " }}}1

" Utilities {{{1

function! s:GetShell() " {{{2
    let sh = ''

    if syntastic#util#parseShebang()['exe'] ==# ''
        if syntastic#util#rawVar('is_kornshell', 0) || syntastic#util#rawVar('is_posix', 0)
            let sh = 'ksh'
        elseif syntastic#util#rawVar('is_bash', 0)
            let sh = 'bash'
        elseif syntastic#util#rawVar('is_sh', 0)
            let sh = 'sh'
        endif
    endif

    return sh !=# '' ? '-s ' . sh : ''
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sh',
    \ 'name': 'shellcheck' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
