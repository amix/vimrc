"============================================================================
"File:        vala.vim
"Description: Syntax checking plugin for syntastic
"Maintainer:  Konstantin Stepanov (me@kstep.me)
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_vala_valac_checker')
    finish
endif
let g:loaded_syntastic_vala_valac_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_vala_valac_GetHighlightRegex(pos) " {{{1
    let length = strlen(matchstr(a:pos['text'], '\m\^\+$'))
    return '\%>' . (a:pos['col'] - 1) . 'c\%<' . (a:pos['col'] + length) . 'c'
endfunction " }}}1

function! SyntaxCheckers_vala_valac_GetLocList() dict " {{{1
    let buf = bufnr('')
    let makeprg = self.makeprgBuild({
            \ 'args': '-C ' .
            \   s:GetValaOpts(buf, 'modules',   'modules',  '--pkg') . ' ' .
            \   s:GetValaOpts(buf, 'vapi_dirs', 'vapidirs', '--vapidir'),
        \ })

    let errorformat =
        \ '%A%f:%l.%c-%\d%\+.%\d%\+: %t%[a-z]%\+: %m,'.
        \ '%C%m,'.
        \ '%Z%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction " }}}1

" Utilities {{{1

function! s:GetValaOpts(buf, name, comment, cmd) " {{{2
    let var = syntastic#util#bufVar(a:buf, 'vala_' . a:name)
    if type(var) == type([])
        let opts = map(copy(var), 'syntastic#util#shescape(v:val)')
    elseif type(var) == type('')
        if var !=# ''
            let opts = split(var, '\m\s\+')
        else
            let opts = []
            for line in filter(getbufline(a:buf, 1, 100), 'v:val =~# ' . string('\m^//\s\+' . a:comment . ':\s*'))
                call extend(opts, split( matchstr(line, '\m^//\s\+' . a:comment . ':\s*\zs.*'), '\m\s\+' ))
            endfor
        endif
    else
        call syntastic#log#error('syntastic_vala_' . a:name . ' must be either a list, or a string')
        return ''
    endif

    return join(map(opts, string(a:cmd . ' ') . ' . v:val'))
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vala',
    \ 'name': 'valac'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
