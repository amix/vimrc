"============================================================================
"File:        vala.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Konstantin Stepanov (me@kstep.me)
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_vala_valac_checker")
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
    let vala_pkg_args = join(map(s:GetValaModules(), '"--pkg ".v:val'), ' ')
    let vala_vapi_args = join(map(s:GetValaVapiDirs(), '"--vapidir ".v:val'), ' ')
    let makeprg = self.makeprgBuild({ 'args': '-C ' . vala_pkg_args . " " . vala_vapi_args })

    let errorformat =
        \ '%A%f:%l.%c-%\d%\+.%\d%\+: %t%[a-z]%\+: %m,'.
        \ '%C%m,'.
        \ '%Z%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction " }}}1

" Utilities {{{1

function! s:GetValaModules() " {{{2
    if exists('g:syntastic_vala_modules')
        if type(g:syntastic_vala_modules) == type('')
            return split(g:syntastic_vala_modules, '\s\+')
        elseif type(g:syntastic_vala_modules) == type([])
            return copy(g:syntastic_vala_modules)
        else
            echoerr 'g:syntastic_vala_modules must be either list or string: fallback to in file modules string'
        endif
    endif

    let modules_line = search('^// modules: ', 'n')
    let modules_str = getline(modules_line)
    return split(strpart(modules_str, 12), '\s\+')
endfunction " }}}2

function! s:GetValaVapiDirs() " {{{2
    if exists('g:syntastic_vala_vapi_dirs')
        if type(g:syntastic_vala_vapi_dirs) == type('')
            return split(g:syntastic_vala_vapi_dirs, '\s\+')
        elseif type(g:syntastic_vala_vapi_dirs) == type([])
            return copy(g:syntastic_vala_vapi_dirs)
        else
            echoerr 'g:syntastic_vala_vapi_dirs must be either a list, or a string: fallback to in-file modules string'
        endif
    endif

    let vapi_line = search('^//\s*vapidirs:\s*','n')
    let vapi_str = getline(vapi_line)
    return split( substitute( vapi_str, '^//\s*vapidirs:\s*', '', 'g' ), '\s\+' )
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vala',
    \ 'name': 'valac'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
