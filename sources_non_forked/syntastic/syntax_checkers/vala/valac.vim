"============================================================================
"File:        vala.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Konstantin Stepanov (me@kstep.me)
"Notes:       Add special comment line into your vala file starting with
"             "// modules: " and containing space delimited list of vala
"             modules, used by the file, so this script can build correct
"             --pkg arguments.
"             Add another special comment line into your vala file starting
"             with "// vapidirs: " followed by a space delimited list of
"             the vapi directories so this script can build with the correct
"             --vapidir arguments
"             Alternatively you can set the g:syntastic_vala_modules array
"             and/or the g:syntastic_vala_vapidirs array
"             in your .vimrc or .lvimrc with localvimrc plugin
"             (http://www.vim.org/scripts/script.php?script_id=441).
"             Valac compiler is not the fastest thing in the world, so you
"             may want to disable this plugin with
"             let g:syntastic_vala_check_disabled = 1 command in your .vimrc or
"             command line. Unlet this variable to set it to 0 to reenable
"             this checker.
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

function! SyntaxCheckers_vala_valac_GetHighlightRegex(pos)
    let length = strlen(matchstr(a:pos['text'], '\m\^\+$'))
    return '\%>' . (a:pos['col'] - 1) . 'c\%<' . (a:pos['col'] + length) . 'c'
endfunction

function! s:GetValaModules()
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
endfunction

function! s:GetValaVapiDirs()
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
endfunction

function! SyntaxCheckers_vala_valac_GetLocList() dict
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
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'vala',
    \ 'name': 'valac'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
