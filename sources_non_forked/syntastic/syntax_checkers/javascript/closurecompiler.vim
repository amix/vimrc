"============================================================================
"File:        closurecompiler.vim
"Description: Javascript syntax checker - using Google Closure Compiler
"Maintainer:  Motohiro Takayama <mootoh at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"============================================================================

if exists("g:loaded_syntastic_javascript_closurecompiler_checker")
    finish
endif
let g:loaded_syntastic_javascript_closurecompiler_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_javascript_closurecompiler_IsAvailable() dict
    call syntastic#log#deprecationWarn('javascript_closure_compiler_path', 'javascript_closurecompiler_path')

    if !executable(self.getExec())
        return 0
    endif

    let s:has_script = exists('g:syntastic_javascript_closurecompiler_script')
    if s:has_script
        return 1
    endif

    let cp = get(g:, 'syntastic_javascript_closurecompiler_path', '')
    call self.log('g:syntastic_javascript_closurecompiler_path =', cp)

    let jar = expand(cp, 1)
    call self.log('filereadable(' . string(jar) . ') = ' . filereadable(jar))

    return filereadable(jar)
endfunction

function! SyntaxCheckers_javascript_closurecompiler_GetLocList() dict
    call syntastic#log#deprecationWarn('javascript_closure_compiler_options', 'javascript_closurecompiler_args')
    call syntastic#log#deprecationWarn('javascript_closure_compiler_file_list', 'javascript_closurecompiler_file_list')

    let flist = expand(get(g:, 'syntastic_javascript_closurecompiler_file_list', ''), 1)
    if filereadable(flist)
        let file_list = map( readfile(flist), 'expand(v:var, 1)' )
    else
        let file_list = [expand('%', 1)]
    endif

    let makeprg = self.makeprgBuild({
        \ 'exe_after': (s:has_script ? [] : ['-jar', expand(g:syntastic_javascript_closurecompiler_path, 1)]),
        \ 'args_after': '--js',
        \ 'fname': file_list })

    let errorformat =
        \ '%-GOK,'.
        \ '%E%f:%l: ERROR - %m,'.
        \ '%W%f:%l: WARNING - %m,'.
        \ '%Z%p^'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'javascript',
    \ 'name': 'closurecompiler',
    \ 'exec': get(g:, 'syntastic_javascript_closurecompiler_script', 'java')})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
