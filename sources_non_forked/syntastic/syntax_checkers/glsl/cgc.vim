"============================================================================
"File:        glsl.vim
"Description: Syntax checker for OpenGL Shading Language
"Maintainer:  Joshua Rahm <joshuarahm@gmail.com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_glsl_cgc_checker')
    finish
endif
let g:loaded_syntastic_glsl_cgc_checker = 1

let s:glsl_extensions = {
        \ 'glslf': 'gpu_fp',
        \ 'glslv': 'gpu_vp',
        \ 'frag':  'gpu_fp',
        \ 'vert':  'gpu_vp',
        \ 'fp':    'gpu_fp',
        \ 'vp':    'gpu_vp'
    \ }

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_glsl_cgc_GetLocList() dict " {{{1
    let buf = bufnr('')
    let makeprg = self.makeprgBuild({
        \ 'args_before': '-oglsl -profile ' . s:GetProfile(buf),
        \ 'args': (exists('g:syntastic_glsl_options') ? ' ' . g:syntastic_glsl_options : '') })

    let errorformat =
        \ '%E%f(%l) : error %m,' .
        \ '%W%f(%l) : warning %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction " }}}1

" Utilities {{{1

function! s:GetProfile(buf) " {{{2
    let profile = matchstr(get(filter(getbufline(a:buf, 1, 100), 'v:val =~# "\\m\\C^//\\s*profile:"'), 0, ''), '\m\C^//\s*profile:\s*\zs.*')
    if profile ==# ''
        let extensions = syntastic#util#bufVar(a:buf, 'glsl_extensions', s:glsl_extensions)
        let profile = get(extensions, tolower(fnamemodify(bufname(a:buf), ':e')), 'gpu_vert')
    endif

    return profile
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \'filetype': 'glsl',
    \'name': 'cgc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
