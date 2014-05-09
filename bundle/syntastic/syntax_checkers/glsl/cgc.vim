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

if exists("g:loaded_syntastic_glsl_cgc_checker")
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

function! SyntaxCheckers_glsl_cgc_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args_before': '-oglsl -profile ' . s:GetProfile(),
        \ 'args': (exists('g:syntastic_glsl_options') ? ' ' . g:syntastic_glsl_options : '') })

    let errorformat =
        \ "%E%f(%l) : error %m," .
        \ "%W%f(%l) : warning %m"

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat })
endfunction

function! s:GetProfile()
    let save_view = winsaveview()
    let old_foldenable = &foldenable
    let old_lazyredraw = &lazyredraw

    let &lazyredraw = 1
    let &foldenable = 0
    call cursor(1, 1)

    let magic = '\m\C^// profile:\s*'
    let line = search(magic, 'c')

    call winrestview(save_view)
    let &foldenable = old_foldenable
    let &lazyredraw = old_lazyredraw

    if line
        let profile = matchstr(getline(line), magic . '\zs.*')
    else
        let extensions = exists('g:syntastic_glsl_extensions') ? g:syntastic_glsl_extensions : s:glsl_extensions
        let profile = get(extensions, tolower(expand('%:e')), 'gpu_vert')
    endif

    return profile
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \'filetype': 'glsl',
    \'name': 'cgc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
