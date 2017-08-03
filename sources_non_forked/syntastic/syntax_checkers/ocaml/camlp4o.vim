"============================================================================
"File:        ocaml.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Török Edwin <edwintorok at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ocaml_camlp4o_checker')
    finish
endif
let g:loaded_syntastic_ocaml_camlp4o_checker = 1

let s:save_cpo = &cpo
set cpo&vim

" Checker options {{{1

if !exists('g:syntastic_ocaml_use_ocamlc') || !executable('ocamlc')
    let g:syntastic_ocaml_use_ocamlc = 0
endif

if !exists('g:syntastic_ocaml_use_janestreet_core')
    let g:syntastic_ocaml_use_janestreet_core = 0
endif

if !exists('g:syntastic_ocaml_janestreet_core_dir')
    let g:syntastic_ocaml_janestreet_core_dir = '.'
endif

if !exists('g:syntastic_ocaml_use_ocamlbuild') || !executable('ocamlbuild')
    let g:syntastic_ocaml_use_ocamlbuild = 0
endif

" }}}1

function! SyntaxCheckers_ocaml_camlp4o_IsAvailable() dict " {{{1
    let s:ocamlpp = get(g:, 'syntastic_ocaml_camlp4r', 0) ? 'camlp4r' : 'camlp4o'
    return executable(s:ocamlpp)
endfunction " }}}1

function! SyntaxCheckers_ocaml_camlp4o_GetLocList() dict " {{{1
    let buf = bufnr('')
    let makeprg = s:GetMakeprg(buf)
    if makeprg ==# ''
        return []
    endif

    let errorformat =
        \ '%WWarning: File "%f"\, line %l\, chars %c-%n:,'.
        \ '%WWarning: line %l\, chars %c-%n:,'.
        \ '%AFile "%f"\, line %l\, characters %c-%n:,'.
        \ '%AFile "%f"\, line %l\, characters %c-%*\d (end at line %*\d\, character %*\d):,'.
        \ '%AFile "%f"\, line %l\, character %c:,'.
        \ '%AFile "%f"\, line %l\, character %c:%m,'.
        \ '%-GPreprocessing error %.%#,'.
        \ '%-GCommand exited %.%#,'.
        \ '%C%tarning %*\d: %m,'.
        \ '%C%m,'.
        \ '%-G+%.%#'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'bufnr': buf} })

    for e in loclist
        if get(e, 'col', 0) && get(e, 'nr', 0)
            let e['hl'] = '\%>' . (e['col'] - 1) . 'c\%<' . (e['nr'] + 1) . 'c'
            let e['nr'] = 0
        endif
    endfor

    return loclist
endfunction " }}}1

" Utilities {{{1

function! s:GetMakeprg(buf) " {{{2
    return
        \ g:syntastic_ocaml_use_ocamlc ? g:syntastic_ocaml_use_ocamlc :
        \ (g:syntastic_ocaml_use_ocamlbuild && isdirectory('_build')) ? s:GetOcamlcMakeprg(a:buf) :
        \ s:GetOtherMakeprg(a:buf)
endfunction " }}}2

function! s:GetOcamlcMakeprg(buf) " {{{2
    let build_cmd = g:syntastic_ocaml_use_janestreet_core ?
        \ 'ocamlc -I ' . syntastic#util#shexpand(g:syntastic_ocaml_janestreet_core_dir) : 'ocamlc'
    let build_cmd .= ' -c ' . syntastic#util#shescape(bufname(a:buf))
    return build_cmd
endfunction " }}}2

function! s:GetOcamlBuildMakeprg(buf) " {{{2
    return 'ocamlbuild -quiet -no-log -tag annot,' . s:ocamlpp . ' -no-links -no-hygiene -no-sanitize ' .
        \ syntastic#util#shexpand(fnamemodify(bufname(a:buf), ':r')) . '.cmi'
endfunction " }}}2

function! s:GetOtherMakeprg(buf) " {{{2
    "TODO: give this function a better name?
    "
    "TODO: should use throw/catch instead of returning an empty makeprg

    let fname = bufname(a:buf)
    let extension = fnamemodify(fname, ':e')
    let makeprg = ''

    if stridx(extension, 'mly') >= 0 && executable('menhir')
        " ocamlyacc output can't be redirected, so use menhir
        let makeprg = 'menhir --only-preprocess ' . syntastic#util#shescape(fname) . ' >' . syntastic#util#DevNull()
    elseif stridx(extension,'mll') >= 0 && executable('ocamllex')
        let makeprg = 'ocamllex -q ' . syntastic#c#NullOutput() . ' ' . syntastic#util#shescape(fname)
    else
        let makeprg = 'camlp4o ' . syntastic#c#NullOutput() . ' ' . syntastic#util#shescape(fname)
    endif

    return makeprg
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ocaml',
    \ 'name': 'camlp4o'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
