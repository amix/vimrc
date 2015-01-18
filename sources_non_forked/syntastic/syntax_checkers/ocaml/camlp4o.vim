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

if exists("g:loaded_syntastic_ocaml_camlp4o_checker")
    finish
endif
let g:loaded_syntastic_ocaml_camlp4o_checker = 1

if exists('g:syntastic_ocaml_camlp4r') && g:syntastic_ocaml_camlp4r != 0
    let s:ocamlpp="camlp4r"
else
    let s:ocamlpp="camlp4o"
endif

let s:save_cpo = &cpo
set cpo&vim

" Checker options {{{1

if !exists('g:syntastic_ocaml_use_ocamlc') || !executable('ocamlc')
    let g:syntastic_ocaml_use_ocamlc = 0
endif

if !exists('g:syntastic_ocaml_use_janestreet_core')
    let g:syntastic_ocaml_use_janestreet_core = 0
endif

if !exists('g:syntastic_ocaml_use_ocamlbuild') || !executable("ocamlbuild")
    let g:syntastic_ocaml_use_ocamlbuild = 0
endif

" }}}1

function! SyntaxCheckers_ocaml_camlp4o_IsAvailable() dict " {{{1
    return executable(s:ocamlpp)
endfunction " }}}1

function! SyntaxCheckers_ocaml_camlp4o_GetLocList() dict " {{{1
    let makeprg = s:GetMakeprg()
    if makeprg == ""
        return []
    endif

    let errorformat =
        \ '%AFile "%f"\, line %l\, characters %c-%*\d:,'.
        \ '%AFile "%f"\, line %l\, characters %c-%*\d (end at line %*\d\, character %*\d):,'.
        \ '%AFile "%f"\, line %l\, character %c:,'.
        \ '%AFile "%f"\, line %l\, character %c:%m,'.
        \ '%-GPreprocessing error %.%#,'.
        \ '%-GCommand exited %.%#,'.
        \ '%C%tarning %n: %m,'.
        \ '%C%m,'.
        \ '%-G+%.%#'

    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction " }}}1

" Utilities {{{1

function! s:GetMakeprg() " {{{2
    if g:syntastic_ocaml_use_ocamlc
        return s:GetOcamlcMakeprg()
    endif

    if g:syntastic_ocaml_use_ocamlbuild && isdirectory('_build')
        return s:GetOcamlBuildMakeprg()
    endif

    return s:GetOtherMakeprg()
endfunction " }}}2

function! s:GetOcamlcMakeprg() " {{{2
    if g:syntastic_ocaml_use_janestreet_core
        let build_cmd = "ocamlc -I "
        let build_cmd .= expand(g:syntastic_ocaml_janestreet_core_dir, 1)
        let build_cmd .= " -c " . syntastic#util#shexpand('%')
        return build_cmd
    else
        return "ocamlc -c " . syntastic#util#shexpand('%')
    endif
endfunction " }}}2

function! s:GetOcamlBuildMakeprg() " {{{2
    return "ocamlbuild -quiet -no-log -tag annot," . s:ocamlpp . " -no-links -no-hygiene -no-sanitize " .
                \ syntastic#util#shexpand('%:r') . ".cmi"
endfunction " }}}2

function! s:GetOtherMakeprg() " {{{2
    "TODO: give this function a better name?
    "
    "TODO: should use throw/catch instead of returning an empty makeprg

    let extension = expand('%:e', 1)
    let makeprg = ""

    if stridx(extension, 'mly') >= 0 && executable("menhir")
        " ocamlyacc output can't be redirected, so use menhir
        let makeprg = "menhir --only-preprocess " . syntastic#util#shexpand('%') . " >" . syntastic#util#DevNull()
    elseif stridx(extension,'mll') >= 0 && executable("ocamllex")
        let makeprg = "ocamllex -q " . syntastic#c#NullOutput() . " " . syntastic#util#shexpand('%')
    else
        let makeprg = "camlp4o " . syntastic#c#NullOutput() . " " . syntastic#util#shexpand('%')
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
