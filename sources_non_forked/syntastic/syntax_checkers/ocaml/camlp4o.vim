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
"
" The more reliable way to check for a single .ml file is to use ocamlc.
" You can do that setting this in your .vimrc:
"
"   let g:syntastic_ocaml_use_ocamlc = 1
" It's possible to use ocamlc in conjuction with Jane Street's Core. In order
" to do that, you have to specify this in your .vimrc:
"
"   let g:syntastic_ocaml_use_janestreet_core = 1
"   let g:syntastic_ocaml_janestreet_core_dir = <path>
"
" Where path is the path to your core installation (usually a collection of
" .cmx and .cmxa files).
"
"
" By default the camlp4o preprocessor is used to check the syntax of .ml, and .mli files,
" ocamllex is used to check .mll files and menhir is used to check .mly files.
" The output is all redirected to /dev/null, nothing is written to the disk.
"
" If your source code needs camlp4r then you can define this in your .vimrc:
"
"   let g:syntastic_ocaml_camlp4r = 1
"
" If you used some syntax extensions, or you want to also typecheck the source
" code, then you can define this:
"
"   let g:syntastic_ocaml_use_ocamlbuild = 1
"
" This will run ocamlbuild <name>.inferred.mli, so it will write to your _build
" directory (and possibly rebuild your myocamlbuild.ml plugin), only enable this
" if you are ok with that.
"
" If you are using syntax extensions / external libraries and have a properly
" set up _tags (and myocamlbuild.ml file) then it should just work
" to enable this flag and get syntax / type checks through syntastic.
"
" For best results your current directory should be the project root
" (same situation if you want useful output from :make).

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

function! SyntaxCheckers_ocaml_camlp4o_IsAvailable() dict
    return executable(s:ocamlpp)
endfunction

if !exists('g:syntastic_ocaml_use_ocamlc') || !executable('ocamlc')
    let g:syntastic_ocaml_use_ocamlc = 0
endif

if !exists('g:syntastic_ocaml_use_janestreet_core')
    let g:syntastic_ocaml_use_janestreet_core = 0
endif

if !exists('g:syntastic_ocaml_use_ocamlbuild') || !executable("ocamlbuild")
    let g:syntastic_ocaml_use_ocamlbuild = 0
endif

function! SyntaxCheckers_ocaml_camlp4o_GetLocList() dict
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
endfunction

function! s:GetMakeprg()
    if g:syntastic_ocaml_use_ocamlc
        return s:GetOcamlcMakeprg()
    endif

    if g:syntastic_ocaml_use_ocamlbuild && isdirectory('_build')
        return s:GetOcamlBuildMakeprg()
    endif

    return s:GetOtherMakeprg()
endfunction

function! s:GetOcamlcMakeprg()
    if g:syntastic_ocaml_use_janestreet_core
        let build_cmd = "ocamlc -I "
        let build_cmd .= expand(g:syntastic_ocaml_janestreet_core_dir)
        let build_cmd .= " -c " . syntastic#util#shexpand('%')
        return build_cmd
    else
        return "ocamlc -c " . syntastic#util#shexpand('%')
    endif
endfunction

function! s:GetOcamlBuildMakeprg()
    return "ocamlbuild -quiet -no-log -tag annot," . s:ocamlpp . " -no-links -no-hygiene -no-sanitize " .
                \ syntastic#util#shexpand('%:r') . ".cmi"
endfunction

function! s:GetOtherMakeprg()
    "TODO: give this function a better name?
    "
    "TODO: should use throw/catch instead of returning an empty makeprg

    let extension = expand('%:e')
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
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ocaml',
    \ 'name': 'camlp4o'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
