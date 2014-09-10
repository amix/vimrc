"============================================================================
"File:        sass.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_sass_sass_checker")
    finish
endif
let g:loaded_syntastic_sass_sass_checker = 1

"sass caching for large files drastically speeds up the checking, but store it
"in a temp location otherwise sass puts .sass_cache dirs in the users project
let s:sass_cache_location = tempname()
lockvar s:sass_cache_location

"By default do not check partials as unknown variables are a syntax error
if !exists("g:syntastic_sass_check_partials")
    let g:syntastic_sass_check_partials = 0
endif

"use compass imports if available
let s:imports = ""
if executable("compass")
    let s:imports = "--compass"
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_sass_sass_GetLocList() dict
    if !g:syntastic_sass_check_partials && expand('%:t')[0] == '_'
        return []
    endif

    let makeprg = self.makeprgBuild({
        \ 'args_before': '--cache-location ' . s:sass_cache_location . ' ' . s:imports . ' --check' })

    let errorformat =
        \ '%ESyntax %trror: %m,' .
        \ '%+C              %.%#,' .
        \ '%C        on line %l of %f\, %.%#,' .
        \ '%C        on line %l of %f,' .
        \ '%-G %\+from line %.%#,' .
        \ '%-G %\+Use --trace for backtrace.,' .
        \ '%W%>DEPRECATION WARNING on line %l of %f:,' .
        \ '%+C%>  %.%#,' .
        \ '%W%>WARNING: on line %l of %f:,' .
        \ '%+C%>  %.%#,' .
        \ '%W%>WARNING on line %l of %f: %m,' .
        \ '%+C%>  %.%#,' .
        \ '%W%>WARNING on line %l of %f:,' .
        \ '%Z%m,' .
        \ '%W%>WARNING: %m,' .
        \ '%C         on line %l of %f\, %.%#,' .
        \ '%C         on line %l of %f,' .
        \ '%-G %\+from line %.%#,' .
        \ 'Syntax %trror on line %l: %m,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['compressWhitespace'] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'sass',
    \ 'name': 'sass'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
