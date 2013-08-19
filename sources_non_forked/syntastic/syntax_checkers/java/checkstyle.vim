"============================================================================
"File:        checkstyle.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Dmitry Geurkov <d.geurkov at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" Tested with checkstyle 5.5
"============================================================================
if exists("g:loaded_syntastic_java_checkstyle_checker")
    finish
endif
let g:loaded_syntastic_java_checkstyle_checker=1

if !exists("g:syntastic_java_checkstyle_classpath")
    let g:syntastic_java_checkstyle_classpath = 'checkstyle-5.5-all.jar'
endif

if !exists("g:syntastic_java_checkstyle_conf_file")
    let g:syntastic_java_checkstyle_conf_file = 'sun_checks.xml'
endif

function! SyntaxCheckers_java_checkstyle_IsAvailable()
    return executable('java')
endfunction

function! SyntaxCheckers_java_checkstyle_Preprocess(errors)
    let out = copy(a:errors)
    for n in range(len(out))
        let parts = matchlist(out[n], '\(.*<file name="\)\([^"]\+\)\(">.*\)')
        if len(parts) >= 4
            let parts[2] = syntastic#util#decodeXMLEntities(parts[2])
            let out[n] = join(parts[1:3], '')
        endif
    endfor
    return out
endfunction

function! SyntaxCheckers_java_checkstyle_GetLocList()

    let fname = syntastic#util#shescape( expand('%:p:h') . '/' . expand('%:t') )

    if has('win32unix')
        let fname = substitute(system('cygpath -m ' . fname), '\%x00', '', 'g')
    endif

    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'java',
        \ 'args': '-cp ' . g:syntastic_java_checkstyle_classpath .
        \         ' com.puppycrawl.tools.checkstyle.Main -c ' . g:syntastic_java_checkstyle_conf_file .
        \         ' -f xml',
        \ 'fname': fname,
        \ 'filetype': 'java',
        \ 'subchecker': 'checkstyle' })

    let errorformat =
        \ '%P<file name="%f">,' .
        \ '%Q</file>,' .
        \ '%E<error line="%l" column="%c" severity="%trror" message="%m" source="%.%#"/>,' .
        \ '%E<error line="%l" severity="%trror" message="%m" source="%.%#"/>,' .
        \ '%E<error line="%l" column="%c" severity="%tarning" message="%m" source="%.%#"/>,' .
        \ '%E<error line="%l" severity="%tarning" message="%m" source="%.%#"/>,' .
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'preprocess': 'SyntaxCheckers_java_checkstyle_Preprocess',
        \ 'postprocess': ['cygwinRemoveCR', 'decodeXMLEntities'] })

endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'java',
    \ 'name': 'checkstyle'})
