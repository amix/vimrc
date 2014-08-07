"============================================================================
"File:        pep257.vim
"Description: Docstring style checking plugin for syntastic.vim
"============================================================================
"
" For details about pep257 see: https://github.com/GreenSteam/pep257

if exists('g:loaded_syntastic_python_pep257_checker')
    finish
endif
let g:loaded_syntastic_python_pep257_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pep257_GetLocList() dict
    if !exists('s:pep257_new')
        let s:pep257_new = syntastic#util#versionIsAtLeast(syntastic#util#getVersion(
            \ self.getExecEscaped() . ' --version'), [0, 3])
    endif

    let makeprg = self.makeprgBuild({})

    if s:pep257_new
        let errorformat =
            \ '%E%f:%l %.%#:,' .
            \ '%+C        %m'
    else
        let errorformat =
            \ '%E%f:%l:%c%\%.%\%.%\d%\+:%\d%\+: %m,' .
            \ '%E%f:%l:%c: %m,' .
            \ '%+C    %m'
    endif

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'subtype': 'Style',
        \ 'preprocess': 'killEmpty',
        \ 'postprocess': ['compressWhitespace'] })

    if s:pep257_new == 0
        " byte offsets rather than column numbers
        for e in loclist
            let e['col'] = get(e, 'col', 0) + 1
        endfor
    endif

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pep257'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
