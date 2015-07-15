"============================================================================
"File:        pyflakes.vim
"Description: Syntax checking plugin for syntastic.vim
"Authors:     Martin Grenfell <martin.grenfell@gmail.com>
"             kstep <me@kstep.me>
"             Parantapa Bhattacharya <parantapa@gmail.com>
"
"============================================================================

if exists('g:loaded_syntastic_python_pyflakes_checker')
    finish
endif
let g:loaded_syntastic_python_pyflakes_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_pyflakes_GetHighlightRegex(i)
    if stridx(a:i['text'], 'is assigned to but never used') >= 0
        \ || stridx(a:i['text'], 'imported but unused') >= 0
        \ || stridx(a:i['text'], 'undefined name') >= 0
        \ || stridx(a:i['text'], 'redefinition of') >= 0
        \ || stridx(a:i['text'], 'referenced before assignment') >= 0
        \ || stridx(a:i['text'], 'duplicate argument') >= 0
        \ || stridx(a:i['text'], 'after other statements') >= 0
        \ || stridx(a:i['text'], 'shadowed by loop variable') >= 0

        " fun with Python's %r: try "..." first, then '...'
        let term = matchstr(a:i['text'], '\m^.\{-}"\zs.\{-1,}\ze"')
        if term !=# ''
            return '\V\<' . escape(term, '\') . '\>'
        endif

        let term = matchstr(a:i['text'], '\m^.\{-}''\zs.\{-1,}\ze''')
        if term !=# ''
            return '\V\<' . escape(term, '\') . '\>'
        endif
    endif
    return ''
endfunction

function! SyntaxCheckers_python_pyflakes_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%E%f:%l: could not compile,'.
        \ '%-Z%p^,'.
        \ '%E%f:%l:%c: %m,'.
        \ '%E%f:%l: %m,'.
        \ '%-G%.%#'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env,
        \ 'defaults': {'text': 'Syntax error'} })

    for e in loclist
        let e['vcol'] = 0
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pyflakes'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
