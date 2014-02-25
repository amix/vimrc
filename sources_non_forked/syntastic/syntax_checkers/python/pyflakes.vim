"============================================================================
"File:        pyflakes.vim
"Description: Syntax checking plugin for syntastic.vim
"Authors:     Martin Grenfell <martin.grenfell@gmail.com>
"             kstep <me@kstep.me>
"             Parantapa Bhattacharya <parantapa@gmail.com>
"
"============================================================================
if exists("g:loaded_syntastic_python_pyflakes_checker")
    finish
endif
let g:loaded_syntastic_python_pyflakes_checker=1

function! SyntaxCheckers_python_pyflakes_IsAvailable()
    return executable('pyflakes')
endfunction

function! SyntaxCheckers_python_pyflakes_GetHighlightRegex(i)
    if match(a:i['text'], 'is assigned to but never used') > -1
        \ || match(a:i['text'], 'imported but unused') > -1
        \ || match(a:i['text'], 'undefined name') > -1
        \ || match(a:i['text'], 'redefinition of') > -1
        \ || match(a:i['text'], 'referenced before assignment') > -1
        \ || match(a:i['text'], 'duplicate argument') > -1
        \ || match(a:i['text'], 'after other statements') > -1
        \ || match(a:i['text'], 'shadowed by loop variable') > -1

        " fun with Python's %r: try "..." first, then '...'
        let terms =  split(a:i['text'], '"', 1)
        if len(terms) > 2
            return terms[1]
        endif

        let terms =  split(a:i['text'], "'", 1)
        if len(terms) > 2
            return terms[1]
        endif
    endif
    return ''
endfunction

function! SyntaxCheckers_python_pyflakes_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pyflakes',
        \ 'filetype': 'python',
        \ 'subchecker': 'pyflakes' })

    let errorformat =
        \ '%E%f:%l: could not compile,'.
        \ '%-Z%p^,'.
        \ '%E%f:%l:%c: %m,'.
        \ '%E%f:%l: %m,'.
        \ '%-G%.%#'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'defaults': {'text': "Syntax error"} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pyflakes'})
