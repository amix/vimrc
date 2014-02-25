"============================================================================
"File:        pylint.vim
"Description: Syntax checking plugin for syntastic.vim
"Author:      Parantapa Bhattacharya <parantapa at gmail dot com>
"
"============================================================================
if exists("g:loaded_syntastic_python_pylint_checker")
    finish
endif
let g:loaded_syntastic_python_pylint_checker=1

function! SyntaxCheckers_python_pylint_IsAvailable()
    return executable('pylint')
endfunction

function! SyntaxCheckers_python_pylint_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pylint',
        \ 'args': ' -f parseable -r n -i y',
        \ 'filetype': 'python',
        \ 'subchecker': 'pylint' })

    let errorformat =
        \ '%A%f:%l:%m,' .
        \ '%A%f:(%l):%m,' .
        \ '%-Z%p^%.%#,' .
        \ '%-G%.%#'

    let loclist=SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['sort'] })

    for n in range(len(loclist))
        let loclist[n]['type'] = match(['R', 'C', 'W'], loclist[n]['text'][2]) >= 0 ? 'W' : 'E'
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylint' })
