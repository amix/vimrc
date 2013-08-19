"============================================================================
"File:        pep257.vim
"Description: Docstring style checking plugin for syntastic.vim
"============================================================================
"
" For details about pep257 see: https://github.com/GreenSteam/pep257

if exists("g:loaded_syntastic_python_pep257_checker")
    finish
endif
let g:loaded_syntastic_python_pep257_checker = 1

function! SyntaxCheckers_python_pep257_IsAvailable()
    return executable('pep257')
endfunction

" sanity: kill empty lines here rather than munging errorformat
function! SyntaxCheckers_python_pep257_Preprocess(errors)
    return filter(copy(a:errors), 'v:val != ""')
endfunction

function! SyntaxCheckers_python_pep257_GetLocList()
    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pep257',
        \ 'filetype': 'python',
        \ 'subchecker': 'pep257' })

    let errorformat =
        \ '%E%f:%l:%c%\%.%\%.%\d%\+:%\d%\+: %m,' .
        \ '%E%f:%l:%c: %m,' .
        \ '%+C    %m'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style',
        \ 'preprocess': 'SyntaxCheckers_python_pep257_Preprocess',
        \ 'postprocess': ['compressWhitespace'] })

    " pep257 outputs byte offsets rather than column numbers
    for n in range(len(loclist))
        let loclist[n]['col'] = get(loclist[n], 'col', 0) + 1
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pep257'})
