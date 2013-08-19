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
    let pylint_new = s:PylintNew()

    let makeprg = syntastic#makeprg#build({
        \ 'exe': 'pylint',
        \ 'args': (pylint_new ? '--msg-template="{path}:{line}: [{msg_id}] {msg}" -r n' : '-f parseable -r n -i y'),
        \ 'filetype': 'python',
        \ 'subchecker': 'pylint' })

    let errorformat =
        \ '%A%f:%l: %m,' .
        \ '%A%f:(%l): %m,' .
        \ '%-Z%p^%.%#,' .
        \ '%-G%.%#'

    let loclist=SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['sort'] })

    for n in range(len(loclist))
        let type = loclist[n]['text'][1]
        if type =~# '\m^[EF]'
            let loclist[n]['type'] = 'E'
        elseif type =~# '\m^[CRW]'
            let loclist[n]['type'] = 'W'
        else
            let loclist[n]['valid'] = 0
        endif
        let loclist[n]['vcol'] = 0
    endfor

    return loclist
endfunction

function s:PylintNew()
    let pylint_version = filter(split(system('pylint --version'), '\m, \|\n'), 'v:val =~# "^pylint"')[0]
    return syntastic#util#versionIsAtLeast(syntastic#util#parseVersion(pylint_version), [1])
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'pylint' })
