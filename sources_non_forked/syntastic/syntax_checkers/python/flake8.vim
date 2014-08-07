"============================================================================
"File:        flake8.vim
"Description: Syntax checking plugin for syntastic.vim
"Authors:     Sylvain Soliman <Sylvain dot Soliman+git at gmail dot com>
"             kstep <me@kstep.me>
"
"============================================================================

if exists("g:loaded_syntastic_python_flake8_checker")
    finish
endif
let g:loaded_syntastic_python_flake8_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_python_flake8_GetHighlightRegex(item)
    return SyntaxCheckers_python_pyflakes_GetHighlightRegex(a:item)
endfunction

function! SyntaxCheckers_python_flake8_GetLocList() dict
    let makeprg = self.makeprgBuild({})

    let errorformat =
        \ '%E%f:%l: could not compile,%-Z%p^,' .
        \ '%A%f:%l:%c: %t%n %m,' .
        \ '%A%f:%l: %t%n %m,' .
        \ '%-G%.%#'

    let env = syntastic#util#isRunningWindows() ? {} : { 'TERM': 'dumb' }

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'env': env })

    for e in loclist
        " E*** and W*** are pep8 errors
        " F*** are PyFlakes codes
        " C*** are McCabe complexity messages
        " N*** are naming conventions from pep8-naming

        if has_key(e, 'nr')
            let e['text'] .= printf(' [%s%03d]', e['type'], e['nr'])
            " E901 are syntax errors
            " E902 are I/O errors
            if e['type'] ==? 'E' && e['nr'] !~ '\m^9'
                let e['subtype'] = 'Style'
            endif
            call remove(e, 'nr')
        endif

        if e['type'] =~? '\m^[CNW]'
            let e['subtype'] = 'Style'
        endif

        let e['type'] = e['type'] =~? '\m^[EFC]' ? 'E' : 'W'
    endfor

    return loclist
endfunction

runtime! syntax_checkers/python/pyflakes.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'python',
    \ 'name': 'flake8'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
