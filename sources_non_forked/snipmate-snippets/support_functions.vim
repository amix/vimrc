"ruby {{{1
function! Snippet_RubyClassNameFromFilename(...)
    let name = expand("%:t:r")
    if len(name) == 0
        if a:0 == 0
            let name = 'MyClass'
        else
            let name = a:1
        endif
    endif
    return Snippet_Camelcase(substitute(name, '_spec$', '', ''))
endfunction

function! Snippet_MigrationNameFromFilename(...)
    let name = substitute(expand("%:t:r"), '^.\{-}_', '', '')
    if len(name) == 0
        if a:0 == 0
            let name = 'MyClass'
        else
            let name = a:1
        endif
    endif
    return Snippet_Camelcase(name)
endfunction


"python {{{1
function! Snippet_PythonClassNameFromFilename(...)
    let name = expand("%:t:r")
    if len(name) == 0
        if a:0 == 0
            let name = 'MyClass'
        else
            let name = a:1
        endif
    endif
    return Snippet_Camelcase(name)
endfunction

"php {{{1
function! Snippet_PHPClassNameFromFilename(...)
    let name = expand("%:t:r:r")
    if len(name) == 0
        if a:0 == 0
            let name = 'MyClass'
        else
            let name = a:1
        endif
    endif
    return name
endfunction

"java {{{1
function! Snippet_JavaClassNameFromFilename(...)
    let name = expand("%:t:r")
    if len(name) == 0
        if a:0 == 0
            let name = 'MyClass'
        else
            let name = a:1
        endif
    endif
    return name
endfunction

function! Snippet_JavaInstanceVarType(name)
    let oldview = winsaveview()
    if searchdecl(a:name) == 0
        normal! B
        let old_reg = @"
        normal! yaW
        let type = @"
        let @" = old_reg
        call winrestview(oldview)
        let type = substitute(type, '\s\+$', '', '')

        "searchdecl treats  'return foo;' as a declaration of foo
        if type != 'return'
            return type
        endif
    endif
    return "<+type+>"
endfunction


"global {{{1
function! s:start_comment()
    return substitute(&commentstring, '^\([^ ]*\)\s*%s\(.*\)$', '\1', '')
endfunction

function! s:end_comment()
    return substitute(&commentstring, '^.*%s\(.*\)$', '\1', '')
endfunction

function! Snippet_Modeline()
    return s:start_comment() . " vim: set ${1:settings}:" . s:end_comment()
endfunction

function! Snippet_Camelcase(s)
    "upcase the first letter
    let toReturn = substitute(a:s, '^\(.\)', '\=toupper(submatch(1))', '')
    "turn all '_x' into 'X'
    return substitute(toReturn, '_\(.\)', '\=toupper(submatch(1))', 'g')
endfunction

function! Snippet_Underscore(s)
    "down the first letter
    let toReturn = substitute(a:s, '^\(.\)', '\=tolower(submatch(1))', '')
    "turn all 'X' into '_x'
    return substitute(toReturn, '\([A-Z]\)', '\=tolower("_".submatch(1))', 'g')
endfunction


" modeline {{{1
" vim: set fdm=marker:
