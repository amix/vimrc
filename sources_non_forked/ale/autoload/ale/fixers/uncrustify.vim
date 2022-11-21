" Author: Derek P Sifford <dereksifford@gmail.com>
" Description: Fixer for C, C++, C#, ObjectiveC, D, Java, Pawn, and VALA.

call ale#Set('c_uncrustify_executable', 'uncrustify')
call ale#Set('c_uncrustify_options', '')

let s:languages = {
\   'c': 'C',
\   'cpp': 'CPP',
\   'cs': 'CS',
\   'objc': 'OC',
\   'objcpp': 'OC+',
\   'd': 'D',
\   'java': 'JAVA',
\   'vala': 'VALA',
\   'p': 'PAWN',
\}

function! ale#fixers#uncrustify#Language(buffer) abort
    return get(s:languages, &filetype, 'C')
endfunction

function! ale#fixers#uncrustify#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'c_uncrustify_executable')
    let l:options = ale#Var(a:buffer, 'c_uncrustify_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' --no-backup '
    \       . '-l' . ale#Pad(ale#fixers#uncrustify#Language(a:buffer))
    \       . ale#Pad(l:options)
    \}
endfunction
