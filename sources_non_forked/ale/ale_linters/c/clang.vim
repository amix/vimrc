" Author: Masahiro H https://github.com/mshr-h
" Description: clang linter for c files

call ale#Set('c_clang_executable', 'clang')
call ale#Set('c_clang_options', '-std=c11 -Wall')

function! ale_linters#c#clang#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return '%e -S -x c -fsyntax-only'
    \   . ' -iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ale#Pad(l:cflags)
    \   . ale#Pad(ale#Var(a:buffer, 'c_clang_options')) . ' -'
endfunction

call ale#linter#Define('c', {
\   'name': 'clang',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'c_clang_executable')},
\   'command': {b -> ale#c#RunMakeCommand(b, function('ale_linters#c#clang#GetCommand'))},
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
