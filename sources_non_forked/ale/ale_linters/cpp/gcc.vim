" Author: geam <mdelage@student.42.fr>
" Description: gcc linter for cpp files
"
call ale#Set('cpp_gcc_executable', 'gcc')
call ale#Set('cpp_gcc_options', '-std=c++14 -Wall')

function! ale_linters#cpp#gcc#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return '%e -S -x c++ -fsyntax-only'
    \   . ' -iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ale#Pad(l:cflags)
    \   . ale#Pad(ale#Var(a:buffer, 'cpp_gcc_options')) . ' -'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'gcc',
\   'aliases': ['g++'],
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'cpp_gcc_executable')},
\   'command_chain': [
\       {'callback': 'ale#c#GetMakeCommand', 'output_stream': 'stdout'},
\       {'callback': 'ale_linters#cpp#gcc#GetCommand'},
\   ],
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
