" Author: w0rp <devw0rp@gmail.com>
" Description: gcc linter for c files

call ale#Set('c_gcc_executable', 'gcc')
call ale#Set('c_gcc_options', '-std=c11 -Wall')

function! ale_linters#c#gcc#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return '%e -S -x c -fsyntax-only'
    \   . ' -iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ale#Pad(l:cflags)
    \   . ale#Pad(ale#Var(a:buffer, 'c_gcc_options')) . ' -'
endfunction

call ale#linter#Define('c', {
\   'name': 'gcc',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'c_gcc_executable')},
\   'command': {b -> ale#c#RunMakeCommand(b, function('ale_linters#c#gcc#GetCommand'))},
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
