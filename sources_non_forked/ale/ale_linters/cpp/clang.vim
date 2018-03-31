" Author: Tomota Nakamura <https://github.com/tomotanakamura>
" Description: clang linter for cpp files

call ale#Set('cpp_clang_executable', 'clang++')
call ale#Set('cpp_clang_options', '-std=c++14 -Wall')

function! ale_linters#cpp#clang#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'cpp_clang_executable')
endfunction

function! ale_linters#cpp#clang#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return ale#Escape(ale_linters#cpp#clang#GetExecutable(a:buffer))
    \   . ' -S -x c++ -fsyntax-only '
    \   . '-iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h')) . ' '
    \   . l:cflags
    \   . ale#Var(a:buffer, 'cpp_clang_options') . ' -'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'clang',
\   'output_stream': 'stderr',
\   'executable_callback': 'ale_linters#cpp#clang#GetExecutable',
\   'command_chain': [
\       {'callback': 'ale#c#GetMakeCommand', 'output_stream': 'stdout'},
\       {'callback': 'ale_linters#cpp#clang#GetCommand'},
\   ],
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\})
