" Author: w0rp <devw0rp@gmail.com>
" Description: A C++ compiler linter for C++ files with gcc/clang, etc.

call ale#Set('cpp_cc_executable', '<auto>')
call ale#Set('cpp_cc_options', '-std=c++14 -Wall')

function! ale_linters#cpp#cc#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'cpp_cc_executable')

    " Default to either clang++ or gcc.
    if l:executable is# '<auto>'
        if ale#engine#IsExecutable(a:buffer, 'clang++')
            let l:executable = 'clang++'
        else
            let l:executable = 'gcc'
        endif
    endif

    return l:executable
endfunction

function! ale_linters#cpp#cc#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)
    let l:ale_flags = ale#Var(a:buffer, 'cpp_cc_options')

    if l:cflags =~# '-std='
        let l:ale_flags = substitute(
        \   l:ale_flags,
        \   '-std=\(c\|gnu\)++[0-9]\{2\}',
        \   '',
        \   'g')
    endif

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    "
    " `-o /dev/null` or `-o null` is needed to catch all errors,
    " -fsyntax-only doesn't catch everything.
    return '%e -S -x c++'
    \   . ' -o ' . g:ale#util#nul_file
    \   . ' -iquote %s:h'
    \   . ale#Pad(l:cflags)
    \   . ale#Pad(l:ale_flags) . ' -'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'cc',
\   'aliases': ['gcc', 'clang', 'g++', 'clang++'],
\   'output_stream': 'stderr',
\   'executable': function('ale_linters#cpp#cc#GetExecutable'),
\   'command': {b -> ale#c#RunMakeCommand(b, function('ale_linters#cpp#cc#GetCommand'))},
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
