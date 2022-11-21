" Author: w0rp <devw0rp@gmail.com>
" Description: A C compiler linter for C files with gcc/clang, etc.

call ale#Set('c_cc_executable', '<auto>')
call ale#Set('c_cc_options', '-std=c11 -Wall')
call ale#Set('c_cc_use_header_lang_flag', -1)
call ale#Set('c_cc_header_exts', ['h'])

function! ale_linters#c#cc#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'c_cc_executable')

    " Default to either clang or gcc.
    if l:executable is# '<auto>'
        if ale#engine#IsExecutable(a:buffer, 'clang')
            let l:executable = 'clang'
        else
            let l:executable = 'gcc'
        endif
    endif

    return l:executable
endfunction

function! ale_linters#c#cc#GetCommand(buffer, output) abort
    let l:cflags = ale#c#GetCFlags(a:buffer, a:output)
    let l:ale_flags = ale#Var(a:buffer, 'c_cc_options')

    if l:cflags =~# '-std='
        let l:ale_flags = substitute(
        \   l:ale_flags,
        \   '-std=\(c\|gnu\)[0-9]\{2\}',
        \   '',
        \   'g')
    endif

    " Select the correct language flag depending on the executable, options
    " and file extension
    let l:executable = ale_linters#c#cc#GetExecutable(a:buffer)
    let l:use_header_lang_flag = ale#Var(a:buffer, 'c_cc_use_header_lang_flag')
    let l:header_exts = ale#Var(a:buffer, 'c_cc_header_exts')
    let l:lang_flag = ale#c#GetLanguageFlag(
    \   a:buffer,
    \   l:executable,
    \   l:use_header_lang_flag,
    \   l:header_exts,
    \   'c')

    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    "
    " `-o /dev/null` or `-o null` is needed to catch all errors,
    " -fsyntax-only doesn't catch everything.
    return '%e -S -x ' . l:lang_flag
    \   . ' -o ' . g:ale#util#nul_file
    \   . ' -iquote %s:h'
    \   . ale#Pad(l:cflags)
    \   . ale#Pad(l:ale_flags) . ' -'
endfunction

call ale#linter#Define('c', {
\   'name': 'cc',
\   'aliases': ['gcc', 'clang'],
\   'output_stream': 'stderr',
\   'executable': function('ale_linters#c#cc#GetExecutable'),
\   'command': {b -> ale#c#RunMakeCommand(b, function('ale_linters#c#cc#GetCommand'))},
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
