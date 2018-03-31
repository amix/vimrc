" Author: vdeurzen <tim@kompiler.org>, w0rp <devw0rp@gmail.com>,
" gagbo <gagbobada@gmail.com>, Andrej Radovic <r.andrej@gmail.com>
" Description: clang-tidy linter for c files

call ale#Set('c_clangtidy_executable', 'clang-tidy')
" Set this option to check the checks clang-tidy will apply.
" The number of checks that can be applied to C files is limited in contrast to
" C++
"
" Consult the check list in clang-tidy's documentation:
" http://clang.llvm.org/extra/clang-tidy/checks/list.html

call ale#Set('c_clangtidy_checks', ['*'])
" Set this option to manually set some options for clang-tidy.
" This will disable compile_commands.json detection.
call ale#Set('c_clangtidy_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#c#clangtidy#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'c_clangtidy_executable')
endfunction

function! s:GetBuildDirectory(buffer) abort
    " Don't include build directory for header files, as compile_commands.json
    " files don't consider headers to be translation units, and provide no
    " commands for compiling header files.
    if expand('#' . a:buffer) =~# '\v\.(h|hpp)$'
        return ''
    endif

    let l:build_dir = ale#Var(a:buffer, 'c_build_dir')

    " c_build_dir has the priority if defined
    if !empty(l:build_dir)
        return l:build_dir
    endif

    return ale#c#FindCompileCommands(a:buffer)
endfunction

function! ale_linters#c#clangtidy#GetCommand(buffer) abort
    let l:checks = join(ale#Var(a:buffer, 'c_clangtidy_checks'), ',')
    let l:build_dir = s:GetBuildDirectory(a:buffer)

    " Get the extra options if we couldn't find a build directory.
    let l:options = empty(l:build_dir)
    \   ? ale#Var(a:buffer, 'c_clangtidy_options')
    \   : ''

    return ale#Escape(ale_linters#c#clangtidy#GetExecutable(a:buffer))
    \   . (!empty(l:checks) ? ' -checks=' . ale#Escape(l:checks) : '')
    \   . ' %s'
    \   . (!empty(l:build_dir) ? ' -p ' . ale#Escape(l:build_dir) : '')
    \   . (!empty(l:options) ? ' -- ' . l:options : '')
endfunction

call ale#linter#Define('c', {
\   'name': 'clangtidy',
\   'output_stream': 'stdout',
\   'executable_callback': 'ale_linters#c#clangtidy#GetExecutable',
\   'command_callback': 'ale_linters#c#clangtidy#GetCommand',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\   'lint_file': 1,
\})
