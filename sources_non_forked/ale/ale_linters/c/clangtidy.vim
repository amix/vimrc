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

call ale#Set('c_clangtidy_checks', [])
" Set this option to manually set some options for clang-tidy.
" This will disable compile_commands.json detection.
call ale#Set('c_clangtidy_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#c#clangtidy#GetCommand(buffer) abort
    let l:checks = join(ale#Var(a:buffer, 'c_clangtidy_checks'), ',')
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)

    " Get the extra options if we couldn't find a build directory.
    let l:options = empty(l:build_dir)
    \   ? ale#Var(a:buffer, 'c_clangtidy_options')
    \   : ''

    return '%e'
    \   . (!empty(l:checks) ? ' -checks=' . ale#Escape(l:checks) : '')
    \   . ' %s'
    \   . (!empty(l:build_dir) ? ' -p ' . ale#Escape(l:build_dir) : '')
    \   . (!empty(l:options) ? ' -- ' . l:options : '')
endfunction

call ale#linter#Define('c', {
\   'name': 'clangtidy',
\   'output_stream': 'stdout',
\   'executable': {b -> ale#Var(b, 'c_clangtidy_executable')},
\   'command': function('ale_linters#c#clangtidy#GetCommand'),
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\   'lint_file': 1,
\})
