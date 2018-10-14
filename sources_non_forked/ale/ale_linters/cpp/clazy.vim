" Description: clazy linter for cpp files (clang-based and Qt-oriented)

call ale#Set('cpp_clazy_executable', 'clazy-standalone')
" Set this option to check the checks clazy will apply.
call ale#Set('cpp_clazy_checks', ['level1'])
" Set this option to manually set some options for clazy.
" This will disable compile_commands.json detection.
call ale#Set('cpp_clazy_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#cpp#clazy#GetCommand(buffer) abort
    let l:checks = join(ale#Var(a:buffer, 'cpp_clazy_checks'), ',')
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)

    " Get the extra options if we couldn't find a build directory.
    let l:options = ale#Var(a:buffer, 'cpp_clazy_options')

    return '%e'
    \   . (!empty(l:checks) ? ' -checks=' . ale#Escape(l:checks) : '')
    \   . (!empty(l:build_dir) ? ' -p ' . ale#Escape(l:build_dir) : '')
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %s'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'clazy',
\   'output_stream': 'stderr',
\   'executable_callback': ale#VarFunc('cpp_clazy_executable'),
\   'command_callback': 'ale_linters#cpp#clazy#GetCommand',
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\   'lint_file': 1,
\})
