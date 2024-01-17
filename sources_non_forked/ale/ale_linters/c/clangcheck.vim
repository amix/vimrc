" Author: gagbo <gagbobada@gmail.com>
"       : luibo <ng.akhoa98@gmail.com>
"       : Jorengarenar <jorengarenar@outlook.com>
" Description: clang-check linter for C files
"              modified from cpp/clangcheck.vim to match for C

call ale#Set('c_clangcheck_executable', 'clang-check')
call ale#Set('c_clangcheck_options', '')
call ale#Set('c_build_dir', '')

function! ale_linters#c#clangcheck#GetCommand(buffer) abort
    let l:user_options = ale#Var(a:buffer, 'c_clangcheck_options')

    " Try to find compilation database to link automatically
    let l:build_dir = ale#Var(a:buffer, 'c_build_dir')

    if empty(l:build_dir)
        let [l:root, l:json_file] = ale#c#FindCompileCommands(a:buffer)
        let l:build_dir = ale#path#Dirname(l:json_file)
    endif

    " The extra arguments in the command are used to prevent .plist files from
    " being generated. These are only added if no build directory can be
    " detected.
    return '%e -analyze %s'
    \   . (empty(l:build_dir) ? ' --extra-arg=-Xclang --extra-arg=-analyzer-output=text --extra-arg=-fno-color-diagnostics': '')
    \   . ale#Pad(l:user_options)
    \   . (!empty(l:build_dir) ? ' -p ' . ale#Escape(l:build_dir) : '')
endfunction

call ale#linter#Define('c', {
\   'name': 'clangcheck',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'c_clangcheck_executable')},
\   'command': function('ale_linters#c#clangcheck#GetCommand'),
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\   'lint_file': 1,
\})
