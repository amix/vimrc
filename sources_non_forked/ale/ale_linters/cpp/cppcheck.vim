" Author: Bart Libert <bart.libert@gmail.com>
" Description: cppcheck linter for cpp files

call ale#Set('cpp_cppcheck_executable', 'cppcheck')
call ale#Set('cpp_cppcheck_options', '--enable=style')

function! ale_linters#cpp#cppcheck#GetCommand(buffer) abort
    let l:cd_command = ale#handlers#cppcheck#GetCdCommand(a:buffer)
    let l:compile_commands_option = ale#handlers#cppcheck#GetCompileCommandsOptions(a:buffer)
    let l:buffer_path_include = empty(l:compile_commands_option)
    \   ? ale#handlers#cppcheck#GetBufferPathIncludeOptions(a:buffer)
    \   : ''

    return l:cd_command
    \   . '%e -q --language=c++'
    \   . ale#Pad(l:compile_commands_option)
    \   . ale#Pad(ale#Var(a:buffer, 'cpp_cppcheck_options'))
    \   . l:buffer_path_include
    \   . ' %t'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'cppcheck',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'cpp_cppcheck_executable')},
\   'command': function('ale_linters#cpp#cppcheck#GetCommand'),
\   'callback': 'ale#handlers#cppcheck#HandleCppCheckFormat',
\})
