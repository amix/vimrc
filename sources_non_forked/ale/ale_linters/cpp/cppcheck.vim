" Author: Bart Libert <bart.libert@gmail.com>
" Description: cppcheck linter for cpp files

call ale#Set('cpp_cppcheck_executable', 'cppcheck')
call ale#Set('cpp_cppcheck_options', '--enable=style')

function! ale_linters#cpp#cppcheck#GetCommand(buffer) abort
    " Search upwards from the file for compile_commands.json.
    "
    " If we find it, we'll `cd` to where the compile_commands.json file is,
    " then use the file to set up import paths, etc.
    let l:compile_commmands_path = ale#path#FindNearestFile(a:buffer, 'compile_commands.json')

    let l:cd_command = !empty(l:compile_commmands_path)
    \   ? ale#path#CdString(fnamemodify(l:compile_commmands_path, ':h'))
    \   : ''
    let l:compile_commands_option = !empty(l:compile_commmands_path)
    \   ? '--project=compile_commands.json '
    \   : ''

    return l:cd_command
    \   . '%e -q --language=c++ '
    \   . l:compile_commands_option
    \   . ale#Var(a:buffer, 'cpp_cppcheck_options')
    \   . ' %t'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'cppcheck',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'cpp_cppcheck_executable')},
\   'command': function('ale_linters#cpp#cppcheck#GetCommand'),
\   'callback': 'ale#handlers#cppcheck#HandleCppCheckFormat',
\})
