" Author: Bart Libert <bart.libert@gmail.com>
" Description: cppcheck linter for c files

call ale#Set('c_cppcheck_executable', 'cppcheck')
call ale#Set('c_cppcheck_options', '--enable=style')

function! ale_linters#c#cppcheck#GetCommand(buffer) abort
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
    \   . '%e -q --language=c '
    \   . l:compile_commands_option
    \   . ale#Var(a:buffer, 'c_cppcheck_options')
    \   . ' %t'
endfunction

call ale#linter#Define('c', {
\   'name': 'cppcheck',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'c_cppcheck_executable')},
\   'command': function('ale_linters#c#cppcheck#GetCommand'),
\   'callback': 'ale#handlers#cppcheck#HandleCppCheckFormat',
\})
