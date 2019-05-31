" Author: Bart Libert <bart.libert@gmail.com>
" Description: cppcheck linter for cpp files

call ale#Set('cpp_cppcheck_executable', 'cppcheck')
call ale#Set('cpp_cppcheck_options', '--enable=style')

function! ale_linters#cpp#cppcheck#GetCommand(buffer) abort
    " Search upwards from the file for compile_commands.json.
    "
    " If we find it, we'll `cd` to where the compile_commands.json file is,
    " then use the file to set up import paths, etc.
    let [l:dir, l:json_path] = ale#c#FindCompileCommands(a:buffer)
    let l:cd_command = !empty(l:dir) ? ale#path#CdString(l:dir) : ''
    let l:compile_commands_option = !empty(l:json_path)
    \   ? '--project=' . ale#Escape(l:json_path[len(l:dir) + 1: ])
    \   : ''

    return l:cd_command
    \   . '%e -q --language=c++'
    \   . ale#Pad(l:compile_commands_option)
    \   . ale#Pad(ale#Var(a:buffer, 'cpp_cppcheck_options'))
    \   . ' %t'
endfunction

call ale#linter#Define('cpp', {
\   'name': 'cppcheck',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'cpp_cppcheck_executable')},
\   'command': function('ale_linters#cpp#cppcheck#GetCommand'),
\   'callback': 'ale#handlers#cppcheck#HandleCppCheckFormat',
\})
