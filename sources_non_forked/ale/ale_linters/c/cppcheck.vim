" Author: Bart Libert <bart.libert@gmail.com>
" Description: cppcheck linter for c files

call ale#Set('c_cppcheck_executable', 'cppcheck')
call ale#Set('c_cppcheck_options', '--enable=style')

function! ale_linters#c#cppcheck#GetCommand(buffer) abort
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
    \   . '%e -q --language=c'
    \   . ale#Pad(l:compile_commands_option)
    \   . ale#Pad(ale#Var(a:buffer, 'c_cppcheck_options'))
    \   . ' %t'
endfunction

call ale#linter#Define('c', {
\   'name': 'cppcheck',
\   'output_stream': 'both',
\   'executable': {b -> ale#Var(b, 'c_cppcheck_executable')},
\   'command': function('ale_linters#c#cppcheck#GetCommand'),
\   'callback': 'ale#handlers#cppcheck#HandleCppCheckFormat',
\})
