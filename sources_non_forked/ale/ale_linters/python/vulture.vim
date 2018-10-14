" Author: Yauheni Kirylau <actionless.loveless@gmail.com>
" Description: vulture linting for python files

call ale#Set('python_vulture_executable', 'vulture')
call ale#Set('python_vulture_options', '')
call ale#Set('python_vulture_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('python_vulture_change_directory', 1)


" The directory to change to before running vulture
function! s:GetDir(buffer) abort
    let l:project_root = ale#python#FindProjectRoot(a:buffer)

    return !empty(l:project_root)
    \   ? l:project_root
    \   : expand('#' . a:buffer . ':p:h')
endfunction


function! ale_linters#python#vulture#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'python_vulture', ['vulture'])
endfunction


function! ale_linters#python#vulture#GetCommand(buffer) abort
    let l:change_dir = ale#Var(a:buffer, 'python_vulture_change_directory')
    \   ? ale#path#CdString(s:GetDir(a:buffer))
    \   : ''

    let l:executable = ale_linters#python#vulture#GetExecutable(a:buffer)

    let l:exec_args = l:executable =~? 'pipenv$'
    \   ? ' run vulture'
    \   : ''

    let l:lint_dest = ale#Var(a:buffer, 'python_vulture_change_directory')
    \   ? ' .'
    \   : ' %s'

    return l:change_dir
    \   . ale#Escape(l:executable) . l:exec_args
    \   . ' '
    \   . ale#Var(a:buffer, 'python_vulture_options')
    \   . l:lint_dest
endfunction


function! ale_linters#python#vulture#Handle(buffer, lines) abort
    for l:line in a:lines[:10]
        if match(l:line, '^Traceback') >= 0
            return [{
            \   'lnum': 1,
            \   'text': 'An exception was thrown. See :ALEDetail',
            \   'detail': join(a:lines, "\n"),
            \}]
        endif
    endfor

    " Matches patterns line the following:
    let l:pattern = '\v^([a-zA-Z]?:?[^:]+):(\d+): (.*)$'
    let l:output = []
    let l:dir = s:GetDir(a:buffer)

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:abspath = ale#path#GetAbsPath(l:dir, l:match[1])
        let l:item = {
        \   'filename': l:abspath,
        \   'lnum': l:match[2] + 0,
        \   'text': l:match[3],
        \   'type': 'W',
        \}
        call add(l:output, l:item)
    endfor

    return l:output
endfunction


call ale#linter#Define('python', {
\   'name': 'vulture',
\   'executable_callback': 'ale_linters#python#vulture#GetExecutable',
\   'command_callback': 'ale_linters#python#vulture#GetCommand',
\   'callback': 'ale_linters#python#vulture#Handle',
\   'lint_file': 1,
\})
