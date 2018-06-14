" Author: Chris Kyrouac - https://github.com/fijshion
" Description: JSHint for Javascript files

call ale#Set('javascript_jshint_executable', 'jshint')
call ale#Set('javascript_jshint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#javascript#jshint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_jshint', [
    \   'node_modules/.bin/jshint',
    \])
endfunction

function! ale_linters#javascript#jshint#GetCommand(buffer) abort
    " Search for a local JShint config locaation, and default to a global one.
    let l:jshint_config = ale#path#ResolveLocalPath(
    \   a:buffer,
    \   '.jshintrc',
    \   get(g:, 'ale_jshint_config_loc', '')
    \)

    let l:command = ale#Escape(ale_linters#javascript#jshint#GetExecutable(a:buffer))
    let l:command .= ' --reporter unix --extract auto'

    if !empty(l:jshint_config)
        let l:command .= ' --config ' . ale#Escape(l:jshint_config)
    endif

    let l:command .= ' -'

    return l:command
endfunction

call ale#linter#Define('javascript', {
\   'name': 'jshint',
\   'executable_callback': 'ale_linters#javascript#jshint#GetExecutable',
\   'command_callback': 'ale_linters#javascript#jshint#GetCommand',
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
