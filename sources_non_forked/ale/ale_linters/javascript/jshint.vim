" Author: Chris Kyrouac - https://github.com/fijshion
" Description: JSHint for Javascript files

call ale#Set('javascript_jshint_executable', 'jshint')
call ale#Set('javascript_jshint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#javascript#jshint#GetCommand(buffer) abort
    " Search for a local JShint config locaation, and default to a global one.
    let l:jshint_config = ale#path#ResolveLocalPath(
    \   a:buffer,
    \   '.jshintrc',
    \   get(g:, 'ale_jshint_config_loc', '')
    \)

    let l:command = '%e --reporter unix --extract auto'

    if !empty(l:jshint_config)
        let l:command .= ' --config ' . ale#Escape(l:jshint_config)
    endif

    let l:command .= ' --filename %s -'

    return l:command
endfunction

call ale#linter#Define('javascript', {
\   'name': 'jshint',
\   'executable': {b -> ale#node#FindExecutable(b, 'javascript_jshint', [
\       'node_modules/.bin/jshint',
\   ])},
\   'command': function('ale_linters#javascript#jshint#GetCommand'),
\   'callback': 'ale#handlers#unix#HandleAsError',
\})
