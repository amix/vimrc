" Author: Chris Kyrouac - https://github.com/fijshion
" Description: jscs for JavaScript files

call ale#Set('javascript_jscs_executable', 'jscs')
call ale#Set('javascript_jscs_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#javascript#jscs#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'javascript_jscs', [
    \   'node_modules/.bin/jscs',
    \])
endfunction

function! ale_linters#javascript#jscs#GetCommand(buffer) abort
    " Search for a local JShint config locaation, and default to a global one.
    let l:jscs_config = ale#path#ResolveLocalPath(
    \   a:buffer,
    \   '.jscsrc',
    \   get(g:, 'ale_jscs_config_loc', '')
    \)

    let l:command = ale#Escape(ale_linters#javascript#jscs#GetExecutable(a:buffer))
    let l:command .= ' --reporter inline --no-colors'

    if !empty(l:jscs_config)
        let l:command .= ' --config ' . ale#Escape(l:jscs_config)
    endif

    let l:command .= ' -'

    return l:command
endfunction

function! ale_linters#javascript#jscs#Handle(buffer, lines) abort
    " Matches patterns looking like the following
    "
    " foobar.js: line 2, col 1, Expected indentation of 1 characters
    "
    let l:pattern = '\v^.*:\s+line (\d+),\s+col\s+(\d+),\s+(.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:obj = {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3]
        \}

        let l:code_match = matchlist(l:match[3], '\v([^ :]+): (.+)$')

        if !empty(l:code_match)
            let l:obj.code = l:code_match[1]
            let l:obj.text = l:code_match[2]
        endif

        call add(l:output, l:obj)
    endfor

    return l:output
endfunction

call ale#linter#Define('javascript', {
\   'name': 'jscs',
\   'executable_callback': 'ale_linters#javascript#jscs#GetExecutable',
\   'command_callback': 'ale_linters#javascript#jscs#GetCommand',
\   'callback': 'ale_linters#javascript#jscs#Handle',
\})

