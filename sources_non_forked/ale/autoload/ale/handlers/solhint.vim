" Author: Henrique Barcelos <@hbarcelos>
" Description: Functions for working with local solhint for checking *.sol files.

let s:executables = [
\   'node_modules/.bin/solhint',
\   'node_modules/solhint/solhint.js',
\   'solhint',
\]

let s:sep = has('win32') ? '\' : '/'

call ale#Set('solidity_solhint_options', '')
call ale#Set('solidity_solhint_executable', 'solhint')
call ale#Set('solidity_solhint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale#handlers#solhint#Handle(buffer, lines) abort
    " Matches patterns like the following:
    " /path/to/file/file.sol: line 1, col 10, Error - 'addOne' is defined but never used. (no-unused-vars)
    let l:output = []

    let l:lint_pattern = '\v^[^:]+: line (\d+), col (\d+), (Error|Warning) - (.*) \((.*)\)$'

    for l:match in ale#util#GetMatches(a:lines, l:lint_pattern)
        let l:isError = l:match[3] is? 'error'
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'code': l:match[5],
        \   'type': l:isError ? 'E' : 'W',
        \})
    endfor

    let l:syntax_pattern = '\v^[^:]+: line (\d+), col (\d+), (Error|Warning) - (Parse error): (.*)$'

    for l:match in ale#util#GetMatches(a:lines, l:syntax_pattern)
        let l:isError = l:match[3] is? 'error'
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[5],
        \   'code': l:match[4],
        \   'type': l:isError ? 'E' : 'W',
        \})
    endfor

    return l:output
endfunction

function! ale#handlers#solhint#FindConfig(buffer) abort
    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        for l:basename in [
        \   '.solhintrc.js',
        \   '.solhintrc.json',
        \   '.solhintrc',
        \]
            let l:config = ale#path#Simplify(join([l:path, l:basename], s:sep))

            if filereadable(l:config)
                return l:config
            endif
        endfor
    endfor

    return ale#path#FindNearestFile(a:buffer, 'package.json')
endfunction

function! ale#handlers#solhint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'solidity_solhint', s:executables)
endfunction

" Given a buffer, return an appropriate working directory for solhint.
function! ale#handlers#solhint#GetCwd(buffer) abort
    " If solhint is installed in a directory which contains the buffer, assume
    " it is the solhint project root. Otherwise, use nearest node_modules.
    " Note: If node_modules not present yet, can't load local deps anyway.
    let l:executable = ale#path#FindNearestExecutable(a:buffer, s:executables)

    if !empty(l:executable)
        let l:nmi = strridx(l:executable, 'node_modules')
        let l:project_dir = l:executable[0:l:nmi - 2]
    else
        let l:modules_dir = ale#path#FindNearestDirectory(a:buffer, 'node_modules')
        let l:project_dir = !empty(l:modules_dir) ? fnamemodify(l:modules_dir, ':h:h') : ''
    endif

    return !empty(l:project_dir) ? l:project_dir : ''
endfunction

function! ale#handlers#solhint#GetCommand(buffer) abort
    let l:executable = ale#handlers#solhint#GetExecutable(a:buffer)

    let l:options = ale#Var(a:buffer, 'solidity_solhint_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --formatter compact %s'
endfunction
