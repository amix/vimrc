" Authors: Franco Victorio <@fvictorio>, Henrique Barcelos <@hbarcelos>
" Description: Report errors in Solidity code with solhint

call ale#Set('solidity_solhint_options', '')
call ale#Set('solidity_solhint_executable', 'solhint')
call ale#Set('solidity_solhint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#solidity#solhint#Handle(buffer, lines) abort
    let l:output = []

    " Matches lines like the following:
    " contracts/Bounty.sol:14:3: Expected indentation of 4 spaces but found 2 [Error/indent]
    let l:lint_pattern = '\v^[^:]+:(\d+):(\d+): %(Parse error: )@<!\ze(.*)\s+\[(Error|Warning)\/([^\]]+)\]$'

    for l:match in ale#util#GetMatches(a:lines, l:lint_pattern)
        let l:is_error = l:match[4] is? 'error'
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \   'code': l:match[5],
        \   'type': l:is_error ? 'E' : 'W',
        \})
    endfor

    " Matches lines like the following:
    " contracts/Bounty.sol:203:4: Parse error: no viable alternative at input '_loserStakeMultiplier}' [Error]
    let l:syntax_pattern = '\v^[^:]+:(\d+):(\d+): Parse error: (.*)\s+\[Error\]$'

    for l:match in ale#util#GetMatches(a:lines, l:syntax_pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[3],
        \   'code': 'Parse error',
        \   'type': 'E',
        \})
    endfor

    return l:output
endfunction

let s:executables = [
\   'node_modules/.bin/solhint',
\   'node_modules/solhint/solhint.js',
\   'solhint',
\]
let s:sep = has('win32') ? '\' : '/'

" Given a buffer, return an appropriate working directory for solhint.
function! ale_linters#solidity#solhint#GetCwd(buffer) abort
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

function! ale_linters#solidity#solhint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'solidity_solhint', s:executables)
endfunction

call ale#linter#Define('solidity', {
\   'name': 'solhint',
\   'output_stream': 'both',
\   'executable': function('ale_linters#solidity#solhint#GetExecutable'),
\   'cwd': function('ale_linters#solidity#solhint#GetCwd'),
\   'command': {b ->
\       ale#node#Executable(b, ale_linters#solidity#solhint#GetExecutable(b))
\       . ale#Pad(ale#Var(b, 'solidity_solhint_options'))
\       . ' --formatter unix %s'
\   },
\   'callback': 'ale_linters#solidity#solhint#Handle',
\})
