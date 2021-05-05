" Author: Ben Reedy <https://github.com/breed808>
" Description: staticcheck for Go files

call ale#Set('go_staticcheck_options', '')
call ale#Set('go_staticcheck_lint_package', 0)

function! ale_linters#go#staticcheck#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'go_staticcheck_options')
    let l:lint_package = ale#Var(a:buffer, 'go_staticcheck_lint_package')
    let l:env = ale#go#EnvString(a:buffer)

    if l:lint_package
        return l:env . 'staticcheck'
        \   . (!empty(l:options) ? ' ' . l:options : '') . ' .'
    endif

    return l:env . 'staticcheck'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %s:t'
endfunction

call ale#linter#Define('go', {
\   'name': 'staticcheck',
\   'executable': 'staticcheck',
\   'cwd': '%s:h',
\   'command': function('ale_linters#go#staticcheck#GetCommand'),
\   'callback': 'ale#handlers#go#Handler',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
