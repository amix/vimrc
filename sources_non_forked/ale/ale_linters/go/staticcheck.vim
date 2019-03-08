" Author: Ben Reedy <https://github.com/breed808>
" Description: staticcheck for Go files

call ale#Set('go_staticcheck_options', '')
call ale#Set('go_staticcheck_lint_package', 0)

function! ale_linters#go#staticcheck#GetCommand(buffer) abort
    let l:filename = expand('#' . a:buffer . ':t')
    let l:options = ale#Var(a:buffer, 'go_staticcheck_options')
    let l:lint_package = ale#Var(a:buffer, 'go_staticcheck_lint_package')

    " BufferCdString is used so that we can be sure the paths output from
    " staticcheck can be calculated to absolute paths in the Handler
    if l:lint_package
        return ale#path#BufferCdString(a:buffer)
        \   . 'staticcheck'
        \   . (!empty(l:options) ? ' ' . l:options : '') . ' .'
    endif

    return ale#path#BufferCdString(a:buffer)
    \   . 'staticcheck'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' ' . ale#Escape(l:filename)
endfunction

call ale#linter#Define('go', {
\   'name': 'staticcheck',
\   'executable': 'staticcheck',
\   'command': function('ale_linters#go#staticcheck#GetCommand'),
\   'callback': 'ale#handlers#go#Handler',
\   'output_stream': 'both',
\   'lint_file': 1,
\})
