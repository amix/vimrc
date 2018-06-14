" Author: KabbAmine - https://github.com/KabbAmine, Ben Falconer
" <ben@falconers.me.uk>

function! ale_linters#scss#sasslint#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer)
    \   . ale#Escape('sass-lint')
    \   . ' -v'
    \   . ' -q'
    \   . ' -f compact'
    \   . ' %t'
endfunction

call ale#linter#Define('scss', {
\   'name': 'sasslint',
\   'executable': 'sass-lint',
\   'command_callback': 'ale_linters#scss#sasslint#GetCommand',
\   'callback': 'ale#handlers#css#HandleCSSLintFormat',
\})
