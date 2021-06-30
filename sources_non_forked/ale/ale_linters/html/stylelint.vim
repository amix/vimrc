" Author: Filipe Kiss <hello@filipekiss.com.br> http://github.com/filipekiss

call ale#Set('html_stylelint_executable', 'stylelint')
call ale#Set('html_stylelint_options', '')
call ale#Set('html_stylelint_use_global', 0)

function! ale_linters#html#stylelint#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'html_stylelint', [
    \   'node_modules/.bin/stylelint',
    \])
endfunction

function! ale_linters#html#stylelint#GetCommand(buffer) abort
    let l:executable = ale_linters#html#stylelint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'html_stylelint_options')

    return ale#Escape(l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' --stdin-filename %s'
endfunction

call ale#linter#Define('html', {
\   'name': 'stylelint',
\   'executable': function('ale_linters#html#stylelint#GetExecutable'),
\   'command': function('ale_linters#html#stylelint#GetCommand'),
\   'callback': 'ale#handlers#css#HandleStyleLintFormat',
\})
