" Author: Ty-Lucas Kelley <tylucaskelley@gmail.com>
" Description: Adds support for markdownlint

call ale#Set('markdown_markdownlint_executable', 'markdownlint')
call ale#Set('markdown_markdownlint_options', '')

function! ale_linters#markdown#markdownlint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'markdown_markdownlint_executable')
endfunction

function! ale_linters#markdown#markdownlint#GetCommand(buffer) abort
    let l:executable = ale_linters#markdown#markdownlint#GetExecutable(a:buffer)

    let l:options = ale#Var(a:buffer, 'markdown_markdownlint_options')

    return ale#Escape(l:executable)
    \ . (!empty(l:options) ? ' ' . l:options : '') . ' %s'
endfunction

call ale#linter#Define('markdown', {
\   'name': 'markdownlint',
\   'executable': function('ale_linters#markdown#markdownlint#GetExecutable'),
\   'lint_file': 1,
\   'output_stream': 'both',
\   'command': function('ale_linters#markdown#markdownlint#GetCommand'),
\   'callback': 'ale#handlers#markdownlint#Handle'
\})
