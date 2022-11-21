" Author: Ty-Lucas Kelley <tylucaskelley@gmail.com>
" Description: Adds support for markdownlint

call ale#Set('markdown_markdownlint_options', '')

function! ale_linters#markdown#markdownlint#GetCommand(buffer) abort
    let l:executable = 'markdownlint'

    let l:options = ale#Var(a:buffer, 'markdown_markdownlint_options')

    return ale#Escape(l:executable)
    \ . (!empty(l:options) ? ' ' . l:options : '') . ' %s'
endfunction

call ale#linter#Define('markdown', {
\   'name': 'markdownlint',
\   'executable': 'markdownlint',
\   'lint_file': 1,
\   'output_stream': 'both',
\   'command': function('ale_linters#markdown#markdownlint#GetCommand'),
\   'callback': 'ale#handlers#markdownlint#Handle'
\})
