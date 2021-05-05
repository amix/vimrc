" Author: Michel Lang <michellang@gmail.com>, w0rp <devw0rp@gmail.com>,
"         Fenner Macrae <fmacrae.dev@gmail.com>
" Description: This file adds support for checking R code with lintr.

let g:ale_r_lintr_options = get(g:, 'ale_r_lintr_options', 'with_defaults()')
" A reasonable alternative default:
"   get(g:, 'ale_r_lintr_options', 'with_defaults(object_usage_linter = NULL)')


let g:ale_r_lintr_lint_package = get(g:, 'ale_r_lintr_lint_package', 0)

function! ale_linters#r#lintr#GetCommand(buffer) abort
    if ale#Var(a:buffer, 'r_lintr_lint_package')
        let l:lint_cmd = 'lint_package(cache = FALSE, linters = '
        \   . ale#Var(a:buffer, 'r_lintr_options') . ')'
    else
        let l:lint_cmd = 'lint(cache = FALSE, commandArgs(TRUE), '
        \   . ale#Var(a:buffer, 'r_lintr_options') . ')'
    endif

    let l:cmd_string = 'suppressPackageStartupMessages(library(lintr));'
    \   . l:lint_cmd

    return 'Rscript --vanilla -e ' . ale#Escape(l:cmd_string) . ' %t'
endfunction

call ale#linter#Define('r', {
\   'name': 'lintr',
\   'executable': 'Rscript',
\   'cwd': '%s:h',
\   'command': function('ale_linters#r#lintr#GetCommand'),
\   'callback': 'ale#handlers#gcc#HandleGCCFormat',
\   'output_stream': 'both',
\})
