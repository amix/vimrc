" Author: blyoa <blyoa110@gmail.com>
" Description: Fixing files with remark-lint.

call ale#Set('markdown_remark_lint_executable', 'remark')
call ale#Set('markdown_remark_lint_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('markdown_remark_lint_options', '')

function! ale#fixers#remark_lint#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'markdown_remark_lint', [
    \   'node_modules/remark-cli/cli.js',
    \   'node_modules/.bin/remark',
    \])
endfunction

function! ale#fixers#remark_lint#Fix(buffer) abort
    let l:executable = ale#fixers#remark_lint#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'markdown_remark_lint_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (!empty(l:options) ? ' ' . l:options : ''),
    \}
endfunction

