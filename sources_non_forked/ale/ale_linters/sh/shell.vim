" Author: w0rp <devw0rp@gmail.com>
" Description: Lints sh files using bash -n

" Backwards compatibility
if exists('g:ale_linters_sh_shell_default_shell')
    let g:ale_sh_shell_default_shell = g:ale_linters_sh_shell_default_shell
endif

" This option can be changed to change the default shell when the shell
" cannot be taken from the hashbang line.
if !exists('g:ale_sh_shell_default_shell')
    let g:ale_sh_shell_default_shell = fnamemodify($SHELL, ':t')

    if g:ale_sh_shell_default_shell is# '' || g:ale_sh_shell_default_shell is# 'fish'
        let g:ale_sh_shell_default_shell = 'bash'
    endif
endif

function! ale_linters#sh#shell#GetExecutable(buffer) abort
    let l:shell_type = ale#handlers#sh#GetShellType(a:buffer)

    if !empty(l:shell_type)
        return l:shell_type
    endif

    return ale#Var(a:buffer, 'sh_shell_default_shell')
endfunction

function! ale_linters#sh#shell#GetCommand(buffer) abort
    return ale_linters#sh#shell#GetExecutable(a:buffer) . ' -n %t'
endfunction

function! ale_linters#sh#shell#Handle(buffer, lines) abort
    " Matches patterns line the following:
    "
    " bash: line 13: syntax error near unexpected token `d'
    " sh: 11: Syntax error: "(" unexpected
    let l:pattern = '\v(line |: ?)(\d+): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': str2nr(l:match[2]),
        \   'text': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('sh', {
\   'name': 'shell',
\   'output_stream': 'stderr',
\   'executable': function('ale_linters#sh#shell#GetExecutable'),
\   'command': function('ale_linters#sh#shell#GetCommand'),
\   'callback': 'ale_linters#sh#shell#Handle',
\})
