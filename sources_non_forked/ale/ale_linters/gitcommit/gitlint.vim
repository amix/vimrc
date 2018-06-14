" Author: Nick Yamane <nick.diego@gmail.com>
" Description: gitlint for git commit message files

let g:ale_gitcommit_gitlint_executable =
\   get(g:, 'ale_gitcommit_gitlint_executable', 'gitlint')
let g:ale_gitcommit_gitlint_options = get(g:, 'ale_gitcommit_gitlint_options', '')
let g:ale_gitcommit_gitlint_use_global = get(g:, 'ale_gitcommit_gitlint_use_global', get(g:, 'ale_use_global_executables', 0))


function! ale_linters#gitcommit#gitlint#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'gitcommit_gitlint', ['gitlint'])
endfunction

function! ale_linters#gitcommit#gitlint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'gitcommit_gitlint_options')
    let l:executable = ale_linters#gitcommit#gitlint#GetExecutable(a:buffer)
    return ale#Escape(l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' lint'
endfunction


function! ale_linters#gitcommit#gitlint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    let l:pattern = '\v^(\d+): (\w+) (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[2]

        if l:code is# 'T2' && !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
            continue
        endif

        let l:item = {
        \   'lnum': l:match[1] + 0,
        \   'text': l:match[3],
        \   'code': l:code,
        \   'type': 'E',
        \}

        call add(l:output, l:item)
    endfor

    return l:output
endfunction


call ale#linter#Define('gitcommit', {
\   'name': 'gitlint',
\   'output_stream': 'stderr',
\   'executable_callback': 'ale_linters#gitcommit#gitlint#GetExecutable',
\   'command_callback': 'ale_linters#gitcommit#gitlint#GetCommand',
\   'callback': 'ale_linters#gitcommit#gitlint#Handle',
\})

