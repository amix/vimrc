" Author: Nick Yamane <nick.diego@gmail.com>
" Description: gitlint for git commit message files

call ale#Set('gitcommit_gitlint_executable', 'gitlint')
call ale#Set('gitcommit_gitlint_options', '')
call ale#Set('gitcommit_gitlint_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#gitcommit#gitlint#GetExecutable(buffer) abort
    return ale#python#FindExecutable(a:buffer, 'gitcommit_gitlint', ['gitlint'])
endfunction

function! ale_linters#gitcommit#gitlint#GetCommand(buffer) abort
    let l:options = ale#Var(a:buffer, 'gitcommit_gitlint_options')

    return '%e' . ale#Pad(l:options) . ' lint'
endfunction

function! ale_linters#gitcommit#gitlint#Handle(buffer, lines) abort
    " Matches patterns line the following:
    let l:pattern = '\v^(\d+): (\w+) (.*)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[2]

        if !ale#Var(a:buffer, 'warn_about_trailing_whitespace')
            if l:code is# 'T2' || l:code is# 'B2'
                continue
            endif
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
\   'executable': function('ale_linters#gitcommit#gitlint#GetExecutable'),
\   'command': function('ale_linters#gitcommit#gitlint#GetCommand'),
\   'callback': 'ale_linters#gitcommit#gitlint#Handle',
\})
