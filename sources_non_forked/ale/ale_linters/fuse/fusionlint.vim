" Author: RyanSquared <vandor2012@gmail.com>
" Description: `fusion-lint` linter for FusionScript files

let g:ale_fuse_fusionlint_executable =
\   get(g:, 'ale_fuse_fusionlint_executable', 'fusion-lint')

let g:ale_fuse_fusionlint_options =
\   get(g:, 'ale_fuse_fusionlint_options', '')

function! ale_linters#fuse#fusionlint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'fuse_fusionlint_executable')
endfunction

function! ale_linters#fuse#fusionlint#GetCommand(buffer) abort
    return ale#Escape(ale_linters#fuse#fusionlint#GetExecutable(a:buffer))
    \   . ' ' . ale#Var(a:buffer, 'fuse_fusionlint_options')
    \   . ' --filename %s -i'
endfunction

function! ale_linters#fuse#fusionlint#Handle(buffer, lines) abort
    let l:pattern = '^.*:\(\d\+\):\(\d\+\): (\([WE]\)\d\+) \(.\+\)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:match[4],
        \   'type': l:match[3],
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('fuse', {
\   'name': 'fusionlint',
\   'executable_callback': 'ale_linters#fuse#fusionlint#GetExecutable',
\   'command_callback': 'ale_linters#fuse#fusionlint#GetCommand',
\   'callback': 'ale_linters#fuse#fusionlint#Handle',
\})
