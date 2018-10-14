" Author: RyanSquared <vandor2012@gmail.com>
" Description: `fusion-lint` linter for FusionScript files

call ale#Set('fuse_fusionlint_executable', 'fusion-lint')
call ale#Set('fuse_fusionlint_options', '')

function! ale_linters#fuse#fusionlint#GetCommand(buffer) abort
    return '%e' . ale#Pad(ale#Var(a:buffer, 'fuse_fusionlint_options'))
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
\   'executable_callback': ale#VarFunc('fuse_fusionlint_executable'),
\   'command_callback': 'ale_linters#fuse#fusionlint#GetCommand',
\   'callback': 'ale_linters#fuse#fusionlint#Handle',
\})
