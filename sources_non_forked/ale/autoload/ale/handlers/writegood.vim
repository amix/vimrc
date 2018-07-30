" Author: Sumner Evans <sumner.evans98@gmail.com>
" Description: Error handling for errors in the write-good format.

function! ale#handlers#writegood#ResetOptions() abort
    call ale#Set('writegood_options', '')
    call ale#Set('writegood_executable', 'write-good')
    call ale#Set('writegood_use_global', get(g:, 'ale_use_global_executables', 0))
endfunction

" Reset the options so the tests can test how they are set.
call ale#handlers#writegood#ResetOptions()

function! ale#handlers#writegood#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'writegood', [
    \   'node_modules/.bin/write-good',
    \   'node_modules/write-good/bin/write-good.js',
    \])
endfunction

function! ale#handlers#writegood#GetCommand(buffer) abort
    let l:executable = ale#handlers#writegood#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'writegood_options')

    return ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

function! ale#handlers#writegood#Handle(buffer, lines) abort
    " Look for lines like the following.
    "
    " "it is" is wordy or unneeded on line 20 at column 53
    " "easily" can weaken meaning on line 154 at column 29
    let l:marks_pattern = '\v^ *(\^+) *$'
    let l:pattern = '\v^(".*"\s.*)\son\sline\s(\d+)\sat\scolumn\s(\d+)$'
    let l:output = []
    let l:last_len = 0

    for l:match in ale#util#GetMatches(a:lines, [l:marks_pattern, l:pattern])
        if empty(l:match[2])
            let l:last_len = len(l:match[1])
        else
            let l:col = l:match[3] + 1

            " Add the linter error. Note that we need to add 1 to the col because
            " write-good reports the column corresponding to the space before the
            " offending word or phrase.
            call add(l:output, {
            \   'text': l:match[1],
            \   'lnum': l:match[2] + 0,
            \   'col': l:col,
            \   'end_col': l:last_len ? (l:col + l:last_len - 1) : l:col,
            \   'type': 'W',
            \})

            let l:last_len = 0
        endif
    endfor

    return l:output
endfunction

" Define the writegood linter for a given filetype.
function! ale#handlers#writegood#DefineLinter(filetype) abort
    call ale#linter#Define(a:filetype, {
    \   'name': 'writegood',
    \   'aliases': ['write-good'],
    \   'executable_callback': 'ale#handlers#writegood#GetExecutable',
    \   'command_callback': 'ale#handlers#writegood#GetCommand',
    \   'callback': 'ale#handlers#writegood#Handle',
    \})
endfunction
