" Author: carakan <carakan@gmail.com>, Fernando Mendes <fernando@mendes.codes>
" Description: Fixing files with elixir formatter 'mix format'.

call ale#Set('elixir_mix_executable', 'mix')
call ale#Set('elixir_mix_format_options', '')

function! ale#fixers#mix_format#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'elixir_mix_executable')
endfunction

function! ale#fixers#mix_format#GetCommand(buffer) abort
    let l:executable = ale#Escape(ale#fixers#mix_format#GetExecutable(a:buffer))
    let l:options = ale#Var(a:buffer, 'elixir_mix_format_options')

    return l:executable . ' format'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . ' %t'
endfunction

function! ale#fixers#mix_format#Fix(buffer) abort
    return {
    \   'command': ale#fixers#mix_format#GetCommand(a:buffer),
    \   'read_temporary_file': 1,
    \}
endfunction
