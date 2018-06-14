" Author: Jeffrey Lau https://github.com/zoonfafer
" Description: Integration of Scalafmt with ALE.

call ale#Set('scala_scalafmt_executable', 'scalafmt')
call ale#Set('scala_scalafmt_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('scala_scalafmt_options', '')

function! ale#fixers#scalafmt#GetCommand(buffer) abort
    let l:executable = ale#Var(a:buffer, 'scala_scalafmt_executable')
    let l:options = ale#Var(a:buffer, 'scala_scalafmt_options')
    let l:exec_args = l:executable =~? 'ng$'
    \   ? ' scalafmt'
    \   : ''

    return ale#Escape(l:executable) . l:exec_args
    \   . (empty(l:options) ? '' : ' ' . l:options)
    \   . ' %t'

endfunction

function! ale#fixers#scalafmt#Fix(buffer) abort
    return {
    \   'command': ale#fixers#scalafmt#GetCommand(a:buffer),
    \   'read_temporary_file': 1,
    \}
endfunction
