" Author: Pat Brisbin <pbrisbin@gmail.com>, toastal <toastal@protonmail.com>
" Description: Functions for working with Dhall’s executable

call ale#Set('dhall_executable', 'dhall')
call ale#Set('dhall_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('dhall_options', '')

function! ale#dhall#GetExecutable(buffer) abort
    let l:executable = ale#Var(a:buffer, 'dhall_executable')

    " Dhall is written in Haskell and commonly installed with Stack
    return ale#handlers#haskell_stack#EscapeExecutable(l:executable, 'dhall')
endfunction

function! ale#dhall#GetExecutableWithOptions(buffer) abort
    let l:executable = ale#dhall#GetExecutable(a:buffer)

    return l:executable
    \   . ale#Pad(ale#Var(a:buffer, 'dhall_options'))
endfunction

function! ale#dhall#GetCommand(buffer) abort
    return '%e ' . ale#Pad(ale#Var(a:buffer, 'dhall_options'))
endfunction
