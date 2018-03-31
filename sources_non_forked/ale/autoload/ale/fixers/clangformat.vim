scriptencoding utf-8
" Author: Peter Renström <renstrom.peter@gmail.com>
" Description: Fixing C/C++ files with clang-format.

call ale#Set('c_clangformat_executable', 'clang-format')
call ale#Set('c_clangformat_use_global', 0)
call ale#Set('c_clangformat_options', '')

function! ale#fixers#clangformat#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'c_clangformat', [
    \   'clang-format',
    \])
endfunction

function! ale#fixers#clangformat#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'c_clangformat_options')

    return {
    \   'command': ale#Escape(ale#fixers#clangformat#GetExecutable(a:buffer))
    \       . ' ' . l:options,
    \}
endfunction
