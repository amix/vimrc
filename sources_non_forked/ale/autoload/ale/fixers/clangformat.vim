scriptencoding utf-8
" Author: Peter Renström <renstrom.peter@gmail.com>
" Description: Fixing C/C++ files with clang-format.

call ale#Set('c_clangformat_executable', 'clang-format')
call ale#Set('c_clangformat_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('c_clangformat_options', '')

function! ale#fixers#clangformat#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'c_clangformat', [
    \   'clang-format',
    \])
endfunction

function! ale#fixers#clangformat#Fix(buffer) abort
    let l:executable = ale#Escape(ale#fixers#clangformat#GetExecutable(a:buffer))
    let l:filename = ale#Escape(bufname(a:buffer))
    let l:options = ale#Var(a:buffer, 'c_clangformat_options')

    let l:command = l:executable . ' --assume-filename=' . l:filename

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return {'command': l:command}
endfunction
