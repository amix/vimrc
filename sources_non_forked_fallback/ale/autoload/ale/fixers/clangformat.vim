scriptencoding utf-8
" Author: Peter Renström <renstrom.peter@gmail.com>
" Description: Fixing C/C++ files with clang-format.

call ale#Set('c_clangformat_executable', 'clang-format')
call ale#Set('c_clangformat_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('c_clangformat_options', '')
call ale#Set('c_clangformat_style_option', '')
call ale#Set('c_clangformat_use_local_file', 0)

function! ale#fixers#clangformat#GetExecutable(buffer) abort
    return ale#path#FindExecutable(a:buffer, 'c_clangformat', [
    \   'clang-format',
    \])
endfunction

function! ale#fixers#clangformat#Fix(buffer) abort
    let l:executable = ale#Escape(ale#fixers#clangformat#GetExecutable(a:buffer))
    let l:filename = ale#Escape(bufname(a:buffer))
    let l:options = ale#Var(a:buffer, 'c_clangformat_options')
    let l:style_option = ale#Var(a:buffer, 'c_clangformat_style_option')
    let l:use_local_file = ale#Var(a:buffer, 'c_clangformat_use_local_file')

    if l:style_option isnot# ''
        let l:style_option = '-style=' . "'" . l:style_option . "'"
    endif

    if l:use_local_file
        let l:config = ale#path#FindNearestFile(a:buffer, '.clang-format')

        if !empty(l:config)
            let l:style_option = '-style=file'
        endif
    endif

    if l:style_option isnot# ''
        let l:options .= ' ' . l:style_option
    endif

    let l:command = l:executable . ' --assume-filename=' . l:filename

    if l:options isnot# ''
        let l:command .= ' ' . l:options
    endif

    return {'command': l:command}
endfunction
