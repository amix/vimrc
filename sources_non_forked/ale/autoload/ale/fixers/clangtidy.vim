scriptencoding utf-8
" Author: ObserverOfTime <chronobserver@disroot.org>
" Description: Fixing C/C++ files with clang-tidy.

function! s:set_variables() abort
    let l:use_global = get(g:, 'ale_use_global_executables', 0)

    for l:ft in ['c', 'cpp']
        call ale#Set(l:ft . '_clangtidy_executable', 'clang-tidy')
        call ale#Set(l:ft . '_clangtidy_use_global', l:use_global)
        call ale#Set(l:ft . '_clangtidy_checks', [])
        call ale#Set(l:ft . '_clangtidy_options', '')
        call ale#Set(l:ft . '_clangtidy_extra_options', '')
        call ale#Set(l:ft . '_clangtidy_fix_errors', 1)
    endfor

    call ale#Set('c_build_dir', '')
endfunction

call s:set_variables()

function! ale#fixers#clangtidy#Var(buffer, name) abort
    let l:ft = getbufvar(str2nr(a:buffer), '&filetype')
    let l:ft = l:ft =~# 'cpp' ? 'cpp' : 'c'

    return ale#Var(a:buffer, l:ft . '_clangtidy_' . a:name)
endfunction

function! ale#fixers#clangtidy#GetCommand(buffer) abort
    let l:checks = join(ale#fixers#clangtidy#Var(a:buffer, 'checks'), ',')
    let l:extra_options = ale#fixers#clangtidy#Var(a:buffer, 'extra_options')
    let l:build_dir = ale#c#GetBuildDirectory(a:buffer)
    let l:options = empty(l:build_dir)
    \   ? ale#fixers#clangtidy#Var(a:buffer, 'options') : ''
    let l:fix_errors = ale#fixers#clangtidy#Var(a:buffer, 'fix_errors')

    return ' -fix' . (l:fix_errors ? ' -fix-errors' : '')
    \   . (empty(l:checks) ? '' : ' -checks=' . ale#Escape(l:checks))
    \   . (empty(l:extra_options) ? '' : ' ' . l:extra_options)
    \   . (empty(l:build_dir) ? '' : ' -p ' . ale#Escape(l:build_dir))
    \   . ' %t' . (empty(l:options) ? '' : ' -- ' . l:options)
endfunction

function! ale#fixers#clangtidy#Fix(buffer) abort
    let l:executable = ale#fixers#clangtidy#Var(a:buffer, 'executable')
    let l:command = ale#fixers#clangtidy#GetCommand(a:buffer)

    return {
    \   'command': ale#Escape(l:executable) . l:command,
    \   'read_temporary_file': 1,
    \}
endfunction
