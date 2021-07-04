" Author: Chen YuanYuan <cyyever@outlook.com>
" Description: Integration of fish_indent with ALE.

call ale#Set('fish_fish_indent_executable', 'fish_indent')
call ale#Set('fish_fish_indent_options', '')

function! ale#fixers#fish_indent#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'fish_fish_indent_executable')
    let l:options = ale#Var(a:buffer, 'fish_fish_indent_options')
    let l:filename = ale#Escape(bufname(a:buffer))

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -w '
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
