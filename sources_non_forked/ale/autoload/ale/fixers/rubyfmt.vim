" Author: Yining <zhang.yining@gmail.com>
" Description: support rubyfmt as ALE fixer for Ruby files

call ale#Set('ruby_rubyfmt_executable', 'rubyfmt')
call ale#Set('ruby_rubyfmt_options', '')

function! ale#fixers#rubyfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'ruby_rubyfmt_executable')
    let l:options = ale#Var(a:buffer, 'ruby_rubyfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options)
    \}
endfunction

