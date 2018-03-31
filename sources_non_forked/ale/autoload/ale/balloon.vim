" Author: w0rp <devw0rp@gmail.com>
" Description: balloonexpr support for ALE.

function! ale#balloon#MessageForPos(bufnr, lnum, col) abort
    " Don't show balloons if they are disabled, or linting is disabled.
    if !ale#Var(a:bufnr, 'set_balloons')
    \|| !g:ale_enabled
    \|| !getbufvar(a:bufnr, 'ale_enabled', 1)
        return ''
    endif

    let l:loclist = get(g:ale_buffer_info, a:bufnr, {'loclist': []}).loclist
    let l:index = ale#util#BinarySearch(l:loclist, a:bufnr, a:lnum, a:col)

    return l:index >= 0 ? l:loclist[l:index].text : ''
endfunction

function! ale#balloon#Expr() abort
    return ale#balloon#MessageForPos(v:beval_bufnr, v:beval_lnum, v:beval_col)
endfunction

function! ale#balloon#Disable() abort
    set noballooneval balloonexpr=
endfunction

function! ale#balloon#Enable() abort
    set ballooneval balloonexpr=ale#balloon#Expr()
endfunction
