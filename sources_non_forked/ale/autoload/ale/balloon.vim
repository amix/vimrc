" Author: w0rp <devw0rp@gmail.com>
" Description: balloonexpr support for ALE.

function! ale#balloon#MessageForPos(bufnr, lnum, col) abort
    let l:set_balloons = ale#Var(a:bufnr, 'set_balloons')
    let l:show_problems = 0
    let l:show_hover = 0

    if l:set_balloons is 1
        let l:show_problems = 1
        let l:show_hover = 1
    elseif l:set_balloons is# 'hover'
        let l:show_hover = 1
    endif

    " Don't show balloons if they are disabled, or linting is disabled.
    if !(l:show_problems || l:show_hover)
    \|| !g:ale_enabled
    \|| !getbufvar(a:bufnr, 'ale_enabled', 1)
        return ''
    endif

    if l:show_problems
        let l:loclist = get(g:ale_buffer_info, a:bufnr, {'loclist': []}).loclist
        let l:index = ale#util#BinarySearch(l:loclist, a:bufnr, a:lnum, a:col)
    endif

    " Show the diagnostics message if found, 'Hover' output otherwise
    if l:show_problems && l:index >= 0
        return l:loclist[l:index].text
    elseif l:show_hover && (
    \   exists('*balloon_show')
    \   || getbufvar(
    \       a:bufnr,
    \       'ale_set_balloons_legacy_echo',
    \       get(g:, 'ale_set_balloons_legacy_echo', 0)
    \   )
    \)
        " Request LSP/tsserver hover information, but only if this version of
        " Vim supports the balloon_show function, or if we turned a legacy
        " setting on.
        call ale#hover#Show(a:bufnr, a:lnum, a:col, {'called_from_balloonexpr': 1})
    endif

    return ''
endfunction

function! ale#balloon#Expr() abort
    return ale#balloon#MessageForPos(v:beval_bufnr, v:beval_lnum, v:beval_col)
endfunction

function! ale#balloon#Disable() abort
    if has('balloon_eval')
        set noballooneval
        set balloonexpr=
    endif

    if has('balloon_eval_term')
        set noballoonevalterm
        set balloonexpr=
    endif
endfunction

function! ale#balloon#Enable() abort
    if has('balloon_eval')
        set ballooneval
        set balloonexpr=ale#balloon#Expr()
    endif

    if has('balloon_eval_term')
        set balloonevalterm
        set balloonexpr=ale#balloon#Expr()
    endif
endfunction
