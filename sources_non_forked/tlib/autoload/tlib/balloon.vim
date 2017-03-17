" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/tlib_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-08-30.
" @Last Change: 2015-11-23.
" @Revision:    48


function! tlib#balloon#Register(expr) "{{{3
    if !has('balloon_eval')
        return
    endif
    if !exists('b:tlib_balloons')
        let b:tlib_balloons = []
    endif
    if !&ballooneval
        setlocal ballooneval
    endif
    if &balloonexpr != 'tlib#balloon#Expr()'
        if !empty(&balloonexpr)
            call add(b:tlib_balloons, &balloonexpr)
        endif
        setlocal ballooneval balloonexpr=tlib#balloon#Expr()
    endif
    if index(b:tlib_balloons, a:expr) == -1
        call add(b:tlib_balloons, a:expr)
    endif
endf


function! tlib#balloon#Remove(expr) "{{{3
    if exists('b:tlib_balloons')
        call filter(b:tlib_balloons, 'v:val != a:expr')
        if empty(b:tlib_balloons)
            setlocal ballooneval&
            setlocal balloonexpr&
            unlet b:tlib_balloons
        endif
    endif
endf


function! tlib#balloon#Expr() "{{{3
    " TLogVAR exists('b:tlib_balloons')
    if !exists('b:tlib_balloons')
        return ''
    endif
    let text = map(copy(b:tlib_balloons), 'eval(v:val)')
    " TLogVAR b:tlib_balloons, text
    call filter(text, '!empty(v:val)')
    if has('balloon_multiline')
        return join(text, "\n----------------------------------\n")
    else
        return get(text, 0, '')
    endif
endf


function! tlib#balloon#Expand(expr) abort "{{{3
    if v:beval_bufnr != bufnr('%')
        " TLogVAR v:beval_bufnr, bufnr('%')
        return ''
    endif
    let win = winsaveview()
    try
        call setpos('.', [v:beval_bufnr, v:beval_lnum, v:beval_col, 0])
        return expand(a:expr)
    finally
        call winrestview(win)
    endtry
endf

