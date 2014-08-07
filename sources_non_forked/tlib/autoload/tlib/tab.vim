" tab.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-08-27.
" @Last Change: 2014-02-06.
" @Revision:    0.0.30

if &cp || exists("loaded_tlib_tab_autoload")
    finish
endif
let loaded_tlib_tab_autoload = 1


" Return a dictionary of bufnumbers => [[tabpage, winnr] ...]
function! tlib#tab#BufMap() "{{{3
    let acc = {}
    for t in range(tabpagenr('$'))
        let bb = tabpagebuflist(t + 1)
        for b in range(len(bb))
            let bn = bb[b]
            let bd = [t + 1, b + 1]
            if has_key(acc, bn)
                call add(acc[bn], bd)
            else
                let acc[bn] = [bd]
            endif
        endfor
    endfor
    return acc
endf


" Find a buffer's window at some tab page.
function! tlib#tab#TabWinNr(buffer) "{{{3
    let bn = bufnr(a:buffer)
    let bt = tlib#tab#BufMap()
    let tn = tabpagenr()
    let wn = winnr()
    let bc = get(bt, bn)
    if !empty(bc)
        for [t, w] in bc
            if t == tn
                return [t, w]
            endif
        endfor
        return bc[0]
    endif
endf


function! tlib#tab#Set(tabnr) "{{{3
    if a:tabnr > 0
        exec a:tabnr .'tabnext'
    endif
endf

