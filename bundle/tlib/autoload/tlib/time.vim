" time.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-17.
" @Last Change: 2009-02-22.
" @Revision:    0.0.29

if &cp || exists("loaded_tlib_time_autoload")
    finish
endif
let loaded_tlib_time_autoload = 1


function! tlib#time#MSecs() "{{{3
    let rts = reltimestr(reltime())
    return substitute(rts, '\.', '', '')
endf


function! tlib#time#Now() "{{{3
    let rts = reltimestr(reltime())
    let rtl = split(rts, '\.')
    return rtl
endf


function! tlib#time#Diff(a, b, ...) "{{{3
    TVarArg ['resolution', 2]
    let [as, am] = a:a
    let [bs, bm] = a:b
    let rv = 0 + (as - bs)
    if resolution > 0
        let rv .= repeat('0', resolution)
        let am = am[0 : resolution - 1]
        let bm = bm[0 : resolution - 1]
        let rv += (am - bm)
    endif
    return rv
endf


function! tlib#time#DiffMSecs(a, b, ...) "{{{3
    TVarArg ['resolution', 2]
    if a:a == a:b
        return 0
    endif
    let a = printf('%30s', a:a[0 : -(7 - resolution)])
    let b = printf('%30s', a:b[0 : -(7 - resolution)])
    for i in range(0, 29)
        if a[i] != b[i]
            let a = a[i : -1]
            let b = b[i : -1]
            return a - b
        endif
    endfor
    return 0
endf


