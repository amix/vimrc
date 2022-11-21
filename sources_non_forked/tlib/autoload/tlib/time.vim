" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    42


function! tlib#time#MSecs() "{{{3
    let rts = reltimestr(reltime())
    return substitute(rts, '\.', '', '')
endf


function! tlib#time#Now() "{{{3
    if has('reltime')
        let rts = reltimestr(reltime())
        let rtl = map(split(rts, '\.'), 'str2nr(v:val)')
    else
        let rtl = [localtime()]
    endif
    return rtl
endf


function! tlib#time#FormatNow() "{{{3
    let rtl = tlib#time#Now()
    if len(rtl) == 2
        let rts = strftime(g:tlib#date#date_format .' %H:%M:%S', rtl[0]) .'.'. rtl[1]
    else
        let rts = strftime(g:tlib#date#date_format .' %H:%M:%S', rtl[0])
    endif
    return rts
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


function! tlib#time#Command(cmd, ...) abort "{{{3
    let loops = a:0 >= 1 ? a:1 : 1
    let silent = a:0 >= 1 ? a:1 : 0
    let start = tlib#time#Now()
    for loop in range(loops)
        if silent
            silent! exec a:cmd
        else
            exec a:cmd
        endif
    endfor
    let end = tlib#time#Now()
    let diff = tlib#time#Diff(end, start)
    echom 'Time:' diff
    return diff
endf

