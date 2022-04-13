" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    14


function! tlib#number#ConvertBase(num, base, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'string'
    " TLogVAR a:num, a:base, rtype
    let rv = []
    let num = 0.0 + a:num
    while floor(num) > 0.0
        let div = floor(num / a:base)
        let num1 = float2nr(num - a:base * div)
        if a:base <= 10
            call insert(rv, num1)
        elseif a:base == 16
            let char = "0123456789ABCDEF"[num1]
            call insert(rv, char)
        endif
        let num = num / a:base
    endwh
    " TLogVAR rv
    if rtype == 'list'
        return rv
    else
        return join(rv, '')
    endif
endf


