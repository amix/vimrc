" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    18


function! tlib#number#ConvertBase(num, base, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'string'
    if a:base > 36
        throw 'tlib#number#ConvertBase: base > 36 is not supported'
    endif
    " TLogVAR a:num, a:base, rtype
    let rv = []
    let num = 0.0 + a:num
    let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    while floor(num) > 0.0
        let div = floor(num / a:base)
        let num1 = float2nr(num - a:base * div)
        call insert(rv, chars[num1])
        let num = num / a:base
    endwh
    " TLogVAR rv
    if rtype == 'list'
        return rv
    else
        return join(rv, '')
    endif
endf


