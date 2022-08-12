" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    26


function! tlib#number#ConvertBase(num, base, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'string'
    " TLogVAR a:num, a:base, rtype
    if a:base == 32
        let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    elseif a:base == 63 || a:base == 64
        let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    elseif a:base == 85
        let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~"
    elseif a:base <= 62
        let chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    else
        throw 'tlib#number#ConvertBase: base is not supported'
    endif
    let rv = []
    let num = 0.0 + a:num
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


