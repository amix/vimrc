" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    124


function! tlib#bitwise#Num2Bits(num) "{{{3
    if type(a:num) <= 1 || type(a:num) == 5
        let bits = reverse(tlib#number#ConvertBase(a:num, 2, 'list'))
    elseif type(a:num) == 3
        let bits = copy(a:num)
    else
        throw "tlib#bitwise#Num2Bits: Must be number of list: ". string(a:num)
    endif
    return bits
endf


function! tlib#bitwise#Bits2Num(bits, ...) "{{{3
    let base = a:0 >= 1 ? a:1 : 10
    " TLogVAR a:bits
    let num = 0.0
    for i in range(len(a:bits))
        if get(a:bits, i, 0)
            let num += pow(2, i)
        endif
    endfor
    " TLogVAR num
    if base == 10
        if type(base) == 5
            return num
        else
            return float2nr(num)
        endif
    else
        return tlib#number#ConvertBase(num, base)
    endif
endf


function! tlib#bitwise#AND(num1, num2, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'num'
    return s:BitwiseComparison(a:num1, a:num2, rtype,
                \ 'get(bits1, v:val) && get(bits2, v:val)')
endf


function! tlib#bitwise#OR(num1, num2, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'num'
    return s:BitwiseComparison(a:num1, a:num2, rtype,
                \ 'get(bits1, v:val) || get(bits2, v:val)')
endf


function! tlib#bitwise#XOR(num1, num2, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'num'
    return s:BitwiseComparison(a:num1, a:num2, rtype,
                \ 'get(bits1, v:val) ? !get(bits2, v:val) : get(bits2, v:val)')
endf


function! s:BitwiseComparison(num1, num2, rtype, expr) "{{{3
    let bits1 = tlib#bitwise#Num2Bits(a:num1)
    let bits2 = tlib#bitwise#Num2Bits(a:num2)
    let range = range(max([len(bits1), len(bits2)]))
    let bits = map(range, a:expr)
    if a:rtype == 'num' || (a:rtype == 'auto' && type(a:num1) <= 1)
        return tlib#bitwise#Bits2Num(bits)
    else
        return bits
    endif
endf


function! tlib#bitwise#ShiftRight(bits, n) "{{{3
    let bits = a:bits[a:n : -1]
    if empty(bits)
        let bits = [0]
    endif
    return bits
endf


function! tlib#bitwise#ShiftLeft(bits, n) "{{{3
    let bits = repeat([0], a:n) + a:bits
    return bits
endf


function! tlib#bitwise#Add(num1, num2, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'num'
    let bits1 = tlib#bitwise#Num2Bits(a:num1)
    let bits2 = tlib#bitwise#Num2Bits(a:num2)
    let range = range(max([len(bits1), len(bits2)]))
    " TLogVAR bits1, bits2, range
    let carry = 0
    let bits  = []
    for i in range
        let sum = get(bits1, i) + get(bits2, i) + carry
        if sum == 3
            let bit = 1
            let carry = 1
        elseif sum == 2
            let bit = 0
            let carry = 1
        elseif sum == 1
            let bit = 1
            let carry = 0
        elseif sum == 0
            let bit = 0
            let carry = 0
        endif
        call add(bits, bit)
        " TLogVAR i, bits, bit
    endfor
    if carry == 1
        call add(bits, carry)
    endif
    if rtype == 'num' || (rtype == 'auto' && type(a:num1) <= 1)
        return tlib#bitwise#Bits2Num(bits)
    else
        return bits
    endif
endf


function! tlib#bitwise#Sub(num1, num2, ...) "{{{3
    let rtype = a:0 >= 1 ? a:1 : 'num'
    let bits1 = tlib#bitwise#Num2Bits(a:num1)
    let bits2 = tlib#bitwise#Num2Bits(a:num2)
    let range = range(max([len(bits1), len(bits2)]))
    let bits2 = map(range, '!get(bits2, v:val)')
    let bits2 = tlib#bitwise#Add(bits2, [1], 'bits')
    let bits3 = tlib#bitwise#Add(bits1, bits2, 'bits')
    let bits = bits3[0 : -2]
    if rtype == 'num' || (rtype == 'auto' && type(a:num1) <= 1)
        return tlib#bitwise#Bits2Num(bits)
    else
        return bits
    endif
endf

