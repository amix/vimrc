" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    276


if !exists('g:tlib#hash#use_crc32')
    let g:tlib#hash#use_crc32 = ''   "{{{2
endif


if !exists('g:tlib#hash#use_adler32')
    let g:tlib#hash#use_adler32 = ''   "{{{2
endif


function! tlib#hash#CRC32B(chars) "{{{3
    if !empty(g:tlib#hash#use_crc32)
        let use = g:tlib#hash#use_crc32
    elseif has('ruby')
        let use = 'ruby'
    else
        let use = 'vim'
    endif
    if exists('*tlib#hash#CRC32B_'. use)
        return tlib#hash#CRC32B_{use}(a:chars)
    else
        throw "Unknown version of tlib#hash#CRC32B: ". use
    endif
endf


function! tlib#hash#CRC32B_ruby(chars) "{{{3
    if has('ruby')
        let rv = ''
        if !exists('s:loaded_ruby_zlib')
            ruby require 'zlib'
            let s:loaded_ruby_zlib = 1
        endif
        ruby VIM::command('let rv = "%08X"' % Zlib.crc32(VIM::evaluate("a:chars")))
        return rv
    else
        throw "tlib#hash#CRC32B_ruby not supported in this version of vim"
    endif
endf


function! tlib#hash#CRC32B_vim(chars) "{{{3
    if !exists('s:crc_table')
        let cfile = tlib#persistent#Filename('tlib', 'crc_table', 1)
        let s:crc_table = tlib#persistent#Value(cfile, 'tlib#hash#CreateCrcTable', 0)
    endif
    let xFFFF_FFFF = repeat([1], 32)
    let crc = tlib#bitwise#XOR([0], xFFFF_FFFF, 'bits')
    for char in split(a:chars, '\zs')
        let octet = char2nr(char)
        let r1 = tlib#bitwise#ShiftRight(crc, 8)
        let i0 = tlib#bitwise#AND(crc, xFFFF_FFFF, 'bits')
        let i1 = tlib#bitwise#XOR(i0, octet, 'bits')
        let i2 = tlib#bitwise#Bits2Num(tlib#bitwise#AND(i1, 0xff, 'bits'))
        let r2 = s:crc_table[i2]
        let crc = tlib#bitwise#XOR(r1, r2, 'bits')
    endfor
    let crc = tlib#bitwise#XOR(crc, xFFFF_FFFF, 'bits')
    let rv = tlib#bitwise#Bits2Num(crc, 16)
    if len(rv) < 8
        let rv = repeat('0', 8 - len(rv)) . rv
    endif
    return rv
endf


" :nodoc:
function! tlib#hash#CreateCrcTable() "{{{3
    let sum = 0.0
    for exponent in [0, 1, 2, 4, 5, 7, 8, 10, 11, 12, 16, 22, 23, 26, 32]
        let exp = tlib#bitwise#Bits2Num(repeat([0], 32 - exponent) + [1], 10.0)
        let sum += exp
    endfor
    let divisor = tlib#bitwise#Num2Bits(sum)
    let crc_table = []
    for octet in range(256)
        let remainder = tlib#bitwise#Num2Bits(octet)
        for i in range(8)
            if get(remainder, i) != 0
                let remainder = tlib#bitwise#XOR(remainder, tlib#bitwise#ShiftLeft(divisor, i), "bits")
            endif
        endfor
        let remainder = tlib#bitwise#ShiftRight(remainder, 8)
        call add(crc_table, remainder)
    endfor
    return crc_table
endf


function! tlib#hash#Adler32(chars) "{{{3
    if !empty(g:tlib#hash#use_adler32)
        let use = g:tlib#hash#use_adler32
    elseif exists('*or')
        let use = 'vim'
    else
        let use = 'tlib'
    endif
    if exists('*tlib#hash#Adler32_'. use)
        return tlib#hash#Adler32_{use}(a:chars)
    else
        throw "Unknown version of tlib#hash#Adler32_: ". use
    endif
endf


function! tlib#hash#Adler32_vim(chars) "{{{3
    if exists('*or')
        let mod_adler = 65521
        let a = 1
        let b = 0
        for index in range(len(a:chars))
            let c = char2nr(a:chars[index])
            let a = (a + c) % mod_adler
            let b = (b + a) % mod_adler
        endfor
        let bb = b * float2nr(pow(2, 16))
        let checksum = or(bb, a)
        " TLogVAR checksum, a, b, bb
        return printf("%08X", checksum)
    else
        throw "TLIB: Vim version doesn't support bitwise or()"
    endif
endf


function! tlib#hash#Adler32_tlib(chars) "{{{3
    let mod_adler = 65521
    let a = 1
    let b = 0
    for index in range(len(a:chars))
        let c = char2nr(a:chars[index])
        let a = (a + c) % mod_adler
        let b = (b + a) % mod_adler
    endfor
    let bb = tlib#bitwise#ShiftLeft(tlib#bitwise#Num2Bits(b), 16)
    let checksum = tlib#bitwise#OR(bb, a, "bits")
    return printf('%08s', tlib#bitwise#Bits2Num(checksum, 16))
endf


