" url.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2011-03-10.
" @Revision:    0.0.28


" TODO: These functions could use printf() now.

" Decode an encoded URL.
function! tlib#url#Decode(url) "{{{3
    return substitute(a:url, '\(+\|%\(%\|\x\x\)\)', '\=tlib#url#DecodeChar(submatch(1))', 'g')
endf


" Decode a single character.
function! tlib#url#DecodeChar(char) "{{{3
    if a:char == '%%'
        return '%'
    elseif a:char == '+'
        return ' '
    else
        return nr2char("0x".a:char[1 : -1])
    endif
endf


" Encode a single character.
function! tlib#url#EncodeChar(char) "{{{3
    if a:char == '%'
        return '%%'
    elseif a:char == ' '
        return '+'
    else
        return printf("%%%X", char2nr(a:char))
    endif
endf


" Encode an URL.
function! tlib#url#Encode(url, ...) "{{{3
    TVarArg ['extrachars', '']
    let rx = '\([^a-zA-Z0-9_.'. extrachars .'-]\)'
    " TLogVAR a:url, rx
    let rv = substitute(a:url, rx, '\=tlib#url#EncodeChar(submatch(1))', 'g')
    " TLogVAR rv
    return rv
endf


