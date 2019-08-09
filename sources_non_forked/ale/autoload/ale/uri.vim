" This probably doesn't handle Unicode characters well.
function! ale#uri#Encode(value) abort
    return substitute(
    \   a:value,
    \   '\([^a-zA-Z0-9\\/$\-_.!*''(),]\)',
    \   '\=printf(''%%%02x'', char2nr(submatch(1)))',
    \   'g'
    \)
endfunction

function! ale#uri#Decode(value) abort
    return substitute(
    \   a:value,
    \   '%\(\x\x\)',
    \   '\=nr2char(''0x'' . submatch(1))',
    \   'g'
    \)
endfunction
