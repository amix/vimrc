" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-03-12.
" @Last Change: 2011-03-10.
" @Revision:    0.0.45

let s:save_cpo = &cpo
set cpo&vim


let s:base = 2327
let s:register = {}


" Clear all signs with name SIGN.
function! tlib#signs#ClearAll(sign) "{{{3
    " TLog a:sign
    for bn in keys(s:register)
        let idxs = keys(s:register)
        call filter(idxs, 's:register[v:val].sign == a:sign')
        " TLogVAR bns
        for idx in idxs
            exec 'sign unplace '. idx .' buffer='. s:register[idx].bn
            call remove(s:register, idx)
        endfor
    endfor
endf


" Clear all signs with name SIGN in buffer BUFNR.
function! tlib#signs#ClearBuffer(sign, bufnr) "{{{3
    for bn in keys(s:register)
        let idxs = keys(s:register)
        call filter(idxs, 's:register[v:val].sign == a:sign && s:register[v:val].bn == a:bufnr')
        " TLogVAR bns
        for idx in idxs
            exec 'sign unplace '. idx .' buffer='. s:register[idx].bn
            call remove(s:register, idx)
        endfor
    endfor
endf


" function! tlib#signs#Clear(sign, list) "{{{3
"     " TLogVAR a:sign
"     let done = []
"     for item in a:list
"         let bn = get(item, 'bufnr', -1)
"         if index(done, bn) == -1
"             let idxs = keys(s:register)
"             call filter(idxs, 's:register[v:val].sign == a:sign && s:register[v:val].bn == bn')
"             for idx in idxs
"                 exec 'sign unplace '. idx .' buffer='. s:register[idx].bn
"                 call remove(s:register, idx)
"             endfor
"             call add(done, bn)
"         endif
"     endfor
" endf


" Add signs for all locations in LIST. LIST must adhere with the 
" quickfix list format (see |getqflist()|; only the fields lnum and 
" bufnr are required).
"
" list:: a quickfix or location list
" sign:: a sign defined with |:sign-define|
function! tlib#signs#Mark(sign, list) "{{{3
    " TLogVAR a:sign
    for item in a:list
        let idx = s:SignId(item)
        if idx >= 0
            let lnum = get(item, 'lnum', 0)
            if lnum > 0
                let bn = get(item, 'bufnr')
                exec ':sign place '. idx .' line='. lnum .' name='. a:sign .' buffer='. bn
                let s:register[idx] = {'sign': a:sign, 'bn': bn}
            endif
        endif
    endfor
endf


function! s:SignId(item) "{{{3
    " TLogVAR a:item
    " let bn  = bufnr('%')
    let bn = get(a:item, 'bufnr', -1)
    if bn == -1
        return -1
    else
        let idx = s:base + bn * 500
        while has_key(s:register, idx)
            let idx += 1
        endwh
        return idx
    endif
endf



let &cpo = s:save_cpo
unlet s:save_cpo
