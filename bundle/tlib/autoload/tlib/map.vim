" map.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-08-23.
" @Last Change: 2009-08-23.
" @Revision:    0.0.4

let s:save_cpo = &cpo
set cpo&vim


" If |pumvisible()| is true, return "\<c-y>". Otherwise return a:key.
" For use in maps like: >
"   imap <expr> <cr> tlib#map#PumAccept("\<cr>")
function! tlib#map#PumAccept(key) "{{{3
    return pumvisible() ? "\<c-y>" : a:key
endf



let &cpo = s:save_cpo
unlet s:save_cpo
