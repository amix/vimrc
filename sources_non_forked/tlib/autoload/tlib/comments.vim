" comments.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-11-15.
" @Last Change: 2009-02-15.
" @Revision:    0.0.24

if &cp || exists("loaded_tlib_comments_autoload")
    finish
endif
let loaded_tlib_comments_autoload = 1
let s:save_cpo = &cpo
set cpo&vim


" function! tlib#comments#Comments(?rx='')
function! tlib#comments#Comments(...)
    TVarArg ['rx', '']
    let comments = {}
    let co = &comments
    while !empty(co)
        " TLogVAR co
        let [m_0, m_key, m_val, m_val1, co0, co; rest] = matchlist(co, '^\([^:]*\):\(\(\\.\|[^,]*\)\+\)\(,\(.*\)$\|$\)')
        " TLogVAR m_key, m_val, co
        if empty(m_key)
            let m_key = ':'
        endif
        if empty(rx) || m_key =~ rx
            let comments[m_key] = m_val
        endif
    endwh
    return comments
endf


" function! tlib#comments#PartitionLine(line) "{{{3
"     if !empty(&commentstring)
"         let cs = '^\(\s*\)\('. printf(tlib#rx#Escape(&commentstring), '\)\(.\{-}\)\(') .'\)\(.*\)$'
"         let ml = matchlist(a:line, cs)
"     else
"         let ml = []
"     endif
"     if !empty(ml)
"         let [m_0, pre, open, line, close, post; rest] = ml
"     else
"         let [m_0, pre, line; rest] = matchstr(a:line, '^\(\s*\)\(.*\)$')
"         for [key, val] in tlib#comments#Comments()
"             if +++
"         endfor
"     endif
"     return {'pre': pre, 'open': open, 'line': line, 'close': close, 'post': post}
" endf


let &cpo = s:save_cpo
unlet s:save_cpo
