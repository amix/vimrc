" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    25


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


