" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-10-24
" @Revision:    2


function! tlib#loclist#Browse(...) abort "{{{3
    let list = getloclist(0)
    return call(function('tlib#qfl#QflList'), [list] + a:000)
endf


