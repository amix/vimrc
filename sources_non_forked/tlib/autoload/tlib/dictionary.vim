" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2015-10-14
" @Revision:    2


function! tlib#dictionary#Rev(dict) abort "{{{3
    let rev = {}
    for [m, f] in items(a:dict)
        let rev[f] = m
    endfor
    return rev
endf

