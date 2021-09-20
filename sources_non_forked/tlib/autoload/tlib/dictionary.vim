" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2016-04-06
" @Revision:    22


" :display: tlib#dictionary#Rev(dict, ?opts = {}) abort "{{{3
function! tlib#dictionary#Rev(dict, ...) abort "{{{3
    let opts = a:0 >= 1 ? a:1 : {}
    Tlibtype a:dict, 'dict', opts, 'dict'
    let rev = {}
    let use_string = get(opts, 'use_string', 0)
    let use_eval = get(opts, 'use_eval', 0)
    let values_as_list = get(opts, 'values_as_list', 0)
    for [m, f] in items(a:dict)
        if use_string
            let k = string(f)
        else
            let k = type(f) == 1 ? f : string(f)
            if k ==# ''
                let k = get(opts, 'empty', '')
                if empty(k)
                    continue
                endif
            endif
        endif
        if use_eval
            let v = eval(m)
        else
            let v = m
        endif
        if values_as_list
            if has_key(rev, k)
                call add(rev[k], v)
            else
                let rev[k] = [v]
            endif
        else
            let rev[k] = v
        endif
    endfor
    return rev
endf

