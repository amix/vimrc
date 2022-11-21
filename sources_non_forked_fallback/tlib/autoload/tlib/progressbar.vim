" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    86

let s:id = 0
let s:ids = []
let s:statusline = []
let s:laststatus = []
let s:max = []
let s:format = []
let s:width = []
let s:value = []
let s:timestamp = -1

" EXAMPLE: >
"     call tlib#progressbar#Init(20)
"     try
"         for i in range(20)
"             call tlib#progressbar#Display(i)
"             call DoSomethingThatTakesSomeTime(i)
"         endfor
"     finally
"         call tlib#progressbar#Restore()
"     endtry
function! tlib#progressbar#Init(max, ...) "{{{3
    TVarArg ['format', '%s'], ['width', 10]
    let s:id += 1
    call insert(s:ids, s:id)
    call insert(s:statusline, &statusline)
    call insert(s:laststatus, &laststatus)
    call insert(s:max, a:max)
    call insert(s:format, format)
    call insert(s:width, width)
    call insert(s:value, -1)
    let sl = {
                \ 'id': s:id,
                \ 'statusline': &statusline,
                \ 'laststatus': &laststatus,
                \ 'max': a:max,
                \ 'format': format,
                \ 'width': width,
                \ 'value': -1
                \ }
    let &laststatus = 2
    let s:timestamp = localtime()
    return sl
endf


function! tlib#progressbar#Restore(...) "{{{3
    if a:0 >= 1
        let sl = a:1
        let idx = index(s:ids, sl.id)
        let &statusline = sl.statusline
        let &laststatus = sl.laststatus
    else
        let idx = 0
        let &statusline = remove(s:statusline, idx)
        let &laststatus = remove(s:laststatus, idx)
    endif
    call remove(s:ids, idx)
    call remove(s:max, idx)
    call remove(s:format, idx)
    call remove(s:width, idx)
    call remove(s:value, idx)
    redrawstatus
    " redraw
    " echo
endf


function! tlib#progressbar#Display(value, ...) "{{{3
    TVarArg 'extra', ['always', 0]
    let ts = localtime()
    if !always && ts == s:timestamp
        return
    else
        let s:timestamp = ts
    endif
    let val = a:value * s:width[0] / s:max[0]
    if always || val != s:value[0]
        let s:value[0] = val
        let pbl = repeat('#', val)
        let pbr = repeat('.', s:width[0] - val)
        let txt = printf(s:format[0], '['.pbl.pbr.']') . extra
        let &statusline = txt
        " TLogDBG txt
        redrawstatus
        " redraw
        " call tlib#notify#Echo(txt)
    endif
endf


