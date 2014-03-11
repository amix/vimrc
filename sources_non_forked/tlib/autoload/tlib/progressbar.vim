" progressbar.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-30.
" @Last Change: 2010-01-07.
" @Revision:    0.0.69

if &cp || exists("loaded_tlib_progressbar_autoload")
    finish
endif
let loaded_tlib_progressbar_autoload = 1

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
    call insert(s:statusline, &statusline)
    call insert(s:laststatus, &laststatus)
    call insert(s:max, a:max)
    call insert(s:format, format)
    call insert(s:width, width)
    call insert(s:value, -1)
    let &laststatus = 2
    let s:timestamp = localtime()
endf


function! tlib#progressbar#Display(value, ...) "{{{3
    TVarArg 'extra'
    let ts = localtime()
    if ts == s:timestamp
        return
    else
        let s:timestamp = ts
    endif
    let val = a:value * s:width[0] / s:max[0]
    if val != s:value[0]
        let s:value[0] = val
        let pbl = repeat('#', val)
        let pbr = repeat('.', s:width[0] - val)
        let txt = printf(s:format[0], '['.pbl.pbr.']') . extra
        let &l:statusline = txt
        " TLogDBG txt
        redrawstatus
        " redraw
        " call tlib#notify#Echo(txt)
    endif
endf


function! tlib#progressbar#Restore() "{{{3
    let &l:statusline = remove(s:statusline, 0)
    let &laststatus = remove(s:laststatus, 0)
    redrawstatus
    " redraw
    " echo
    call remove(s:max, 0)
    call remove(s:format, 0)
    call remove(s:width, 0)
    call remove(s:value, 0)
endf


