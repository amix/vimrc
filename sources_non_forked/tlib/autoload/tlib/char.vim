" char.vim
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-06-30.
" @Last Change: 2009-02-15.
" @Revision:    0.0.30

if &cp || exists("loaded_tlib_char_autoload")
    finish
endif
let loaded_tlib_char_autoload = 1


" :def: function! tlib#char#Get(?timeout=0)
" Get a character.
"
" EXAMPLES: >
"   echo tlib#char#Get()
"   echo tlib#char#Get(5)
function! tlib#char#Get(...) "{{{3
    TVarArg ['timeout', 0], ['resolution', 0]
    if timeout == 0 || !has('reltime')
        return getchar()
    else
        return tlib#char#GetWithTimeout(timeout, resolution)
    endif
    return -1
endf


function! tlib#char#IsAvailable() "{{{3
    let ch = getchar(1)
    return type(ch) == 0 && ch != 0
endf


function! tlib#char#GetWithTimeout(timeout, ...) "{{{3
    TVarArg ['resolution', 2]
    " TLogVAR a:timeout, resolution
    let start = tlib#time#MSecs()
    while 1
        let c = getchar(0)
        if type(c) != 0 || c != 0
            return c
        else
            let now = tlib#time#MSecs()
            let diff = tlib#time#DiffMSecs(now, start, resolution)
            " TLogVAR diff
            if diff > a:timeout
                return -1
            endif
        endif
    endwh
    return -1
endf


