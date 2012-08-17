" paragraph.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2009-10-26.
" @Last Change: 2011-04-03.
" @Revision:    62

let s:save_cpo = &cpo
set cpo&vim


" Return an object describing a |paragraph|.
function! tlib#paragraph#GetMetric() "{{{3
    let sp = {'text_start': line("'{") + 1}
    if line("'}") == line("$")
        let sp.last = 1
        let sp.text_end = line("'}")
        if line("'{") == 1
            let sp.ws_start = 0
            let sp.ws_end = 0
            let sp.top = sp.text_start
            let sp.bottom = sp.text_end
        else
            let sp.ws_start = prevnonblank(line("'{")) + 1
            let sp.ws_end = line("'{")
            let sp.top = sp.ws_start
            let sp.bottom = sp.text_end
        endif
    else
        let sp.last = 0
        let sp.text_end = line("'}") - 1
        let sp.ws_start = line("'}")
        for i in range(line("'}"), line('$'))
            if getline(i) =~ '\w'
                let sp.ws_end = i - 1
                break
            elseif i == line("$")
                let sp.ws_end = i
            endif
        endfor
        let sp.top = sp.text_start
        let sp.bottom = sp.ws_end
    endif
    return sp
endf


" This function can be used with the tinymode plugin to move around 
" paragraphs.
"
" Example configuration: >
" 
"   call tinymode#EnterMap("para_move", "gp")
"   call tinymode#ModeMsg("para_move", "Move paragraph: j/k")
"   call tinymode#Map("para_move", "j", "silent call tlib#paragraph#Move('Down', '[N]')")
"   call tinymode#Map("para_move", "k", "silent call tlib#paragraph#Move('Up', '[N]')")
"   call tinymode#ModeArg("para_move", "owncount", 1)
function! tlib#paragraph#Move(direction, count)
    " TLogVAR a:direction, a:count
    let mycount = empty(a:count) ? 1 : a:count
    for i in range(1, mycount)
        let para = tlib#paragraph#GetMetric()
        " TLogVAR para
        let text = getline(para.text_start, para.text_end)
        let ws = getline(para.ws_start, para.ws_end)
        " TLogVAR text, ws
        exec para.top .','. para.bottom .'delete'
        if a:direction == "Down"
            let other = tlib#paragraph#GetMetric()
            let target = other.bottom + 1
            if other.last
                let lines = ws + text
                let pos = target + len(ws)
            else
                let lines = text + ws
                let pos = target
            endif
        elseif a:direction == "Up"
            if !para.last
                norm! {
            endif
            let other = tlib#paragraph#GetMetric()
            let target = other.text_start
            let lines = text + ws
            let pos = target
        endif
        " TLogVAR other, target
        " TLogVAR lines
        call append(target - 1, lines)
        exec pos
    endfor
endf


let &cpo = s:save_cpo
unlet s:save_cpo
