" notify.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-09-19.
" @Last Change: 2017-09-28.
" @Revision:    3.3.19

let s:save_cpo = &cpo
set cpo&vim


" :display: tlib#notify#Echo(text, ?style='')
" Print text in the echo area. Temporarily disable 'ruler' and 'showcmd' 
" in order to prevent |press-enter| messages.
function! tlib#notify#Echo(text, ...)
    TVarArg 'style'
    let ruler = &ruler
    let showcmd = &showcmd
    let text = substitute(a:text, '\n', '|', 'g')
    try
        set noruler
        set noshowcmd
        if !empty(style)
            exec 'echohl' style
        endif
        echo tlib#string#Strcharpart(text, 0, &columns - 1)
    finally
        if !empty(style)
            echohl None
        endif
        let &ruler = ruler
        let &showcmd = showcmd
    endtry
endf


" Contributed by Erik Falor:
" If the line containing the message is too long, echoing it will cause 
" a 'Hit ENTER' prompt to appear.  This function cleans up the line so 
" that does not happen.
" The echoed line is too long if it is wider than the width of the 
" window, minus cmdline space taken up by the ruler and showcmd 
" features.
function! tlib#notify#TrimMessage(message) "{{{3
    let filler = '...'

    " If length of message with tabs converted into spaces + length of 
    " line number + 2 (for the ': ' that follows the line number) is 
    " greater than the width of the screen, truncate in the middle
    let to_fill = &columns
    " TLogVAR to_fill

    " Account for space used by elements in the command-line to avoid 
    " 'Hit ENTER' prompts.
    " If showcmd is on, it will take up 12 columns.
    " If the ruler is enabled, but not displayed in the statusline, it 
    " will in its default form take 17 columns.  If the user defines a 
    " custom &rulerformat, they will need to specify how wide it is.
    if has('cmdline_info')
        if &showcmd
            let to_fill -= 12
        else
            let to_fill -= 1
        endif
        " TLogVAR &showcmd, to_fill

        " TLogVAR &laststatus, &ruler, &rulerformat
        if &ruler
            if &laststatus == 0 || winnr('$') == 1
                if has('statusline')
                    if &rulerformat == ''
                        " default ruler is 17 chars wide
                        let to_fill -= 17
                    elseif exists('g:MP_rulerwidth')
                        let to_fill -= g:MP_rulerwidth
                    else
                        " tml: fallback: guess length
                        let to_fill -= strlen(&rulerformat)
                    endif
                else
                endif
            endif
        else
        endif
    else
        let to_fill -= 1
    endif

    " TLogVAR to_fill
    " TLogDBG strlen(a:message)
    if strlen(a:message) > to_fill
        let front = to_fill / 2 - 1
        let back  = front 
        if to_fill % 2 == 0 | let back -= 1 | endif
        return tlib#string#Strcharpart(a:message, 0, front) . filler .
                    \ tlib#string#Strcharpart(a:message, strlen(a:message) - back)
    else
        return a:message
    endif
endfunction


function! tlib#notify#PrintError() abort "{{{3
    echohl ErrorMsg
    echom v:exception
    echom v:throwpoint
    echohl NONE
endf


let &cpo = s:save_cpo
unlet s:save_cpo
