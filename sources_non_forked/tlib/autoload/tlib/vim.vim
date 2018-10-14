" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @GIT:         http://github.com/tomtom/tlib_vim/
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-07-19.
" @Last Change: 2012-06-08.
" @Revision:    37


let s:restoreframecmd = ''
let s:fullscreen = 0

if has('win16') || has('win32') || has('win64')

    if !exists('g:tlib#vim#simalt_maximize')
        " The alt-key for maximizing the window.
        " CAUTION: The value of this paramter depends on your locale and 
        " maybe the windows version you are running.
        let g:tlib#vim#simalt_maximize = 'x'   "{{{2
    endif

    if !exists('g:tlib#vim#simalt_restore')
        " The alt-key for restoring the window.
        " CAUTION: The value of this paramter depends on your locale and 
        " maybe the windows version you are running.
        let g:tlib#vim#simalt_restore = 'r'   "{{{2
    endif

    if !exists('g:tlib#vim#use_vimtweak')
        " If true, use the vimtweak.dll for windows. This will enable 
        " tlib to remove the caption for fullscreen windows.
        let g:tlib#vim#use_vimtweak = 0   "{{{2
    endif

    " Maximize the window.
    " You might need to redefine |g:tlib#vim#simalt_maximize| if it doesn't 
    " work for you.
    fun! tlib#vim#Maximize(fullscreen) "{{{3
        if !has("gui_running")
            return
        endif
        call s:SaveFrameParams()
        let s:fullscreen = a:fullscreen
        if g:tlib#vim#use_vimtweak && a:fullscreen
            call libcallnr("vimtweak.dll", "EnableCaption", 0)
        endif
        exec 'simalt ~'. g:tlib#vim#simalt_maximize
    endf

    " Restore the original vimsize after having called |tlib#vim#Maximize()|.
    function! tlib#vim#RestoreWindow() "{{{3
        if !has("gui_running")
            return
        endif
        if g:tlib#vim#use_vimtweak
            call libcallnr("vimtweak.dll", "EnableCaption", 1)
        endif
        exec 'simalt ~'. g:tlib#vim#simalt_restore
        call s:RestoreFrameParams()
    endf

else

    if !exists('g:tlib#vim#use_wmctrl')
        " If true, use wmctrl for X windows to make a window 
        " maximized/fullscreen.
        "
        " This is the preferred method for maximizing windows under X 
        " windows. Some window managers have problem coping with the 
        " default method of setting 'lines' and 'columns' to a large 
        " value.
        let g:tlib#vim#use_wmctrl = executable('wmctrl')  "{{{2
    endif

    " :nodoc:
    fun! tlib#vim#Maximize(fullscreen) "{{{3
        if !has("gui_running")
            return
        endif
        call s:SaveFrameParams()
        let s:fullscreen = a:fullscreen
        if g:tlib#vim#use_wmctrl
            if a:fullscreen
                silent !wmctrl -r :ACTIVE: -b add,fullscreen
            else
                silent !wmctrl -r :ACTIVE: -b add,maximized_vert,maximized_horz
            endif
        else
            set lines=1000 columns=1000 
        endif
    endf

    " :nodoc:
    function! tlib#vim#RestoreWindow() "{{{3
        if !has("gui_running")
            return
        endif
        if g:tlib#vim#use_wmctrl
            if s:fullscreen
                silent !wmctrl -r :ACTIVE: -b remove,fullscreen
            else
                silent !wmctrl -r :ACTIVE: -b remove,maximized_vert,maximized_horz
            endif
        endif
        call s:RestoreFrameParams()
    endf

endif


function! s:SaveFrameParams() "{{{3
    let s:restoreframecmd = printf("set lines=%d columns=%d | winpos %d %d", &lines, &columns, getwinposx(), getwinposy())
endf


function! s:RestoreFrameParams() "{{{3
    if !empty(s:restoreframecmd)
        exec s:restoreframecmd
        let s:restoreframecmd = ''
    endif
endf


" :display: tlib#vim##CopyFunction(old, new, overwrite=0)
function! tlib#vim#CopyFunction(old, new, ...) "{{{3
    let overwrite = a:0 >= 1 ? a:1 : 0
    redir => oldfn
    exec 'silent function' a:old
    redir END
    if exists('*'. a:new)
        if overwrite > 0
            exec 'delfunction' a:new
        elseif overwrite < 0
            throw 'tlib#vim##CopyFunction: Function already exists: '. a:old .' -> '. a:new
        else
            return
        endif
    endif
    let fn = split(oldfn, '\n')
    let fn = map(fn, 'substitute(v:val, ''^\d\+'', "", "")')
    let fn[0] = substitute(fn[0], '\V\^\s\*fu\%[nction]!\?\s\+\zs'. a:old, a:new, '')
    let t = @t
    try
        let @t = join(fn, "\n")
        redir => out
        @t
        redir END
    finally
        let @t = t
    endtry
endf

