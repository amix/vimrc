" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    84


if !exists('g:tlib#win#use_winid')
    let g:tlib#win#use_winid = exists('*win_gotoid') && exists('*win_getid')   "{{{2
endif


" Return vim code to jump back to the original window.
function! tlib#win#Set(winnr) "{{{3
    if a:winnr > 0
        " TLogVAR a:winnr
        " TLogDBG winnr()
        " TLogDBG string(tlib#win#List())
        if winnr() != a:winnr && winbufnr(a:winnr) != -1
            let rv = winnr().'wincmd w'
            exec a:winnr .'wincmd w'
            " TLogVAR rv
            " TLogDBG string(tlib#win#List())
            return rv
        endif
    endif
    return ''
endf


if g:tlib#win#use_winid
    let g:tlib#win#null_id = -1
    function! tlib#win#GetID() abort "{{{3
        return win_getid()
    endf
    function! tlib#win#GotoID(win_id) abort "{{{3
        call win_gotoid(a:win_id)
    endf
else
    let s:win_id = 0
    let g:tlib#win#null_id = {}
    function! tlib#win#GetID() abort "{{{3
        if !exists('w:tlib_win_id')
            let s:win_id += 1
            let w:tlib_win_id = s:win_id
        endif
        return {'tabpagenr': tabpagenr(), 'bufnr': bufnr('%'), 'winnr': winnr(), 'win_id': w:tlib_win_id}
    endf
    function! tlib#win#GotoID(win_id) abort "{{{3
        Tlibtrace 'tlib', a:win_id
        if tabpagenr() != a:win_id.tabpagenr
            exec 'tabnext' a:win_id.tabpagenr
        endif
        for wnr in range(1, winnr('$'))
            let win_id = getwinvar(wnr, 'tlib_win_id', -1)
            Tlibtrace 'tlib', wnr, win_id
            if win_id == a:win_id.win_id
                Tlibtrace 'tlib', wnr
                exec wnr 'wincmd w'
                return
            endif
        endfor
        " Was the window closed? What should we do now?
        if winnr() != a:win_id.winnr
            exec a:win_id.winnr 'wincmd w'
        endif
        if bufnr('%') != a:win_id.bufnr
            exec 'hide buffer' a:win_id.bufnr
        endif
    endf
endif


" Return vim code to jump back to the original window.
function! tlib#win#SetById(win_id) "{{{3
    if a:win_id != g:tlib#win#null_id
        let win_id = tlib#win#GetID()
        call tlib#win#GotoID(a:win_id)
        return printf('call tlib#win#GotoID(%s)', win_id)
        " " TLogVAR a:winnr
        " " TLogDBG winnr()
        " " TLogDBG string(tlib#win#List())
        " if winnr() != a:winnr && winbufnr(a:winnr) != -1
        "     let rv = winnr().'wincmd w'
        "     exec a:winnr .'wincmd w'
        "     " TLogVAR rv
        "     " TLogDBG string(tlib#win#List())
        "     return rv
        " endif
    endif
    return ''
endf
 

" :def: function! tlib#win#GetLayout(?save_view=0)
function! tlib#win#GetLayout(...) "{{{3
    TVarArg ['save_view', 0]
    let views = {}
    if save_view
        let winnr = winnr()
        windo let views[winnr()] = winsaveview()
        " for w in range(1, winnr('$'))
        "     call tlib#win#Set(w)
        "     let views[w] = winsaveview()
        " endfor
        call tlib#win#Set(winnr)
    endif
    return {'winnr': winnr('$'), 'winrestcmd': winrestcmd(), 'views': views, 'cmdheight': &cmdheight, 'guioptions': &guioptions, 'tabpagenr': tabpagenr()}
endf


function! tlib#win#SetLayout(layout) "{{{3
    if a:layout.tabpagenr == tabpagenr() && a:layout.winnr == winnr('$')
        " TLogVAR a:layout.winrestcmd
        " TLogDBG string(tlib#win#List())
        exec a:layout.winrestcmd
        if !empty(a:layout.views)
            let winnr = winnr()
            " TLogVAR winnr
            for [w, v] in items(a:layout.views)
                " TLogVAR w, v
                call tlib#win#Set(w)
                call winrestview(v)
            endfor
            call tlib#win#Set(winnr)
        endif
        if a:layout.cmdheight != &cmdheight
            let &cmdheight = a:layout.cmdheight
        endif
        " TLogDBG string(tlib#win#List())
        return 1
    endif
    return 0
endf


function! tlib#win#List() "{{{3
    let wl = {}
    for wn in range(1, winnr('$'))
        let wl[wn] = bufname(winbufnr(wn))
    endfor
    return wl
endf


" " :def: function! tlib#win#GetLayout1(?save_view=0)
" " Contrary to |tlib#win#GetLayout|, this version doesn't use 
" " |winrestcmd()|. It can also save windows views.
" function! tlib#win#GetLayout1(...) "{{{3
"     TVarArg ['save_view', 0]
"     let winnr = winnr()
"     let acc = {}
"     for w in range(1, winnr('$'))
"         let def = {'h': winheight(w), 'w': winwidth(w)}
"         if save_view
"             call tlib#win#Set(w)
"             let def.view = winsaveview()
"         endif
"         let acc[w] = def
"     endfor
"     call tlib#win#Set(winnr)
"     return acc
" endf
" 
" 
" " Reset layout from the value of |tlib#win#GetLayout1|.
" function! tlib#win#SetLayout1(layout) "{{{3
"     if len(a:layout) != winnr('$')
"         return 0
"     endif
"     let winnr = winnr()
"     for [w, def] in items(a:layout)
"         if tlib#win#Set(w)
"             exec 'resize '. def.h
"             exec 'vertical resize '. def.w
"             if has_key(def, 'view')
"                 call winrestview(def.view)
"             endif
"         else
"             break
"         endif
"     endfor
"     call tlib#win#Set(winnr)
"     return 1
" endf


function! tlib#win#Width(wnr) "{{{3
    return winwidth(a:wnr) - &fdc
endf


function! tlib#win#WinDo(ex) "{{{3
    let w = winnr()
    exec 'windo '. a:ex
    exec w .'wincmd w'
endf

