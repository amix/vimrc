"   Copyright: Copyright (C) 2005 Marius Groleo
"              Permission is hereby granted to use and distribute this code,
"              with or without modifications, provided that  this  copyright
"              notice is copied with it. Like anything else that's free,
"              tabbar.vim is provided *AS IS* and comes with no warranty of
"              any kind, either expressed or implied. In no event will the
"              copyright holder be liable for any damamges resulting from the
"              use of this software.
"
"              Derived from  Bindu Wavell miniBufExplorer.vim version 6.3.2


"   Version:      0.7
"   Maintainer:   Marius Groleo < groleo@gmail.com >
"   Description:  TabBar buffer explorer Vim Plugin
"   Name Of File: tabbar.vim

"   DOCUMENTATION: is at line :1445

" press zR , in normal mode to OPEN  all folds
" press zM , in normal mode to CLOSE all folds

" Already been loaded? ~~
if exists('Tb_loaded')
    finish
else
      let Tb_loaded= 1
endif "%%


" Debug Level ~~
"   0 = no logging
" 1-5 = errors ; 1 is the most important
" 5-9 = info ; 5 is the most important
"  10 = Entry/Exit
if !exists('g:Tb_DBG_LVL')
      let g:Tb_DBG_LVL = 0
endif" %%


" Logging method ~~
" 0 = log to a window
" 1 = log with vim's echo facility
" 2 = log to a file named TabBar.DBG
"     in the directory where vim was started
"     THIS IS VERY SLOW
" 3 = log into g:Tb_DbgOutput
"     global variable [This is the default]
if !exists('g:Tb_DebugMode')
      let g:Tb_DebugMode = 0
endif" %%


" Mappings and Commands
" TabBar Keyboard Mappings ~~
if ! hasmapto('1') || !hasmapto('<M-1>')
      "gui bindings containing META key, are different from terminal bindings
      if has('gui_running')
            "NORMAL mode bindings for gvim
            noremap <unique> <script> <M-1> :call <SID>Bf_SwitchTo( 1)<CR>:<BS>
            noremap <unique> <script> <M-2> :call <SID>Bf_SwitchTo( 2)<CR>:<BS>
            noremap <unique> <script> <M-3> :call <SID>Bf_SwitchTo( 3)<CR>:<BS>
            noremap <unique> <script> <M-4> :call <SID>Bf_SwitchTo( 4)<CR>:<BS>
            noremap <unique> <script> <M-5> :call <SID>Bf_SwitchTo( 5)<CR>:<BS>
            noremap <unique> <script> <M-6> :call <SID>Bf_SwitchTo( 6)<CR>:<BS>
            noremap <unique> <script> <M-7> :call <SID>Bf_SwitchTo( 7)<CR>:<BS>
            noremap <unique> <script> <M-8> :call <SID>Bf_SwitchTo( 8)<CR>:<BS>
            noremap <unique> <script> <M-9> :call <SID>Bf_SwitchTo( 9)<CR>:<BS>
            noremap <unique> <script> <M-0> :call <SID>Bf_SwitchTo( 10)<CR>:<BS>
            "INSERT mode bindings for gvim
            inoremap <unique> <script> <M-1> <esc>:call <SID>Bf_SwitchTo( 1)<CR>:<BS>a
            inoremap <unique> <script> <M-2> <esc>:call <SID>Bf_SwitchTo( 2)<CR>:<BS>a
            inoremap <unique> <script> <M-3> <esc>:call <SID>Bf_SwitchTo( 3)<CR>:<BS>a
            inoremap <unique> <script> <M-4> <esc>:call <SID>Bf_SwitchTo( 4)<CR>:<BS>a
            inoremap <unique> <script> <M-5> <esc>:call <SID>Bf_SwitchTo( 5)<CR>:<BS>a
            inoremap <unique> <script> <M-6> <esc>:call <SID>Bf_SwitchTo( 6)<CR>:<BS>a
            inoremap <unique> <script> <M-7> <esc>:call <SID>Bf_SwitchTo( 7)<CR>:<BS>a
            inoremap <unique> <script> <M-8> <esc>:call <SID>Bf_SwitchTo( 8)<CR>:<BS>a
            inoremap <unique> <script> <M-9> <esc>:call <SID>Bf_SwitchTo( 9)<CR>:<BS>a
            inoremap <unique> <script> <M-0> <esc>:call <SID>Bf_SwitchTo( 10)<CR>:<BS>a
      else
            "NORMAL mode bindings for vim( dos32 )
            noremap <unique> <script> ± :call <SID>Bf_SwitchTo( 1)<CR>:<BS>
            noremap <unique> <script> ² :call <SID>Bf_SwitchTo( 2)<CR>:<BS>
            noremap <unique> <script> ³ :call <SID>Bf_SwitchTo( 3)<CR>:<BS>
            noremap <unique> <script> ´ :call <SID>Bf_SwitchTo( 4)<CR>:<BS>
            noremap <unique> <script> µ :call <SID>Bf_SwitchTo( 5)<CR>:<BS>
            noremap <unique> <script> ¶ :call <SID>Bf_SwitchTo( 6)<CR>:<BS>
            noremap <unique> <script> · :call <SID>Bf_SwitchTo( 7)<CR>:<BS>
            noremap <unique> <script> ¸ :call <SID>Bf_SwitchTo( 8)<CR>:<BS>
      "else
            "NORMAL mode bindings for vim( terminal)
            noremap <unique> <script> 1 :call <SID>Bf_SwitchTo( 1)<CR>:<BS>
            noremap <unique> <script> 2 :call <SID>Bf_SwitchTo( 2)<CR>:<BS>
            noremap <unique> <script> 3 :call <SID>Bf_SwitchTo( 3)<CR>:<BS>
            noremap <unique> <script> 4 :call <SID>Bf_SwitchTo( 4)<CR>:<BS>
            noremap <unique> <script> 5 :call <SID>Bf_SwitchTo( 5)<CR>:<BS>
            noremap <unique> <script> 6 :call <SID>Bf_SwitchTo( 6)<CR>:<BS>
            noremap <unique> <script> 7 :call <SID>Bf_SwitchTo( 7)<CR>:<BS>
            noremap <unique> <script> 8 :call <SID>Bf_SwitchTo( 8)<CR>:<BS>
            noremap <unique> <script> 9 :call <SID>Bf_SwitchTo( 9)<CR>:<BS>
            noremap <unique> <script> 0 :call <SID>Bf_SwitchTo( 10)<CR>:<BS>
            "INSERT mode bindings for vim( terminal)
            inoremap <unique> <script> 1 <esc>:call <SID>Bf_SwitchTo( 1)<CR>:<BS>a
            inoremap <unique> <script> 2 <esc>:call <SID>Bf_SwitchTo( 2)<CR>:<BS>a
            inoremap <unique> <script> 3 <esc>:call <SID>Bf_SwitchTo( 3)<CR>:<BS>a
            inoremap <unique> <script> 4 <esc>:call <SID>Bf_SwitchTo( 4)<CR>:<BS>a
            inoremap <unique> <script> 5 <esc>:call <SID>Bf_SwitchTo( 5)<CR>:<BS>a
            inoremap <unique> <script> 6 <esc>:call <SID>Bf_SwitchTo( 6)<CR>:<BS>a
            inoremap <unique> <script> 7 <esc>:call <SID>Bf_SwitchTo( 7)<CR>:<BS>a
            inoremap <unique> <script> 8 <esc>:call <SID>Bf_SwitchTo( 8)<CR>:<BS>a
            inoremap <unique> <script> 9 <esc>:call <SID>Bf_SwitchTo( 9)<CR>:<BS>a
            inoremap <unique> <script> 0 <esc>:call <SID>Bf_SwitchTo( 10)<CR>:<BS>a
      endif
endif " %%


" TabBar <Script> internal map ~~
noremap <unique> <script> <Plug>tbstart  :call <SID>Tb_Start(1, -1)<CR>:<BS>
noremap <unique> <script> <Plug>tbstop   :call <SID>Tb_Stop(1)<CR>:<BS>
noremap <unique> <script> <Plug>tbaut    :call <SID>Tb_Aup(-1)<CR>:<BS>
noremap <unique> <script> <Plug>tbtoggle :call <SID>Tb_Toggle()<CR>:<BS>
" %%


" TabBar commands ~~
if !exists(':TbStart')
      command! TbStart  call <SID>Tb_Start(1, -1)
endif

if !exists(':TbStop')
      command! TbStop  call <SID>Tb_Stop(1)
endif

if !exists(':TbAup')
      command! TbAup  call <SID>Tb_AutoUpdt(-1)
endif

if !exists(':TbToggle')
      command! TbToggle  call <SID>Tb_Toggle()
endif

if !exists(':Tbbn')
      command! Tbbn call <SID>Bf_Cycle(1)
endif
if !exists(':Tbp')
      command! Tbbp call <SID>Bf_Cycle(0)
endif " %%



" Global Configuration Variables
" Allow auto update? ~~
" We start out with this off for startup, but once vim is running we
" turn this on.
if !exists('g:Tb_AutoUpdt')
      let g:Tb_AutoUpdt = 0
endif " %%


" MoreThanOne? ~~
" Display Mini Buf Explorer when there are 'More Than One' eligible buffers
if !exists('g:Tb_MoreThanOne')
      let g:Tb_MoreThanOne = 2
endif" %%


" Split below/above/left/right? ~~
" When opening a new -TabBar- window, split the new windows below or
" above the current window?  1 = below, 0 = above.
if !exists('g:Tb_SplitBelow')
      let g:Tb_SplitBelow = &splitbelow
endif" %%


" Horizontal or Vertical explorer? ~~
" For folks that like vertical explorers, I'm caving in and providing for
" veritcal splits. If this is set to 0 then the current horizontal
" splitting logic will be run. If however you want a vertical split,
" assign the width (in characters) you wish to assign to the -TabBar- window.
if !exists('g:Tb_VSplit')
      let g:Tb_VSplit = 0
endif" %%


" TabWrap? ~~
" By default line wrap is used (possibly breaking a tab name between two
" lines.) Turning this option on (setting it to 1) can take more screen
" space, but will make sure that each tab is on one and only one line.
if !exists('g:Tb_TabWrap')
    let g:Tb_TabWrap = 0
endif" %%


" Switch buffers using Ctrl-Tab ?~~
if !exists('g:Tb_cTabSwitchBufs')
    let g:Tb_cTabSwitchBufs = 1
endif" %%


" if cTabSwitchBufs is turned on then we turn off cTabSwitchWindows.~~
if g:Tb_cTabSwitchBufs == 1 || !exists('g:Tb_cTabSwitchWindows')
      let g:Tb_cTabSwitchWindows = 0
endif" %%


" If we have enabled <C-TAB> and <C-S-TAB> to switch buffers~~
" in the current window then perform the remapping
if g:Tb_cTabSwitchBufs
    noremap <C-TAB>   :call <SID>Bf_Cycle(1)<CR>:<BS>
    noremap <C-S-TAB> :call <SID>Bf_Cycle(0)<CR>:<BS>
endif "%%


" If we have enabled <C-TAB> and <C-S-TAB> to switch windows ~~
" then perform the remapping
if g:Tb_cTabSwitchWindows
    noremap <C-TAB>   <C-W>w
    noremap <C-S-TAB> <C-W>W
endif "%%


" Modifiable Select Target ~~
if !exists('g:Tb_ModSelTarget')
      let g:Tb_ModSelTarget = 0
endif "%%


" Force Syntax Enable ~~
if !exists('g:Tb_ForceSyntaxEnable')
      let g:Tb_ForceSyntaxEnable = 0
endif "%%


" Single/Double Click? ~~
" flag that can be set to 1 in a users .vimrc to allow
" single click switching of tabs. By default we use
" double click for tab selection.
if !exists('g:Tb_UseSingleClick')
      let g:Tb_UseSingleClick = 0
endif

"
" attempt to perform single click mapping, it would be much
" nicer if we could nnoremap <buffer> ... however vim does
" not fire the <buffer> <leftmouse> when you use the mouse
" to enter a buffer.
"
if g:Tb_UseSingleClick == 1
    let s:clickmap = ':if bufname("%") == "-TabBar-" <bar> call <SID>Tb_Click() <bar> endif <CR>'
    if maparg('<LEFTMOUSE>', 'n') == ''
        " no mapping for leftmouse
        exec ':nnoremap <silent> <LEFTMOUSE> <LEFTMOUSE>' . s:clickmap
    else
        " we have a mapping
        let  g:Tb_DoneClickSave = 1
        let  s:m = ':nnoremap <silent> <LEFTMOUSE> <LEFTMOUSE>'
        let  s:m = s:m . substitute(substitute(maparg('<LEFTMOUSE>', 'n'), '|', '<bar>', 'g'), '\c^<LEFTMOUSE>', '', '')
        let  s:m = s:m . s:clickmap
        exec s:m
    endif
endif " %%



"------------"
" Variables  "
"------------"
" used to pass maxTabWidth info between functions
let s:maxTabWidth = 0


" Debug line counter
let s:DBG_LN_CNT = 0


" Global used to store the buffer list so we don't update the ~~
" UI unless the list has changed.
if !exists('g:Tb_VimBufList')
    let g:Tb_VimBufList = ''
    let g:Tb_BufferMap=''
endif "%%


" g:Tb_UpdtMutex: Variable used as a mutex so that we don't do lots~~
" of Tb_AutoUpdts at the same time.
if !exists('g:Tb_UpdtMutex')
    let g:Tb_UpdtMutex = 0
endif "%%


" g:Tb_MaxHeight maxSize ~~
if !exists('g:Tb_MaxSize')
      let g:Tb_MaxSize = 1
      "TODO implement Tb_MaxHeight
endif "%%


" g:Tb_MaxHeight MinSize ~~
if !exists('g:Tb_MinSize')
      let g:Tb_MinSize = 1
      "TODO implement Tb_MaxHeight
endif "%%


" g:Tb_DbgOutput: In debug mode 3 this variable will hold the debug output~~
if !exists('g:Tb_DbgOutput')
      let g:Tb_DbgOutput = ''
endif "%%


" g:Tb_ForceDisplay ~~
if !exists('g:Tb_ForceDisplay')
      let g:Tb_ForceDisplay = 0
endif "%%


" Setup an autocommand group and some autocommands ~~
" that keep our explorer updated automatically.
augroup TabBar
autocmd TabBar BufDelete   * call <SID>DEBUG('-=> BufDelete AutoCmd', 10) |call <SID>Tb_AutoUpdt(expand('<abuf>'))
autocmd TabBar BufEnter    * call <SID>DEBUG('-=> BufEnter  AutoCmd', 10) |call <SID>Tb_AutoUpdt(-1)
autocmd TabBar VimEnter    * call <SID>DEBUG('-=> VimEnter  AutoCmd', 10) |let g:Tb_AutoUpdt = 1 |call <SID>Tb_AutoUpdt(-1)
" %%


"------------"
" Functions  "
"------------"
" Tb_Start - Sets up our explorer and causes it to be displayed ~~
function! <SID>Tb_Start(sticky, delBufNum)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Tb_Start()'   ,10)
    endif

    if a:sticky == 1
        let g:Tb_AutoUpdt = 1
    endif

    " Store the current buffer
    let l:curBuf = bufnr('%')

    " Prevent a report of our actions from showing up.
    let l:save_rep = &report
    let l:save_sc  = &showcmd
    let &report    = 10000
    set noshowcmd

    call <SID>Win_FindOrCreate('-TabBar-', -1, 1)

    " Make sure we are in our window
    if bufname('%') != '-TabBar-'
        call <SID>DEBUG('Tb_Start: called in invalid window',1)
        let &report  = l:save_rep
        let &showcmd = l:save_sc
        return
    endif

    " !!! We may want to make the following optional -- Bindu
    " New windows don't cause all windows to be resized to equal sizes
    set noequalalways
    " !!! We may want to make the following optional -- Bindu
    " We don't want the mouse to change focus without a click
    set nomousefocus

    " If folks turn numbering and columns on by default we will turn
    " them off for the -TabBar- window
    setlocal foldcolumn=0
    setlocal nonumber

    if has("syntax")
        syn clear
        syn match Tb_Normal             '\[[^\]]*\]'
        syn match Tb_Changed            '\[[^\]]*\]+'
        syn match Tb_VisibleNormal      '\[[^\]]*\]\*+\='
        syn match Tb_VisibleChanged     '\[[^\]]*\]\*+'

        if !exists("g:did_tabbar_syntax_inits")
            let g:did_tabbar_syntax_inits = 1
            highlight def link Tb_Normal         Comment
            highlight def link Tb_Changed        String
            highlight def link Tb_VisibleNormal  StatusLineNC
            highlight def link Tb_VisibleChanged Special
        endif
    endif


    " If you press return in the -TabBar- then try
    " to open the selected buffer in the previous window.
    " Bf_CrSel = Buffer CR Select
    nnoremap <buffer> <CR> :call <SID>Bf_CrSel()<CR>:<BS>


    " If you Bf_DblClkSel in the -TabBar- then try
    " to open the selected buffer in the previous window.
    nnoremap <buffer> <2-LEFTMOUSE> :call <SID>Bf_DblClkSel()<CR>:<BS>


    " delete the selected buffer.
    nnoremap <buffer> d :call <SID>Bf_DelWithD()<CR>:<BS>


    " If you press w in the -TabBar- then switch back
    " to the previous window.
    nnoremap <buffer> p :wincmd p<CR>:<BS>

    " The following allows for quicker moving between buffer
    " names in the -TabBar- window it also saves the last-pattern
    " and restores it.
    nnoremap <buffer> <TAB>   :call search('\[[0-9]*:[^\]]*\]')<CR>:<BS>
    nnoremap <buffer> <S-TAB> :call search('\[[0-9]*:[^\]]*\]','b')<CR>:<BS>

    call <SID>Bf_SafePrint(a:delBufNum)

    if (l:curBuf != -1)
        call search('\['.l:curBuf.':'.expand('#'.l:curBuf.':t').'\]')
    else
        call <SID>DEBUG('Tb_Start: No current buffer to search for',9)
    endif

    let &report  = l:save_rep
    let &showcmd = l:save_sc

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Tb_Start()'  ,10)
    endif
endfunction " %%


" Tb_Stop - Looks for our explorer and closes the window if it is opened ~~
function! <SID>Tb_Stop( sticky)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Tb_Stop()'    ,10)
    endif

    if a:sticky == 1
        let g:Tb_AutoUpdt = 0
    endif

    let l:winNum = <SID>Win_Find('-TabBar-')

    if l:winNum != -1
        exec l:winNum.' wincmd w'
        silent! close
        wincmd p
    endif

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Tb_Stop()'   ,10)
    endif
endfunction " %%


" Tb_Toggle - Looks for our explorer and opens/closes the window ~~
function! <SID>Tb_Toggle()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Tb_Toggle()'  ,10)
    endif

    let g:Tb_AutoUpdt = 0

    let l:winNum = <SID>Win_Find('-TabBar-')

    if l:winNum != -1
        call <SID>Tb_Stop(1)
    else
        call <SID>Tb_Start(1, -1)
        wincmd p
    endif

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Tb_Toggle()' ,10)
    endif
endfunction " %%


" Tb_Max - Returns the max of two numbers ~~
function! <SID>Tb_Max(argOne, argTwo)
    if a:argOne > a:argTwo
        return a:argOne
    else
        return a:argTwo
    endif
endfunction " %%


" Tb_AutoUpdt - Function called by auto commands for auto updating -TabBar- ~~
"     IF auto update is turned on     AND
"     we are in a real buffer         AND
"     we have enough eligible buffers THEN
"     Update our explorer and get back to the current window
" If we get a buffer number for a buffer that
" is being deleted, we need to make sure and
" remove the buffer from the list of eligible
" buffers in case we are down to one eligible
" buffer, in which case we will want to close
" the -TabBar- window.
function! <SID>Tb_AutoUpdt(delBufNum)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Tb_AutoUpdt( delBufNum='.a:delBufNum.') : bufnr(%)='.bufnr('%').' : bufname(%)=['.bufname('%') . ']',10)
    endif

    if (g:Tb_UpdtMutex == 1)
        if g:Tb_DBG_LVL > 0
              call <SID>DEBUG('Tb_AutoUpdt: recursion stopped',9)
              call <SID>DEBUG('EXIT : Tb_AutoUpdt()'    ,10)
        endif
        return
    else
        let g:Tb_UpdtMutex = 1
    endif

    " Don't update the TabBar window
    if (bufname('%') == '-TabBar-')
        " If this is the only buffer left then toggle the buffer
        if (winbufnr(2) == -1)
            call <SID>Bf_Cycle(1)
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Tb_AutoUpdt: does not run for cycled windows', 9)
            endif
        else
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Tb_AutoUpdt: does not run for the -TabBar- window', 9)
            endif
        endif

        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Tb_AutoUpdt()'    ,10)
        endif

        let g:Tb_UpdtMutex = 0
        return
    endif

    if (a:delBufNum != -1)
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Tb_AutoUpdt: will make sure that buffer '.a:delBufNum.' is not included in the buffer list.', 5)
        endif
    endif

  " Only allow updates when the Tb_AutoUpdt flag is set
  " this allows us to stop updates on startup.
    if g:Tb_AutoUpdt == 1
        " Only show TabBar if we have a real buffer
        if ((g:Tb_MoreThanOne == 0) || (bufnr('%') != -1 && bufname('%') != ""))
            if <SID>Bf_Eligible(a:delBufNum) == 1
                " if we don't have a window then create one
                let l:bufnr = <SID>Win_Find('-TabBar-')
                if (l:bufnr == -1)
                        if g:Tb_DBG_LVL > 0
                            call <SID>DEBUG('Tb_AutoUpdt: About to call Tb_Start (Create -TabBar-)', 9)
                        endif
                        call <SID>Tb_Start(0, a:delBufNum)
                else
                    " otherwise only update the window if the contents have changed
                    let l:ListChanged = <SID>Bf_BuildList(a:delBufNum, 0)
                    if (l:ListChanged)
                        if g:Tb_DBG_LVL > 0
                            call <SID>DEBUG('Tb_AutoUpdt: About to call Tb_Start (Update -TabBar-)', 9)
                        endif
                        call <SID>Tb_Start(0, a:delBufNum)
                    endif
                endif

                " go back to the working buffer
                if (bufname('%') == '-TabBar-')
                    wincmd p
                endif
            else
                if g:Tb_DBG_LVL > 0
                    call <SID>DEBUG('Tb_AutoUpdt: Failed in eligible check', 9)
                endif
                call <SID>Tb_Stop(0)
            endif

            " VIM sometimes turns syntax highlighting off,
            " we can force it on, but this may cause weird
            " behavior so this is an optional hack to force
            " syntax back on when we enter a buffer
            if g:Tb_ForceSyntaxEnable
                if g:Tb_DBG_LVL > 0
                    call <SID>DEBUG('Tb_AutoUpdt: Enable Syntax', 9)
                endif
                exec 'syntax enable'
            endif
        else
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Tb_AutoUpdt: No buffers loaded...',9)
            endif
        endif
    else
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Tb_AutoUpdt are turned off',9)
        endif
    endif

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Tb_AutoUpdt()'     ,10)
    endif

let g:Tb_UpdtMutex = 0
endfunction " %%


" Tb_Click - Handle mouse double click ~~
function! <SID>Tb_Click()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Tb_Click()',10)
    endif
    call <SID>Bf_CrSel()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Tb_Click()',10)
    endif
endfunction " %%



"-------------------"
" Window operations "
"-------------------"
" Win_Find - Return the window number of a named buffer ~~
" If none is found then returns -1.
function! <SID>Win_Find(bufName)
    "if g:Tb_DBG_LVL > 0
    "    call <SID>DEBUG('ENTER: Win_Find()',10)
    "endif

    " Try to find an existing window that contains
    " our buffer.
    let l:bufNr = bufnr(a:bufName)
    if l:bufNr != -1
        "if g:Tb_DBG_LVL > 0
        "    call <SID>DEBUG('Found buffer ('.a:bufName.'): '.l:bufNr,9)
        "endif
        let l:bufWinNr = bufwinnr(l:bufNr)
    else
        let l:bufWinNr = -1
    endif
    return l:bufWinNr
endfunction " %%


" Win_FindOrCreate - Attempts to find a window for a named buffer. ~~
"
" If it is found then moves there. Otherwise creates a new window and
" configures it and moves there.
"
" forceEdge, -1 use defaults, 0 below, 1 above
" isExplorer, 0 no, 1 yes
" 0 no, 1 yes
function! <SID>Win_FindOrCreate(bufName, forceEdge, isExplorer)
  "if g:Tb_DBG_LVL > 0
  "  call <SID>DEBUG('ENTER: Win_FindOrCreate('.a:bufName.')',10)
  "endif

  " Save the user's split setting.
  let l:saveSplitBelow = &splitbelow

  " Set to our new values.
  let &splitbelow = g:Tb_SplitBelow

  " Try to find an existing explorer window
  let l:winNum = <SID>Win_Find(a:bufName)

  " If found goto the existing window, otherwise
  " split open a new window.
  if l:winNum != -1
"    if g:Tb_DBG_LVL > 0
"      call <SID>DEBUG('Found window ('.a:bufName.'): '.l:winNum,9)
"    endif
    exec l:winNum.' wincmd w'
    let l:winFound = 1
  else

        "if g:Tb_SplitToEdge == 1 || a:forceEdge >= 0
        if a:forceEdge >= 0

            let l:edge = &splitbelow
            if a:forceEdge >= 0
                let l:edge = a:forceEdge
            endif

            if l:edge
                if g:Tb_VSplit == 0
                exec 'bo sp '.a:bufName
                else
                exec 'bo vsp '.a:bufName
                endif
            else
                if g:Tb_VSplit == 0
                exec 'to sp '.a:bufName
                else
                exec 'to vsp '.a:bufName
                endif
            endif
        else
            if g:Tb_VSplit == 0
            exec 'sp '.a:bufName
            else
            " &splitbelow doesn't affect vertical splits
            " so we have to do this explicitly.. ugh.
            if &splitbelow
                exec 'rightb vsp '.a:bufName
            else
                exec 'vsp '.a:bufName
            endif
            endif
        endif

    let g:Tb_ForceDisplay = 1

    " Try to find an existing explorer window
    let l:winNum = <SID>Win_Find(a:bufName)
    if l:winNum != -1
      "if g:Tb_DBG_LVL > 0
      "  call <SID>DEBUG('Created and then found window ('.a:bufName.'): '.l:winNum,9)
      "endif
      exec l:winNum.' wincmd w'
    else
      "if g:Tb_DBG_LVL > 0
      "  call <SID>DEBUG('Win_FindOrCreate failed to create window ('.a:bufName.').',1)
      "endif
      return
    endif

    if a:isExplorer
      " Turn off the swapfile, set the buffer type so that it won't get written,
      " and so that it will get deleted when it gets hidden and turn on word wrap.
      setlocal noswapfile
      setlocal buftype=nofile
      setlocal bufhidden=delete
      if g:Tb_VSplit == 0
        setlocal wrap
      else
        setlocal nowrap
        exec('setlocal winwidth='.g:Tb_MinSize)
      endif
    endif

    "if g:Tb_DBG_LVL > 0
    "  call <SID>DEBUG('Window ('.a:bufName.') created: '.winnr(),9)
    "endif

  endif

  " Restore the user's split setting.
  let &splitbelow = l:saveSplitBelow

endfunction " %%


" Win_Resize - Set width/height of -TabBar- window ~~
"
" Makes sure we are in our explorer, then sets the height/width for our explorer
" window so that we can fit all of our information without taking extra lines.
function! <SID>Win_Resize()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Win_Resize()',10)
    endif

  " Make sure we are in our window
  if bufname('%') != '-TabBar-'
    call <SID>DEBUG('EXIT : Win_Resize called in invalid window',1)
    return
  endif

  let l:width  = winwidth('.')

  " Horizontal Resize
  if g:Tb_VSplit == 0

    if g:Tb_TabWrap == 0
      let l:length = strlen(getline('.'))
      let l:height = 0
      if (l:width == 0)
        let l:height = winheight('.')
      else
        let l:height = (l:length / l:width)
        " handle truncation from div
        if (l:length % l:width) != 0
          let l:height = l:height + 1
        endif
      endif
    else
      exec("setlocal textwidth=".l:width)
      normal gg
      normal gq}
      normal G
      let l:height = line('.')
      normal gg
    endif

    " enforce max window height
    if g:Tb_MaxSize != 0
      if g:Tb_MaxSize < l:height
        let l:height = g:Tb_MaxSize
      endif
    endif

    " enfore min window height
    if l:height < g:Tb_MinSize || l:height == 0
      let l:height = g:Tb_MinSize
    endif

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Win_Resize to '.l:height.' lines',9)
    endif
    exec('resize '.l:height)

  " Vertical Resize
  else

    if g:Tb_MaxSize != 0
      let l:newWidth = s:maxTabWidth
      if l:newWidth > g:Tb_MaxSize
          let l:newWidth = g:Tb_MaxSize
      endif
      if l:newWidth < g:Tb_MinSize
          let l:newWidth = g:Tb_MinSize
      endif
    else
      let l:newWidth = g:Tb_VSplit
    endif

    if l:width != l:newWidth
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Win_Resize to '.l:newWidth.' columns',9)
        endif
      exec('vertical resize '.l:newWidth)
    endif

  endif

endfunction " %%



"-------------------"
" Buffer operations "
"-------------------"
" Bf_Choosed - From the -TabBar- window, return the bufnum for buf under cursor ~~
" If we are in our explorer window then return the buffer number
" for the buffer under the cursor.
function! <SID>Bf_Choosed()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_Choosed()',10)
    endif

    " Make sure we are in our window
    if bufname('%') != '-TabBar-'
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_Choosed: called in invalid window',1)
        endif
        return -1
    endif

    let l:save_reg = @"
    let @" = ""
    normal ""yi[
    if @" != ""
        let l:retv = substitute(@",'\([0-9]*\):.*', '\1', '') + 0
        let @" = l:save_reg
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_Choosed: l:retv='.l:retv,5)
        endif
        return l:retv
    else
        let @" = l:save_reg
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_Choosed: return -1',1)
        endif
        return -1
    endif
endfunction " %%


" Bf_DelWithD - From the -TabBar- window, delete selected buffer from list ~~
" After making sure that we are in our explorer, This will delete the buffer
" under the cursor. If the buffer under the cursor is being displayed in a
" window, this routine will attempt to get different buffers into the
" windows that will be affected so that windows don't get removed.
function! <SID>Bf_DelWithD()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_DelWithD() g:Tb_VimBufList =['. g:Tb_VimBufList.'] g:Tb_BufferMap=['.g:Tb_BufferMap.']',10)
    endif

    " Make sure we are in our window
    if bufname('%') != '-TabBar-'
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_DelWithD not called in -TabBar-',1)
        endif
        return
    endif

    let l:selected_buf  =  <SID>Bf_Choosed()
    let l:selected_buf  =  <SID>Map_Get_key( l:selected_buf )
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('Bf_DelWithD: l:selected_buf=['.l:selected_buf.']',5)
    endif
    let l:selected_name = bufname(l:selected_buf +0 )
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('Bf_DelWithD: l:selBufName=['. l:selected_name.']',5)
    endif

    let l:curLine    = line('.')
    let l:curCol     = virtcol('.')
    "make the filename an option
    if l:selected_name == 'TabBar.DBG' && g:Tb_DBG_LVL > 0
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_DelWithD will not delete the debug window.',1)
        endif
        return
    endif

    let l:save_rep = &report
    let l:save_sc  = &showcmd
    let &report    = 10000
    set noshowcmd


    if l:selected_buf != -1 && l:selected_name != ""

        " Don't want auto updates while we are processing a delete
        " request.
        let l:saveTb_AutoUpdt = g:Tb_AutoUpdt
        let g:Tb_AutoUpdt = 0

        " Save previous window so that if we show a buffer after
        " deleting. The show will come up in the correct window.
        wincmd p
        let l:prevWin    = winnr()
        let l:prevWinBuf = winbufnr(winnr())

        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Bf_DelWithD: l:prevWin='.l:prevWin.' buffer in window: '.l:prevWinBuf,5)
            call <SID>DEBUG('Bf_DelWithD: l:selected_name=<'.l:selected_name.'>: l:selected_buf=['.l:selected_buf.']',5)
        endif

        " If buffer is being displayed in a window then
        " move window to a different buffer before
        " deleting this one.
        let l:winNum = (bufwinnr(l:selected_name) + 0)
        " while we have windows that contain our buffer
        while l:winNum != -1
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Bf_DelWithD: l:selected_buf='.l:selected_buf.' is being displayed in window: l:winNum='.l:winNum,5)
            endif

            " move to window that contains our selected buffer
            exec l:winNum.' wincmd w'

            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Bf_DelWithD: We are now in window: '.winnr().' which contains buffer: '.bufnr('%').' and should contain buffer: '.l:selected_buf,5)
            endif

            let l:origBuf = bufnr('%')
            call <SID>Bf_Cycle(1)
            let l:curBuf  = bufnr('%')

            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Bf_DelWithD: Window now contains buffer: '.bufnr('%').' which should not be: '.l:selected_buf,5)
            endif

            if l:origBuf == l:curBuf
                " we wrapped so we are going to have to delete a buffer
                " that is in an open window.
                let l:winNum = -1
            else
                " see if we have anymore windows with our selected buffer
                let l:winNum = (bufwinnr(l:selected_name) + 0)
            endif
        endwhile

        " Attempt to restore previous window
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Bf_DelWithD: Restoring previous window to: '.l:prevWin,5)
        endif
        exec l:prevWin.' wincmd w'

        " Try to get back to the -TabBar- window
        let l:winNum = bufwinnr(bufnr('-TabBar-'))
        if l:winNum != -1
            exec l:winNum.' wincmd w'
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Bf_DelWithD: Got to -TabBar- window: '.winnr(),5)
            endif
        else
            if g:Tb_DBG_LVL > 0
                call <SID>DEBUG('Bf_DelWithD: Unable to get to -TabBar- window',1)
            endif
        endif

        " Delete the buffer selected.
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Bf_DelWithD: About to delete buffer: '.l:selected_buf,5)
        endif
        " [displayed_buffer]-- [real_buffer] list
        "let l:vimBuf = <SID>Map_Get_key( l:selected_buf )
        exec('silent! bd '.l:selected_buf )

        let g:Tb_AutoUpdt = l:saveTb_AutoUpdt
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Bf_DelWithD: current window=: '.bufname('%'),5)
        endif
        call <SID>Bf_SafePrint(1)
        call cursor(l:curLine, l:curCol)
    endif

    let &report  = l:save_rep
    let &showcmd = l:save_sc

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_DelWithD() g:Tb_VimBufList =['. g:Tb_VimBufList.'] g:Tb_BufferMap=['.g:Tb_BufferMap.']',10)
    endif
endfunction " %%


" Bf_SafePrint - Wrapper for getting -TabBar- window shown ~~
"
" Makes sure we are in our explorer, then erases the current buffer and turns
" it into a mini buffer explorer window.
function! <SID>Bf_SafePrint(delBufNum)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_SafePrint()',10)
    endif

    " Make sure we are in our window
    if bufname('%') != '-TabBar-'
        call <SID>DEBUG('EXIT : Bf_SafePrint not called in -TabBar-',1)
        return
    endif

    " We need to be able to modify the buffer
    setlocal modifiable

    call <SID>Bf_PrintList(a:delBufNum)
    call <SID>Win_Resize()

    normal! zz

    " Prevent the buffer from being modified.
    setlocal nomodifiable
    set nobuflisted
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_SafePrint()',10)
    endif
endfunction " %%


" Bf_PrintList - Clear current buffer and put the -TabBar- text into it ~~
" Makes sure we are in our explorer, then adds a list of all modifiable
" buffers to the current buffer. Special marks are added for buffers that
" are in one or more windows (*) and buffers that have been modified (+)
function! <SID>Bf_PrintList(delBufNum)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_PrintList()',10)
    endif

    let l:ListChanged = <SID>Bf_BuildList(a:delBufNum, 1)

    if (l:ListChanged == 1 || g:Tb_ForceDisplay)
        let l:save_rep = &report
        let l:save_sc = &showcmd
        let &report = 10000
        set noshowcmd

        " Delete all lines in buffer.
        1,$d _

        " Goto the end of the buffer put the buffer list
        " and then delete the extra trailing blank line
        $
        put! =g:Tb_VimBufList
        $ d _
        let g:Tb_ForceDisplay = 0

        let &report  = l:save_rep
        let &showcmd = l:save_sc
    else
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('Buffer list not update since there was no change',9)
        endif
    endif
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_PrintList()',10)
    endif
endfunction " %%


" Bf_BuildList - Build the text for the -TabBar- window ~~
" Creates the buffer list string and returns 1 if it is different than
" last time this was called and 0 otherwise.
function! <SID>Bf_BuildList(delBufNum, updateBufList)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_BuildList()',10)
    endif

    let l:NBuffers = bufnr('$')     " Get the number of the last buffer.
    let l:i = 0                     " Set the buffer index to zero.
    let l:y = 0                     " Displayed buffers: more sugestive
    let l:fileNames = ''
    let l:maxTabWidth = 0
    call <SID>Map_Clear()

    " Loop through every buffer less than the total number of buffers.
    while( l:i <= l:NBuffers)
            let l:i = l:i + 1

        " If we have a delBufNum and it is the current
        " buffer then ignore the current buffer.
        " Otherwise, continue.
        if (a:delBufNum == -1 || l:i != a:delBufNum)
            " Make sure the buffer in question is listed.
            if(getbufvar(l:i, '&buflisted') == 1)
                " Get the name of the buffer.
                let l:BufName = bufname(l:i)
                " Check to see if the buffer is a blank or not. If the buffer does have
                " a name, process it.
                if(strlen(l:BufName))
                    " Only show modifiable buffers (The idea is that we don't
                    " want to show Explorers)
                    if (getbufvar(l:i, '&modifiable') == 1 && BufName != '-TabBar-')
                        " Get filename & Remove []'s & ()'s
                        let l:shortBufName = fnamemodify(l:BufName, ":t")
                        let l:shortBufName = substitute(l:shortBufName, '[][()]', '', 'g')
                        let l:y =l:y +1
                        let g:Tb_BufferMap=g:Tb_BufferMap . l:y . "-" . l:i . "\r"
                        let l:tab = '['.l:y.':'.l:shortBufName." ]"

                        " If the buffer is open in a window mark it
                        if bufwinnr(l:i) != -1
                            let l:tab = "[".l:y.':'.l:shortBufName."]*"
                        endif

                        " If the buffer is modified then mark it
                        if(getbufvar(l:i, '&modified') == 1)
                            let l:tab = l:tab . '+'
                        endif
                        let l:maxTabWidth = <SID>Tb_Max(strlen(l:tab), l:maxTabWidth)
                        let l:fileNames = l:fileNames.l:tab
                        " If horizontal and tab wrap is turned on we need to add spaces
                        if g:Tb_VSplit == 0
                            if g:Tb_TabWrap != 0
                                let l:fileNames = l:fileNames.' '
                            endif
                            " If not horizontal we need a newline
                        else
                            let l:fileNames = l:fileNames . "\n"
                        endif
                    endif
                endif
            endif
        endif
    endwhile

    if (g:Tb_VimBufList != l:fileNames)
        if (a:updateBufList)
            let g:Tb_VimBufList = l:fileNames
            let s:maxTabWidth = l:maxTabWidth
        endif
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_BuildList: Bf_List has changed',10)
        endif
        return 1
    else
        if g:Tb_DBG_LVL > 0
            call <SID>DEBUG('EXIT : Bf_BuildList: no changes',10)
        endif
        return 0
    endif
endfunction " %%


" Bf_Eligible - Are there enough -TabBar- eligible buffers to open the -TabBar- window? ~~
" Returns 1 if there are any buffers that can be displayed in a
" mini buffer explorer. Otherwise returns 0. If delBufNum is
" any non -1 value then don't include that buffer in the list
" of eligible buffers.
function! <SID>Bf_Eligible(delBufNum)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_Eligible()',10)
    endif

  let l:save_rep = &report
  let l:save_sc = &showcmd
  let &report = 10000
  set noshowcmd

  let l:NBuffers = bufnr('$')     " Get the number of the last buffer.
  let l:i        = 0              " Set the buffer index to zero.
  let l:found    = 0              " No buffer found

  if (g:Tb_MoreThanOne > 1)
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('Bf_Eligible : More Than One mode turned on',6)
    endif
  endif
  let l:needed = g:Tb_MoreThanOne

  " Loop through every buffer less than the total number of buffers.
  while(l:i <= l:NBuffers && l:found < l:needed)
    let l:i = l:i + 1

    " If we have a delBufNum and it is the current
    " buffer then ignore the current buffer.
    " Otherwise, continue.
    if (a:delBufNum == -1 || l:i != a:delBufNum)
      " Make sure the buffer in question is listed.
      if (getbufvar(l:i, '&buflisted') == 1)
        " Get the name of the buffer.
        let l:BufName = bufname(l:i)
        " Check to see if the buffer is a blank or not. If the buffer does have
        " a name, process it.
        if (strlen(l:BufName))
          " Only show modifiable buffers (The idea is that we don't
          " want to show Explorers)
          if ((getbufvar(l:i, '&modifiable') == 1) && (BufName != '-TabBar-'))

              let l:found = l:found + 1

          endif
        endif
      endif
    endif
  endwhile

  let &report  = l:save_rep
  let &showcmd = l:save_sc

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_Eligible '.l:found.' eligible buffers of '.l:needed.' needed',6)
    endif
    return (l:found >= l:needed)
endfunction " %%


" Bf_SwitchTo      Switch to bufNum( parameter) buffer~~
function! <SID>Bf_SwitchTo( bufNum)

    let l:vimbuf = <SID>Map_Get_key( a:bufNum )
    exec "b!" . l:vimbuf
endfunction " %%


" Bf_CrSel - From the -TabBar- window, open buffer under the cursor ~~
" If we are in our explorer, then we attempt to open the buffer under the
" cursor in the previous window.
function! <SID>Bf_CrSel()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_CrSel()' ,10)
    endif

  " Make sure we are in our window
  if bufname('%') != '-TabBar-'
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_CrSel : called in invalid window',1)
    endif
    return
  endif

  let l:save_rep = &report
  let l:save_sc  = &showcmd
  let &report    = 10000
  set noshowcmd

  let l:bufnr  = <SID>Bf_Choosed()
  let l:resize = 0

  if(l:bufnr != -1)             " If the buffer exists.

    let l:saveTb_AutoUpdt = g:Tb_AutoUpdt
    let g:Tb_AutoUpdt = 0
    " Switch to the previous window
    wincmd p

    " If we are in the buffer explorer or in a nonmodifiable buffer with
    " g:Tb_ModSelTarget set then try another window (a few times)
    if bufname('%') == '-TabBar-' || (g:Tb_ModSelTarget == 1 && getbufvar(bufnr('%'), '&modifiable') == 0)
      wincmd w
      if bufname('%') == '-TabBar-' || (g:Tb_ModSelTarget == 1 && getbufvar(bufnr('%'), '&modifiable') == 0)
        wincmd w
        if bufname('%') == '-TabBar-' || (g:Tb_ModSelTarget == 1 && getbufvar(bufnr('%'), '&modifiable') == 0)
          wincmd w
          " The following handles the case where -TabBar-
          " is the only window left. We need to resize so we don't
          " end up with a 1 or two line buffer.
          if bufname('%') == '-TabBar-'
            let l:resize = 1
          endif
        endif
      endif
    endif

    "exec('b! '.l:bufnr)
    call <SID>Bf_SwitchTo( l:bufnr)
    if (l:resize)
      resize
    endif
    let g:Tb_AutoUpdt = l:saveTb_AutoUpdt
    call <SID>Tb_AutoUpdt(-1)

  endif

  let &report  = l:save_rep
  let &showcmd = l:save_sc

    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_CrSel()',10)
    endif
endfunction " %%


" Bf_DblClkSel - Double click with the mouse.~~
function! <SID>Bf_DblClkSel()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('ENTER: Bf_DblClkSel()',10)
    endif
  call <SID>Bf_CrSel()
    if g:Tb_DBG_LVL > 0
        call <SID>DEBUG('EXIT : Bf_DblClkSel()',10)
    endif
endfunction " %%


" Bf_Cycle - Cycle Through Buffers ~~
" Move to next or previous buffer in the current window. If there
" are no more modifiable buffers then stay on the current buffer.
" can be called with no parameters in which case the buffers are
" cycled forward. Otherwise a single argument is accepted, if
" it's 0 then the buffers are cycled backwards, otherwise they
" are cycled forward.
function! <SID>Bf_Cycle(forward)

    " The following hack handles the case where we only have one
    " window open and it is too small
    let l:saveTb_AutoUpdt = g:Tb_AutoUpdt
    if (winbufnr(2) == -1)
        resize
        let g:Tb_AutoUpdt = 0
    endif

    " Change buffer (keeping track of before and after buffers)
    let l:origBuf = bufnr('%')
    if (a:forward == 1)
        bn!
    else
        bp!
    endif
    let l:curBuf  = bufnr('%')

    " Skip any non-modifiable buffers, but don't cycle forever
    " This should stop us from stopping in any of the [Explorers]
    " FIXME: infite loop
    while getbufvar(l:curBuf, '&modifiable') == 0 && l:origBuf != l:curBuf
        if (a:forward == 1)
            bn!
        else
            bp!
        endif
        let l:curBuf = bufnr('%')
    endwhile

    let g:Tb_AutoUpdt = l:saveTb_AutoUpdt
    if (l:saveTb_AutoUpdt == 1)
        call <SID>Tb_AutoUpdt(-1)
    endif
endfunction " %%

" the format for a map is something like:
" idx-key\r
" idx: the number displayed in tabbar [idx:fooBar.txt]
" key: the buffer number in which idx is hold
function! <SID>Map_Get_key( idx )
    let l:i=matchstr( g:Tb_BufferMap, a:idx . "-[0-9]*\r" )
    let l:x=substitute (l:i , a:idx ."-", "","")
    let l:x=substitute (l:x , "\r", "","")
    return l:x
endfunction

function! <SID>Map_Get_idx( key )
    let l:i=matchstr( g:Tb_BufferMap, "[0-9]*-" , a:key."\r" )
    let l:x=substitute (l:i , "-".a:key, "","")
    let l:x=substitute (l:x , "\r", "","")
    return l:x
endfunction


" I know this is short, But it keeps the code clean
function! <SID>Map_Clear()
    let g:Tb_BufferMap=''
endfunction


" DEBUG - Display debug output when debugging is turned on ~~
" Thanks to Charles E. Campbell, Jr. PhD <cec@NgrOyphSon.gPsfAc.nMasa.gov>
" for Decho.vim which was the inspiration for this enhanced debugging
" capability.
function! <SID>DEBUG(msg, level)

    if g:Tb_DBG_LVL >= a:level

        " Prevent a report of our actions from showing up.
        let l:save_rep    = &report
        let l:save_sc     = &showcmd
        let &report       = 10000
        set noshowcmd

        " Debug output to a buffer
        if g:Tb_DebugMode == 0
            " Save the current window number so we can come back here
            let l:prevWin     = winnr()
            wincmd p
            let l:prevPrevWin = winnr()
            wincmd p

            " Get into the debug window or create it if needed
            call <SID>Win_FindOrCreate('TabBar.DBG', 1, 0)

            " Make sure we really got to our window, if not we
            " will display a confirm dialog and turn debugging
            " off so that we won't break things even more.
            if bufname('%') != 'TabBar.DBG'
                call confirm('ERROR in window debugging code. Dissabling TabBar debugging.', 'OK')
                let g:Tb_DBG_LVL = 0
            endif
            " Write Message to DBG buffer
            let res=append("$",s:DBG_LN_CNT.':'.a:level.":\t".a:msg)
            norm G
            "set nomodified

            " Return to original window
            exec l:prevPrevWin.' wincmd w'
            exec l:prevWin.' wincmd w'
        " Debug output using VIM's echo facility
        elseif g:Tb_DebugMode == 1
        echo s:DBG_LN_CNT.':'.a:level.':'.a:msg
        " Debug output to a file -- VERY SLOW!!!
        " should be OK on UNIX and Win32 (not the 95/98 variants)
        elseif g:Tb_DebugMode == 2
            if has('system') || has('fork')
                if has('win32') && !has('win95')
                    let l:result = system("cmd /c 'echo ".s:DBG_LN_CNT.':'.a:level.':'.a:msg." >> TabBar.DBG'")
                endif
                if has('unix')
                    let l:result = system("echo '".s:DBG_LN_CNT.':'.a:level.':'.a:msg." >> TabBar.DBG'")
                endif
            else
                call confirm('Error in file writing version of the debugging code, vim not compiled with system or fork. Dissabling TabBar debugging.', 'OK')
                let g:Tb_DBG_LVL = 0
            endif
        elseif g:Tb_DebugMode == 3
            let g:Tb_DbgOutput = g:Tb_DbgOutput."\n".s:DBG_LN_CNT.':'.a:level.':'.a:msg
        endif
        let s:DBG_LN_CNT = s:DBG_LN_CNT + 1

        let &report  = l:save_rep
        let &showcmd = l:save_sc

    endif
endfunc " %%


"     Documentation~~
"
"     :TbStart .......... Open and/or goto Explorer
"     :TbStop  .......... Close the Explorer if it's open
"     :TbAup   .......... Update Explorer without navigating
"     :TbToggle ......... Toggle Tabbar
"``````````````````````````````````````````````````````````````````
"
"
"     let g:Tb_SplitBelow=0     " Put new window above current or on the  left
                                " for vertical split
"     let g:Tb_SplitBelow=1     " Put new window below current or on the  right
                                " for vertical split
"     By default we are now forcing the Tabbar window to open up at the edge of
"     the screen.
"     You can turn this off by setting the following variable in .vimrc:
"
"           let g:Tb_SplitToEdge = 0

"  o  IN HORIZONTAL MODE
"     You can set the max height by setting this in .vimrc:
"
"           let g:Tb_MaxSize = <max lines: default 0>
"
"     Setting this to 0 will mean the window gets as big as
"     needed to fit all your buffers.
"
"   o  NOTE
"   You can set the min height by
"   letting the following variable in your .vimrc:
"
"       let g:Tb_MinSize = <min height: default 1>
"
"   o  IN VERTICAL MODE
"   By default the vertical explorer has a fixed width. If you put:
"
"       let g:Tb_MaxSize = <max width: default 0>
"
"   into your .vimrc then tabbar will attempt to set the width of the
"   tabbar window to be as wide as your widest tab. The width will not
"   exceed Tb_MaxSize even if you have wider tabs.
"
"   Accepting the default value of 0 for this will give you a fixed
"   width tabbar window.
"
"   --    You can specify a MinSize for the vertical explorer window by
"   putting the following in your .vimrc:
"
"         let g:Tb_MinSize = <min width: default 1>
"
"   This will have no effect unless you also specivy Tb_MaxSize.
"````````````````````````````````````````````````````````````````
"
"
"   --  This stops the -TabBar- from opening
"   automatically until more than one eligible buffer is available.
"   You can turn this feature off by setting the following variable
"   in your .vimrc:
"      Setting this to 0 will cause the TabBar window to be loaded even
"   if no buffers are available. Setting it to 1 causes the TabBar
"   window to be loaded as soon as an eligible buffer is read. You
"   can also set it to larger numbers. So if you set it to 4 for
"   example the TabBar window wouldn't auto-open until 4 eligibles
"   buffers had been loaded. This is nice for folks that don't
"   want an TabBar window unless they are editing more than two or
"   three buffers.
"
"       let g:Tb_MoreThanOne=1
"``````````````````````````````````````````````````````````````````
"
"
"   --  To enable the optional mapping of Control + Vim Direction Keys
"   [hjkl] to window movement commands, you can put the following into
"   your .vimrc:
"
"       let g:Tb_MapWindowNavVim = 1
"
"   To enable the optional mapping of Control + Arrow Keys to window
"   movement commands, you can put the following into your .vimrc:
"
"       let g:Tb_MapWindowNavArrows = 1
"``````````````````````````````````````````````````````````````````
"
"
"   --  To enable the optional mapping of <C-TAB> and <C-S-TAB> to a
"   function that will bring up the next or previous buffer in the
"   current window, you can put the following into your .vimrc:
"
"       let g:Tb_MapCTabSwitchBufs = 1
"````````````````````````````````````````````````````````````````
"
"
"   --  To enable the optional mapping of <C-TAB> and <C-S-TAB> to mappings
"   that will move to the next and previous (respectively) window, you
"   can put the following into your .vimrc:
"
"       let g:Tb_MapCTabSwitchWindows = 1
"
"
"   o  NOTE
"   If you set TabSwitchBufs AND ...TabSwitchWindows,
"           TabSwitchBufs will *BE* enabled and
"           TabSwitchWindows will be *NOT*.
"``````````````````````````````````````````````````````````````````
"
"
"   --  If you would like to single click on tabs rather than double
"   clicking on them to goto the selected buffer.
"
"       let g:Tb_UseSingleClick = 1
"````````````````````````````````````````````````````````````````
"
"
"   --  It is possible to customize the the highlighting for the tabs in
"   the TabBar by configuring the following highlighting groups:
"
"      Tb_Normal .............  for buffers that have NOT CHANGED and
"                               are NOT VISIBLE.
"      Tb_Changed ............. for buffers that HAVE CHANGED and are
"                               NOT VISIBLE
"      Tb_VisibleNormal ....... buffers that have NOT CHANGED and are
"                               VISIBLE
"      Tb_VisibleChanged ...... buffers that have CHANGED and are VISIBLE
"
"   You can either link to an existing highlighting group by
"   adding a command like:
"
"       hi link Tb_VisibleChanged Error
"
"   to your .vimrc or you can specify exact foreground and background
"   colors using the following syntax:
"
"       hi Tb_Changed guibg=darkblue ctermbg=darkblue termbg=white
"
"   o  NOTE
"   If you set a colorscheme in your .vimrc you should do it
"       BEFORE updating the TabBar highlighting groups.
"
"   If you use other explorers like TagList you can put:
"
"       let g:Tb_ModSelTarget = 1
"
"   into your .vimrc in order to force TabBar to try to place selected
"   buffers into a window that does not have a nonmodifiable buffer.
"   The upshot of this should be that if you go into TabBar and select
"   a buffer, the buffer should not show up in a window that is
"   hosting an explorer.
"
"       let g:Tb_ForceSyntaxEnable = 1
"````````````````````````````````````````````````````````````````
"
"
"   --  TabBar has a basic debugging capability.
"       let g:Tb_DebugLevel = 0     " TabBar serious errors output
"       let g:Tb_DebugLevel = 4     " TabBar all errors output
"       let g:Tb_DebugLevel = 10    " TabBar reports everything
"
"   You can also set a DebugMode to cause output to be target as
"   follows (default is mode 3):
"
"       let g:Tb_DebugMode  = 0     " Errors will show up in a vim window
"       let g:Tb_DebugMode  = 1     " Uses VIM's echo function to display on
"                                   " the screen
"       let g:Tb_DebugMode  = 2     " Writes to a file TabBarDBG.vim
"       let g:Tb_DebugMode  = 3     " Store output in variable g:Tb_DbgOutput
"
"   Or if you are able to start VIM, you might just perform these
"   at a command prompt right before you do the operation that is
"   failing.
"````````````````````````````````````````````````````````````````
" %%

" BUGFIX:
" 0.7. Removed mapping to <tt>, to avoid delayed response to <<
" 0.6. Fixed the "delete with d" bug.

" TODO: 0.  When the tabbar is full, try to scroll it
"       1.  See what is goind on with the error "Not enough space"
"       2.  Indent code
"       3.  Optimize code
"       4.  Better Document
"       5.  noremap in vimrc to <C-TAB> to update the buffer list
"       6.  noremap in vimrc to <?> to switch to -TabBar-
"vim:foldmethod=marker vim:foldmarker=~~,%%
