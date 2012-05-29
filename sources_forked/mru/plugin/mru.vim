" File: mru.vim
" Author: Yegappan Lakshmanan (yegappan AT yahoo DOT com)
" Version: 3.4
" Last Modified: April 13, 2012
" Copyright: Copyright (C) 2003-2012 Yegappan Lakshmanan
"            Permission is hereby granted to use and distribute this code,
"            with or without modifications, provided that this copyright
"            notice is copied with it. Like anything else that's free,
"            mru.vim is provided *as is* and comes with no warranty of any
"            kind, either expressed or implied. In no event will the copyright
"            holder be liable for any damamges resulting from the use of this
"            software.
"
" Overview
" --------
" The Most Recently Used (MRU) plugin provides an easy access to a list of
" recently opened/edited files in Vim. This plugin automatically stores the
" file names as you open/edit them in Vim.
"
" This plugin will work on all the platforms where Vim is supported. This
" plugin will work in both console and GUI Vim. This version of the MRU
" plugin needs Vim 7.0 and above. If you are using an earlier version of
" Vim, then you should use an older version of the MRU plugin.
"
" The recently used filenames are stored in a file specified by the Vim
" MRU_File variable.
"
" Installation
" ------------
" 1. Copy the mru.vim file to one of the following directories:
"
"       $HOME/.vim/plugin     - Unix like systems
"       $HOME/vimfiles/plugin - MS-Windows
"       $VIM:vimfiles:plugin  - Macintosh
"       $VIM/vimfiles/plugin  - All
"
"    Refer to the following Vim help topics for more information about Vim
"    plugins:
"
"       :help add-plugin
"       :help add-global-plugin
"       :help runtimepath
"
" 2. Set the MRU_File Vim variable in the .vimrc file to the location of a
"    file to store the most recently edited file names. This step is needed
"    only if you want to change the default MRU filename.
" 3. Restart Vim.
" 4. You can use the ":MRU" command to list and edit the recently used files.
"    In GUI Vim, you can use the 'File->Recent Files' menu to access the
"    recently used files.
"
" To uninstall this plugin, remove this file (mru.vim) from the
" $HOME/.vim/plugin or $HOME/vimfiles/plugin or the $VIM/vimfile/plugin
" directory.
"
" Usage
" -----
" To list and edit files from the MRU list, you can use the ":MRU" command.
" The ":MRU" command displays the MRU file list in a temporary Vim window.  If
" the MRU window is already opened, then the MRU list displayed in the window
" is refreshed.
"
" If you are using GUI Vim, then the names of the recently edited files are
" added to the "File->Recent Files" menu. You can select the name of a file
" from this sub-menu to edit the file.
"
" You can use the normal Vim commands to move around in the MRU window. You
" cannot make changes in the MRU window.
"
" You can select a file name to edit by pressing the <Enter> key or by double
" clicking the left mouse button on a file name.  The selected file will be
" opened. If the file is already opened in a window, the cursor will be moved
" to that window. Otherwise, the file is opened in the previous window. If the
" previous window has a modified buffer or is the preview window or is used by
" some other plugin, then the file is opened in a new window.
"
" You can press the 'o' key to open the file name under the cursor in the
" MRU window in a new window.
"
" To open a file from the MRU window in read-only mode (view), press the 'v'
" key.
"
" To open a file from the MRU window in a new tab, press the 't' key.  If the
" file is already opened in a window in the current or in another tab, then
" the cursor is moved to that tab. Otherwise, a new tab is opened.
"
" You can open multiple files from the MRU window by specifying a count before
" pressing '<Enter>' or 'v' or 'o' or 't'. You can also visually select
" multiple filenames and invoke the commands to open the files. Each selected
" file will be opened in a separate window or tab.
"
" You can press the 'u' key in the MRU window to update the file list. This is
" useful if you keep the MRU window open always.
"
" You can close the MRU window by pressing the 'q' key or using one of the Vim
" window commands.
"
" To display only files matching a pattern from the MRU list in the MRU
" window, you can specify a pattern to the ":MRU" command. For example, to
" display only file names matching "vim" in them, you can use the following
" command ":MRU vim".  When you specify a partial file name and only one
" matching filename is found, then the ":MRU" command will edit that file.
"
" The ":MRU" command supports command-line completion of file names from
" the MRU list. You can enter a partial file name and then press <Tab>
" or <Ctrl-D> to complete or list all the matching file names. Note that
" after typing the ":MRU" command, you have to enter a space before completing
" the file names with <Tab>.
"
" When a file supplied to the ":MRU" command is not present in the MRU list,
" but it is a readable file, then the file will be opened (even though it is
" not present in the MRU list). This is useful if you want to open a file
" present in the same directory as a file in the MRU list. You can use the
" command-line completion of the ":MRU" command to complete the full path of a
" file and then modify the path to open another file present in the same path.
"
" Whenever the MRU list changes, the MRU file is updated with the latest MRU
" list. When you have multiple instances of Vim running at the same time, the
" latest MRU list will show up in all the instances of Vim.
"
" Configuration
" -------------
" By changing the following variables you can configure the behavior of this
" plugin. Set the following variables in your .vimrc file using the 'let'
" command.
"
" The list of recently edited file names is stored in the file specified by the
" MRU_File variable.  The default setting for this variable is
" $HOME/.vim_mru_files for Unix-like systems and $USERPROFILE/_vim_mru_files
" for MS-Windows systems. You can change this variable to point to a file by
" adding the following line to the .vimrc file:
"
"       let MRU_File = 'd:\myhome\_vim_mru_files'
"
" By default, the plugin will remember the names of the last 100 used files.
" As you edit more files, old file names will be removed from the MRU list.
" You can set the 'MRU_Max_Entries' variable to remember more file names. For
" example, to remember 1000 most recently used file names, you can use
"
"       let MRU_Max_Entries = 1000
"
" By default, all the edited file names will be added to the MRU list. If you
" want to exclude file names matching a list of patterns, you can set the
" MRU_Exclude_Files variable to a list of Vim regular expressions. By default,
" this variable is set to an empty string. For example, to not include files
" in the temporary (/tmp, /var/tmp and d:\temp) directories, you can set the
" MRU_Exclude_Files variable to
"
"       let MRU_Exclude_Files = '^/tmp/.*\|^/var/tmp/.*'  " For Unix
"       let MRU_Exclude_Files = '^c:\\temp\\.*'           " For MS-Windows
" 
" The specified pattern should be a Vim regular expression pattern.
"
" If you want to add only file names matching a set of patterns to the MRU
" list, then you can set the MRU_Include_Files variable. This variable should
" be set to a Vim regular expression pattern. For example, to add only .c and
" .h files to the MRU list, you can set this variable as below:
"
"       let MRU_Include_Files = '\.c$\|\.h$'
"
" By default, MRU_Include_Files is set to an empty string and all the edited
" filenames are added to the MRU list.
"
" The default height of the MRU window is 8. You can set the MRU_Window_Height
" variable to change the window height.
"
"       let MRU_Window_Height = 15
"
" By default, when the :MRU command is invoked, the MRU list will be displayed
" in a new window. Instead, if you want the MRU plugin to reuse the current
" window, then you can set the 'MRU_Use_Current_Window' variable to one.
"
"       let MRU_Use_Current_Window = 1
"
" The MRU plugin will reuse the current window. When a file name is selected,
" the file is also opened in the current window.
"
" When you select a file from the MRU window, the MRU window will be
" automatically closed and the selected file will be opened in the previous
" window. You can set the 'MRU_Auto_Close' variable to zero to keep the MRU
" window open.
"
"       let MRU_Auto_Close = 0
"
" If you don't use the "File->Recent Files" menu and want to disable it,
" then you can set the 'MRU_Add_Menu' variable to zero. By default, the
" menu is enabled.
"
"       let MRU_Add_Menu = 0
"
" If too many file names are present in the MRU list, then updating the MRU
" menu to list all the file names makes Vim slow. To avoid this, the
" MRU_Max_Menu_Entries variable controls the number of file names to show in
" the MRU menu. By default, this is set to 10. You can change this to show
" more entries in the menu.
"
"       let MRU_Max_Menu_Entries = 20
"
" If many file names are present in the MRU list, then the MRU menu is split
" into sub-menus. Each sub-menu contains MRU_Max_Submenu_Entries file names.
" The default setting for this is 10. You can change this to increase the
" number of file names displayed in a single sub-menu:
"
"       let MRU_Max_Submenu_Entries = 15
"
" ****************** Do not modify after this line ************************
if exists('loaded_mru')
    finish
endif
let loaded_mru=1

if v:version < 700
    finish
endif

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" MRU configuration variables {{{1
" Maximum number of entries allowed in the MRU list
if !exists('MRU_Max_Entries')
    let MRU_Max_Entries = 100
endif

" Files to exclude from the MRU list
if !exists('MRU_Exclude_Files')
    let MRU_Exclude_Files = ''
endif

" Files to include in the MRU list
if !exists('MRU_Include_Files')
    let MRU_Include_Files = ''
endif

" Height of the MRU window
" Default height is 8
if !exists('MRU_Window_Height')
    let MRU_Window_Height = 8
endif

if !exists('MRU_Use_Current_Window')
    let MRU_Use_Current_Window = 0
endif

if !exists('MRU_Auto_Close')
    let MRU_Auto_Close = 1
endif

if !exists('MRU_File')
    if has('unix') || has('macunix')
        let MRU_File = $HOME . '/.vim_mru_files'
    else
        let MRU_File = $VIM . '/_vim_mru_files'
        if has('win32')
            " MS-Windows
            if $USERPROFILE != ''
                let MRU_File = $USERPROFILE . '\_vim_mru_files'
            endif
        endif
    endif
endif

" Option for enabling or disabling the MRU menu
if !exists('MRU_Add_Menu')
    let MRU_Add_Menu = 1
endif

" Maximum number of file names to show in the MRU menu. If too many files are
" listed in the menu, then Vim becomes slow when updating the menu. So set
" this to a low value.
if !exists('MRU_Max_Menu_Entries')
    let MRU_Max_Menu_Entries = 10
endif

" Maximum number of file names to show in a MRU sub-menu. If the MRU list
" contains more file names than this setting, then the MRU menu is split into
" one or more sub-menus.
if !exists('MRU_Max_Submenu_Entries')
    let MRU_Max_Submenu_Entries = 10
endif

" When only a single matching filename is found in the MRU list, the following
" option controls whether the file name is displayed in the MRU window or the
" file is directly opened. When this variable is set to 0 and a single
" matching file name is found, then the file is directly opened.
if !exists('MRU_Window_Open_Always')
    let MRU_Window_Open_Always = 0
endif

" When opening a file from the MRU list, the file is opened in the current
" tab. If the selected file has to be opened in a tab always, then set the
" following variable to 1. If the file is already opened in a tab, then the
" cursor will be moved to that tab.
if !exists('MRU_Open_File_Use_Tabs')
    let MRU_Open_File_Use_Tabs = 0
endif

" Control to temporarily lock the MRU list. Used to prevent files from
" getting added to the MRU list when the ':vimgrep' command is executed.
let s:mru_list_locked = 0

" MRU_LoadList                          {{{1
" Loads the latest list of file names from the MRU file
function! s:MRU_LoadList()
    " If the MRU file is present, then load the list of filenames. Otherwise
    " start with an empty list.
    if filereadable(g:MRU_File)
        let s:MRU_files = readfile(g:MRU_File)
        if s:MRU_files[0] =~# '^\s*" Most recently edited files in Vim'
            " Generated by the previous version of the MRU plugin.
            " Discard the list.
            let s:MRU_files = []
        elseif s:MRU_files[0] =~# '^#'
            " Remove the comment line
            call remove(s:MRU_files, 0)
        else
            " Unsupported format
            let s:MRU_files = []
        endif
    else
        let s:MRU_files = []
    endif

    " Refresh the MRU menu with the latest list of filenames
    call s:MRU_Refresh_Menu()
endfunction

" MRU_SaveList                          {{{1
" Saves the MRU file names to the MRU file
function! s:MRU_SaveList()
    let l = []
    call add(l, '# Most recently edited files in Vim (version 3.0)')
    call extend(l, s:MRU_files)
    call writefile(l, g:MRU_File)
endfunction

" MRU_AddFile                           {{{1
" Adds a file to the MRU file list
"   acmd_bufnr - Buffer number of the file to add
function! s:MRU_AddFile(acmd_bufnr)
    if s:mru_list_locked
        " MRU list is currently locked
        return
    endif

    " Get the full path to the filename
    let fname = fnamemodify(bufname(a:acmd_bufnr + 0), ':p')
    if fname == ''
        return
    endif

    " Skip temporary buffers with buftype set. The buftype is set for buffers
    " used by plugins.
    if &buftype != ''
        return
    endif

    if g:MRU_Include_Files != ''
        " If MRU_Include_Files is set, include only files matching the
        " specified pattern
        if fname !~# g:MRU_Include_Files
            return
        endif
    endif

    if g:MRU_Exclude_Files != ''
        " Do not add files matching the pattern specified in the
        " MRU_Exclude_Files to the MRU list
        if fname =~# g:MRU_Exclude_Files
            return
        endif
    endif

    " If the filename is not already present in the MRU list and is not
    " readable then ignore it
    let idx = index(s:MRU_files, fname)
    if idx == -1
        if !filereadable(fname)
            " File is not readable and is not in the MRU list
            return
        endif
    endif

    " Load the latest MRU file list
    call s:MRU_LoadList()

    " Remove the new file name from the existing MRU list (if already present)
    call filter(s:MRU_files, 'v:val !=# fname')

    " Add the new file list to the beginning of the updated old file list
    call insert(s:MRU_files, fname, 0)

    " Trim the list
    if len(s:MRU_files) > g:MRU_Max_Entries
        call remove(s:MRU_files, g:MRU_Max_Entries, -1)
    endif

    " Save the updated MRU list
    call s:MRU_SaveList()

    " Refresh the MRU menu
    call s:MRU_Refresh_Menu()

    " If the MRU window is open, update the displayed MRU list
    let bname = '__MRU_Files__'
    let winnum = bufwinnr(bname)
    if winnum != -1
        let cur_winnr = winnr()
        call s:MRU_Open_Window()
        if winnr() != cur_winnr
            exe cur_winnr . 'wincmd w'
        endif
    endif
endfunction

" MRU_escape_filename                   {{{1
" Escape special characters in a filename. Special characters in file names
" that should be escaped (for security reasons)
let s:esc_filename_chars = ' *?[{`$%#"|!<>();&' . "'\t\n"
function! s:MRU_escape_filename(fname)
    return escape(a:fname, s:esc_filename_chars)
endfunction

" MRU_Edit_File                         {{{1
" Edit the specified file
"   filename - Name of the file to edit
"   sanitized - Specifies whether the filename is already escaped for special
"   characters or not.
function! s:MRU_Edit_File(filename, sanitized)
    if !a:sanitized
	let esc_fname = s:MRU_escape_filename(a:filename)
    else
	let esc_fname = a:filename
    endif

    " If the user wants to always open the file in a tab, then open the file
    " in a tab. If it is already opened in a tab, then the cursor will be
    " moved to that tab.
    if g:MRU_Open_File_Use_Tabs
	call s:MRU_Open_File_In_Tab(a:filename, esc_fname)
	return
    endif

    " If the file is already open in one of the windows, jump to it
    let winnum = bufwinnr('^' . a:filename . '$')
    if winnum != -1
        if winnum != winnr()
            exe winnum . 'wincmd w'
        endif
    else
        if &modified || &buftype != '' || &previewwindow
            " Current buffer has unsaved changes or is a special buffer or is
            " the preview window.  So open the file in a new window
            exe 'split ' . esc_fname
        else
            exe 'edit ' . esc_fname
        endif
    endif
endfunction

" MRU_Open_File_In_Tab
" Open a file in a tab. If the file is already opened in a tab, jump to the
" tab. Otherwise, create a new tab and open the file.
"   fname     : Name of the file to open
"   esc_fname : File name with special characters escaped
function! s:MRU_Open_File_In_Tab(fname, esc_fname)
    " If the selected file is already open in the current tab or in
    " another tab, jump to it. Otherwise open it in a new tab
    if bufwinnr('^' . a:fname . '$') == -1
	let tabnum = -1
	let i = 1
	let bnum = bufnr('^' . a:fname . '$')
	while i <= tabpagenr('$')
	    if index(tabpagebuflist(i), bnum) != -1
		let tabnum = i
		break
	    endif
	    let i += 1
	endwhile

	if tabnum != -1
	    " Goto the tab containing the file
	    exe 'tabnext ' . i
	else
	    " Open a new tab as the last tab page
	    exe '999tabnew ' . a:esc_fname
	endif
    endif

    " Jump to the window containing the file
    let winnum = bufwinnr('^' . a:fname . '$')
    if winnum != winnr()
	exe winnum . 'wincmd w'
    endif
endfunction

" MRU_Window_Edit_File                  {{{1
"   fname     : Name of the file to edit. May specify single or multiple
"               files.
"   edit_type : Specifies how to edit the file. Can be one of 'edit' or 'view'.
"               'view' - Open the file as a read-only file
"               'edit' - Edit the file as a regular file
"   multi     : Specifies  whether a single file or multiple files need to be
"               opened.
"   open_type : Specifies where to open the file. Can be one of 'useopen' or
"               'newwin' or 'newtab'.
"               useopen - If the file is already present in a window, then
"                         jump to that window.  Otherwise, open the file in
"                         the previous window.
"               newwin_horiz - Open the file in a new horizontal window.
"               newwin_vert - Open the file in a new vertical window.
"               newtab  - Open the file in a new tab. If the file is already
"                         opened in a tab, then jump to that tab.
function! s:MRU_Window_Edit_File(fname, multi, edit_type, open_type)
    let esc_fname = s:MRU_escape_filename(a:fname)

    if a:open_type == 'newwin_horiz'
        " Edit the file in a new horizontally split window above the previous
        " window
        wincmd p
        exe 'belowright new ' . esc_fname
    elseif a:open_type == 'newwin_vert'
        " Edit the file in a new vertically split window above the previous
        " window
        wincmd p
        exe 'belowright vnew ' . esc_fname
    elseif a:open_type == 'newtab' || g:MRU_Open_File_Use_Tabs
	call s:MRU_Open_File_In_Tab(a:fname, esc_fname)
    else
        " If the selected file is already open in one of the windows,
        " jump to it
        let winnum = bufwinnr('^' . a:fname . '$')
        if winnum != -1
            exe winnum . 'wincmd w'
        else
            if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
                " Jump to the window from which the MRU window was opened
                if exists('s:MRU_last_buffer')
                    let last_winnr = bufwinnr(s:MRU_last_buffer)
                    if last_winnr != -1 && last_winnr != winnr()
                        exe last_winnr . 'wincmd w'
                    endif
                endif
            else
                if g:MRU_Use_Current_Window == 0
                    " Goto the previous window
                    " If MRU_Use_Current_Window is set to one, then the
                    " current window is used to open the file
                    wincmd p
                endif
            endif

            let split_window = 0

            if &modified || &previewwindow || a:multi
                " Current buffer has unsaved changes or is the preview window
                " or the user is opening multiple files
                " So open the file in a new window
                let split_window = 1
            endif

            if &buftype != ''
                " Current buffer is a special buffer (maybe used by a plugin)
                if g:MRU_Use_Current_Window == 0 ||
                            \ bufnr('%') != bufnr('__MRU_Files__')
                    let split_window = 1
                endif
            endif

            " Edit the file
            if split_window
                " Current buffer has unsaved changes or is a special buffer or
                " is the preview window.  So open the file in a new window
                if a:edit_type == 'edit'
                    exe 'split ' . esc_fname
                else
                    exe 'sview ' . esc_fname
                endif
            else
                if a:edit_type == 'edit'
                    exe 'edit ' . esc_fname
                else
                    exe 'view ' . esc_fname
                endif
            endif
        endif
    endif
endfunction

" MRU_Select_File_Cmd                   {{{1
" Open a file selected from the MRU window
"
"   'opt' has two values separated by comma. The first value specifies how to
"   edit the file  and can be either 'edit' or 'view'. The second value
"   specifies where to open the file. It can take one of the following values:
"     'useopen' to open file in the previous window
"     'newwin_horiz' to open the file in a new horizontal split window
"     'newwin_vert' to open the file in a new vertical split window.
"     'newtab' to open the file in a new tab.
" If multiple file names are selected using visual mode, then open multiple
" files (either in split windows or tabs)
function! s:MRU_Select_File_Cmd(opt) range
    let [edit_type, open_type] = split(a:opt, ',')

    let fnames = getline(a:firstline, a:lastline)

    if g:MRU_Auto_Close == 1 && g:MRU_Use_Current_Window == 0
        " Automatically close the window if the file window is
        " not used to display the MRU list.
        silent! close
    endif

    let multi = 0

    for f in fnames
        if f == ''
            continue
        endif

        " The text in the MRU window contains the filename in parenthesis
        let file = matchstr(f, '(\zs.*\ze)')

        call s:MRU_Window_Edit_File(file, multi, edit_type, open_type)

        if a:firstline != a:lastline
            " Opening multiple files
            let multi = 1
        endif
    endfor
endfunction

" MRU_Warn_Msg                          {{{1
" Display a warning message
function! s:MRU_Warn_Msg(msg)
    echohl WarningMsg
    echo a:msg
    echohl None
endfunction

" MRU_Open_Window                       {{{1
" Display the Most Recently Used file list in a temporary window.
" If the optional argument is supplied, then it specifies the pattern of files
" to selectively display in the MRU window.
function! s:MRU_Open_Window(...)

    " Load the latest MRU file list
    call s:MRU_LoadList()

    " Check for empty MRU list
    if empty(s:MRU_files)
        call s:MRU_Warn_Msg('MRU file list is empty')
        return
    endif

    " Save the current buffer number. This is used later to open a file when a
    " entry is selected from the MRU window. The window number is not saved,
    " as the window number will change when new windows are opened.
    let s:MRU_last_buffer = bufnr('%')

    let bname = '__MRU_Files__'

    " If the window is already open, jump to it
    let winnum = bufwinnr(bname)
    if winnum != -1
        if winnr() != winnum
            " If not already in the window, jump to it
            exe winnum . 'wincmd w'
        endif

        setlocal modifiable

        " Delete the contents of the buffer to the black-hole register
        silent! %delete _
    else
        if g:MRU_Use_Current_Window
            " Reuse the current window
            "
            " If the __MRU_Files__ buffer exists, then reuse it. Otherwise open
            " a new buffer
            let bufnum = bufnr(bname)
            if bufnum == -1
                let cmd = 'edit ' . bname
            else
                let cmd = 'buffer ' . bufnum
            endif

            exe cmd

            if bufnr('%') != bufnr(bname)
                " Failed to edit the MRU buffer
                return
            endif
        else
            " Open a new window at the bottom

            " If the __MRU_Files__ buffer exists, then reuse it. Otherwise open
            " a new buffer
            let bufnum = bufnr(bname)
            if bufnum == -1
                let wcmd = bname
            else
                let wcmd = '+buffer' . bufnum
            endif

            exe 'silent! botright ' . g:MRU_Window_Height . 'split ' . wcmd
        endif
    endif

    " Mark the buffer as scratch
    setlocal buftype=nofile
    setlocal bufhidden=delete
    setlocal noswapfile
    setlocal nowrap
    setlocal nobuflisted
    " Use fixed height for the MRU window
    setlocal winfixheight

    call MRU_SetupSyntax()

    " Setup the cpoptions properly for the maps to work
    let old_cpoptions = &cpoptions
    set cpoptions&vim

    " Create mappings to select and edit a file from the MRU list
    nnoremap <buffer> <silent> <CR>
                \ :call <SID>MRU_Select_File_Cmd('edit,useopen')<CR>
    vnoremap <buffer> <silent> <CR>
                \ :call <SID>MRU_Select_File_Cmd('edit,useopen')<CR>
    nnoremap <buffer> <silent> o
                \ :call <SID>MRU_Select_File_Cmd('edit,newwin_horiz')<CR>
    vnoremap <buffer> <silent> o
                \ :call <SID>MRU_Select_File_Cmd('edit,newwin_horiz')<CR>
    nnoremap <buffer> <silent> O
                \ :call <SID>MRU_Select_File_Cmd('edit,newwin_vert')<CR>
    vnoremap <buffer> <silent> O
                \ :call <SID>MRU_Select_File_Cmd('edit,newwin_vert')<CR>
    nnoremap <buffer> <silent> t
                \ :call <SID>MRU_Select_File_Cmd('edit,newtab')<CR>
    vnoremap <buffer> <silent> t
                \ :call <SID>MRU_Select_File_Cmd('edit,newtab')<CR>
    nnoremap <buffer> <silent> v
                \ :call <SID>MRU_Select_File_Cmd('view,useopen')<CR>
    nnoremap <buffer> <silent> u :MRU<CR>
    nnoremap <buffer> <silent> <2-LeftMouse>
                \ :call <SID>MRU_Select_File_Cmd('edit,useopen')<CR>
    nnoremap <buffer> <silent> q :close<CR>

    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions

    " Display the MRU list
    if a:0 == 0
        " No search pattern specified. Display the complete list
        let m = copy(s:MRU_files)
    else
        " Display only the entries matching the specified pattern
	" First try using it as a literal pattern
	let m = filter(copy(s:MRU_files), 'stridx(v:val, a:1) != -1')
	if len(m) == 0
	    " No match. Try using it as a regular expression
	    let m = filter(copy(s:MRU_files), 'v:val =~# a:1')
	endif
    endif

    " Get the tail part of the file name (without the directory) and display
    " it along with the full path
    let  output = map(m, 'fnamemodify(v:val, ":t") . " (" . v:val . ")"')
    silent! 0put =output

    " Delete the empty line at the end of the buffer
    $delete

    " Move the cursor to the beginning of the file
    normal! gg

    setlocal nomodifiable
endfunction

" MRU_Complete                          {{{1
" Command-line completion function used by :MRU command
function! s:MRU_Complete(ArgLead, CmdLine, CursorPos)
    if a:ArgLead == ''
        " Return the complete list of MRU files
        return s:MRU_files
    else
        " Return only the files matching the specified pattern
        return filter(copy(s:MRU_files), 'v:val =~? a:ArgLead')
    endif
endfunction

" MRU_Cmd                               {{{1
" Function to handle the MRU command
"   pat - File name pattern passed to the MRU command
function! s:MRU_Cmd(pat)
    if a:pat == ''
        " No arguments specified. Open the MRU window
        call s:MRU_Open_Window()
        return
    endif

    " Load the latest MRU file
    call s:MRU_LoadList()

    " Empty MRU list
    if empty(s:MRU_files)
        call s:MRU_Warn_Msg('MRU file list is empty')
        return
    endif

    " First use the specified string as a literal string and search for
    " filenames containing the string. If only one filename is found,
    " then edit it (unless the user wants to open the MRU window always)
    let m = filter(copy(s:MRU_files), 'stridx(v:val, a:pat) != -1')
    if len(m) > 0
	if len(m) == 1 && !g:MRU_Window_Open_Always
	    call s:MRU_Edit_File(m[0], 0)
	    return
	endif

	" More than one file matches. Try find an accurate match
	let new_m = filter(m, 'v:val ==# a:pat')
	if len(new_m) == 1 && !g:MRU_Window_Open_Always
	    call s:MRU_Edit_File(new_m[0], 0)
	    return
	endif

	" Couldn't find an exact match, open the MRU window with all the
        " files matching the pattern.
	call s:MRU_Open_Window(a:pat)
	return
    endif

    " Use the specified string as a regular expression pattern and search
    " for filenames matching the pattern
    let m = filter(copy(s:MRU_files), 'v:val =~? a:pat')

    if len(m) == 0
        " If an existing file (not present in the MRU list) is specified,
        " then open the file.
        if filereadable(a:pat)
            call s:MRU_Edit_File(a:pat, 0)
            return
        endif

        " No filenames matching the specified pattern are found
        call s:MRU_Warn_Msg("MRU file list doesn't contain " .
                    \ "files matching " . a:pat)
        return
    endif

    if len(m) == 1 && !g:MRU_Window_Open_Always
        call s:MRU_Edit_File(m[0], 0)
        return
    endif

    call s:MRU_Open_Window(a:pat)
endfunction

" MRU_add_files_to_menu                 {{{1
" Adds a list of files to the "Recent Files" sub menu under the "File" menu.
"   prefix - Prefix to use for each of the menu entries
"   file_list - List of file names to add to the menu
function! s:MRU_add_files_to_menu(prefix, file_list)
    for fname in a:file_list
        " Escape special characters in the filename
        let esc_fname = escape(fnamemodify(fname, ':t'), ".\\" .
                                        \ s:esc_filename_chars)
        let esc_fname = substitute(esc_fname, '&', '&&', 'g')

        " Truncate the directory name if it is long
        let dir_name = fnamemodify(fname, ':h')
        let len = strlen(dir_name)
        " Shorten long file names by adding only few characters from
        " the beginning and end.
        if len > 30
            let dir_name = strpart(dir_name, 0, 10) .
                        \ '...' . 
                        \ strpart(dir_name, len - 20)
        endif
        let esc_dir_name = escape(dir_name, ".\\" . s:esc_filename_chars)
        let esc_dir_name = substitute(esc_dir_name, '&', '&&', 'g')

	let menu_path = '&File.&Recent\ Files.' . a:prefix . esc_fname .
		    \ '\ (' . esc_dir_name . ')'
	let esc_mfname = s:MRU_escape_filename(fname)
        exe 'anoremenu <silent> ' . menu_path .
                    \ " :call <SID>MRU_Edit_File('" . esc_mfname . "', 1)<CR>"
	exe 'tmenu ' . menu_path . ' Edit file ' . esc_mfname
    endfor
endfunction

" MRU_Refresh_Menu                      {{{1
" Refresh the MRU menu
function! s:MRU_Refresh_Menu()
    if !has('menu') || !g:MRU_Add_Menu
        " No support for menus
        return
    endif

    " Setup the cpoptions properly for the maps to work
    let old_cpoptions = &cpoptions
    set cpoptions&vim

    " Remove the MRU menu
    " To retain the teared-off MRU menu, we need to add a dummy entry
    silent! unmenu &File.&Recent\ Files
    " The menu priority of the File menu is 10. If the MRU plugin runs
    " first before menu.vim, the File menu order may not be correct.
    " So specify the priority of the File menu here.
    10noremenu &File.&Recent\ Files.Dummy <Nop>
    silent! unmenu! &File.&Recent\ Files

    anoremenu <silent> &File.&Recent\ Files.Refresh\ list
                \ :call <SID>MRU_LoadList()<CR>
    exe 'tmenu File.&Recent\ Files.Refresh\ list Reload the MRU file list from '
		\ . s:MRU_escape_filename(g:MRU_File)
    anoremenu File.&Recent\ Files.-SEP1-           :

    " Add the filenames in the MRU list to the menu
    let entry_cnt = len(s:MRU_files)
    if entry_cnt > g:MRU_Max_Menu_Entries
        " Show only MRU_Max_Menu_Entries file names in the menu
        let mru_list = s:MRU_files[1 : g:MRU_Max_Menu_Entries]
        let entry_cnt = g:MRU_Max_Menu_Entries
    else
        let mru_list = s:MRU_files
    endif
    if entry_cnt > g:MRU_Max_Submenu_Entries
	" Split the MRU menu into sub-menus
        for start_idx in range(0, entry_cnt, g:MRU_Max_Submenu_Entries)
            let last_idx = start_idx + g:MRU_Max_Submenu_Entries - 1
            if last_idx >= entry_cnt
                let last_idx = entry_cnt - 1
            endif
            let prefix = 'Files\ (' . (start_idx + 1) . '\.\.\.' .
                        \ (last_idx + 1) . ').'
            call s:MRU_add_files_to_menu(prefix,
                        \ mru_list[start_idx : last_idx])
        endfor
    else
        call s:MRU_add_files_to_menu('', mru_list)
    endif

    " Remove the dummy menu entry
    unmenu &File.&Recent\ Files.Dummy

    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions
endfunction

" Setup syntax highlight
function! MRU_SetupSyntax()
  if has("syntax")
    syn match mruName       /.\+\s/
    syn match mruDir       /(.\+)/
    hi def link mruDir Folded
    hi def link mruName String
  endif
endfunction

" Load the MRU list on plugin startup
call s:MRU_LoadList()

" MRU autocommands {{{1
" Autocommands to detect the most recently used files
autocmd BufRead * call s:MRU_AddFile(expand('<abuf>'))
autocmd BufNewFile * call s:MRU_AddFile(expand('<abuf>'))
autocmd BufWritePost * call s:MRU_AddFile(expand('<abuf>'))

" The ':vimgrep' command adds all the files searched to the buffer list.
" This also modifies the MRU list, even though the user didn't edit the
" files. Use the following autocmds to prevent this.
autocmd QuickFixCmdPre *vimgrep* let s:mru_list_locked = 1
autocmd QuickFixCmdPost *vimgrep* let s:mru_list_locked = 0

" Command to open the MRU window
command! -nargs=? -complete=customlist,s:MRU_Complete MRU
            \ call s:MRU_Cmd(<q-args>)
command! -nargs=? -complete=customlist,s:MRU_Complete Mru
            \ call s:MRU_Cmd(<q-args>)

" }}}

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save

" vim:set foldenable foldmethod=marker:
