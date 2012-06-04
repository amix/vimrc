" yankring.vim - Yank / Delete Ring for Vim
" ---------------------------------------------------------------
" Version:  12.0
" Authors:  David Fishburn <dfishburn.vim@gmail.com>
" Last Modified: 2011 Jun 14
" Script:   http://www.vim.org/scripts/script.php?script_id=1234
" Based On: Mocked up version by Yegappan Lakshmanan
"           http://groups.yahoo.com/group/vim/post?act=reply&messageNum=34406
" License:  GPL (Gnu Public License)
" GetLatestVimScripts: 1234 1 :AutoInstall: yankring.vim

if exists('loaded_yankring') || &cp
    finish
endif

if v:version < 700
  echomsg 'yankring: You need at least Vim 7.0'
  finish
endif

let loaded_yankring = 120

let s:yr_has_voperator     = 0
if v:version > 701 || ( v:version == 701 && has("patch205") )
    let s:yr_has_voperator = 1
endif

if !exists('g:yankring_history_dir')
    let g:yankring_history_dir = expand('$HOME')
else
    let g:yankring_history_dir = expand(g:yankring_history_dir)
endif

if !exists('g:yankring_history_file')
    let g:yankring_history_file = 'yankring_history'
endif

" Allow the user to override the # of yanks/deletes recorded
if !exists('g:yankring_max_history')
    let g:yankring_max_history = 100
elseif g:yankring_max_history < 0
    let g:yankring_max_history = 100
endif

" Specify the minimum length of 1 entry 
if !exists('g:yankring_min_element_length')
    let g:yankring_min_element_length = 1
endif

" Specify the maximum length of 1 entry (1MB default)
if !exists('g:yankring_max_element_length')
    let g:yankring_max_element_length = 1048576
endif

" Warn if truncation occurs
if !exists('g:yankring_warn_on_truncate')
    let g:yankring_warn_on_truncate = 1
endif

" Allow the user to specify if the plugin is enabled or not
if !exists('g:yankring_enabled')
    let g:yankring_enabled = 1
endif

" Specify max display length for each element for YRShow
if !exists('g:yankring_max_display')
    let g:yankring_max_display = 0
endif

" Check if yankring should persist between Vim instances
if !exists('g:yankring_persist')
    let g:yankring_persist = 1
endif

" Check if yankring share 1 file between all instances of Vim
if !exists('g:yankring_share_between_instances')
    let g:yankring_share_between_instances = 1
endif

" Specify whether the results of the ring should be displayed
" in a separate buffer window instead of the use of echo
if !exists('g:yankring_window_use_separate')
    let g:yankring_window_use_separate = 1
endif

" Specifies whether the window is closed after an action
" is performed
if !exists('g:yankring_window_auto_close')
    let g:yankring_window_auto_close = 1
endif

" When displaying the buffer, how many lines should it be
if !exists('g:yankring_window_height')
    let g:yankring_window_height = 8
endif

" When displaying the buffer, how many lines should it be
if !exists('g:yankring_window_width')
    let g:yankring_window_width = 30
endif

" When displaying the buffer, where it should be placed
if !exists('g:yankring_window_use_horiz')
    let g:yankring_window_use_horiz = 1
endif

" When displaying the buffer, where it should be placed
if !exists('g:yankring_window_use_bottom')
    let g:yankring_window_use_bottom = 1
endif

" When displaying the buffer, where it should be placed
if !exists('g:yankring_window_use_right')
    let g:yankring_window_use_right = 1
endif

" If the user presses <space>, toggle the width of the window
if !exists('g:yankring_window_increment')
    let g:yankring_window_increment = 50
endif

" Controls whether the . operator will repeat yank operations
" The default is based on cpoptions: |cpo-y|
"	y	A yank command can be redone with ".".
if !exists('g:yankring_dot_repeat_yank')
    let g:yankring_dot_repeat_yank = (&cpoptions=~'y'?1:0)
endif

" Only adds unique items to the yankring.
" If the item already exists, that element is set as the
" top of the yankring.
if !exists('g:yankring_ignore_duplicate')
    let g:yankring_ignore_duplicate = 1
endif

" Vim automatically manages the numbered registers:
" 0   - last yanked text
" 1-9 - last deleted items
" If this option is turned on, the yankring will manage the 
" values in them.
if !exists('g:yankring_manage_numbered_reg')
    let g:yankring_manage_numbered_reg = 0
endif

" Allow the user to specify what characters to use for the mappings.
if !exists('g:yankring_n_keys')
    " 7.1.patch205 introduces the v:operator function which was essential
    " to gain the omap support.
    if s:yr_has_voperator == 1
        " Use omaps for the rest of the functionality
        let g:yankring_n_keys = 'Y D x X'
    else
        let g:yankring_n_keys = 'x yy dd yw dw ye de yE dE yiw diw yaw daw y$ d$ Y D yG dG ygg dgg'
    endif
endif

" Allow the user to specify what operator pending motions to map
if !exists('g:yankring_o_keys')
    " o-motions and text objects, without zap-to-char motions
    let g:yankring_o_keys  = 'b B w W e E d h j k l H M L y G ^ 0 $ , ;'
    let g:yankring_o_keys .= ' g_  g^ gm g$ gk gj gg ge gE - + _ '
    let g:yankring_o_keys .= ' iw iW aw aW as is ap ip a] a[ i] i[ a) a( ab i) i( ib a> a< i> i< at it a} a{ aB i} i{ iB a" a'' a` i" i'' i`'
endif

if !exists('g:yankring_zap_keys')
    let g:yankring_zap_keys = 'f F t T / ? @'
endif

" Allow the user to specify what operator pending motions to map
if !exists('g:yankring_ignore_operator')
    let g:yankring_ignore_operator = 'g~ gu gU ! = gq g? > < zf g@'
endif
let g:yankring_ignore_operator = ' '.g:yankring_ignore_operator.' '

" Whether we should map the . operator
if !exists('g:yankring_map_dot')
    let g:yankring_map_dot = 1
endif

" Whether we sould map the "g" paste operators
if !exists('g:yankring_paste_using_g')
    let g:yankring_paste_using_g = 1
endif

if !exists('g:yankring_v_key')
    let g:yankring_v_key = 'y'
endif

if !exists('g:yankring_del_v_key')
    let g:yankring_del_v_key = 'd x'
endif

if !exists('g:yankring_paste_n_bkey')
    let g:yankring_paste_n_bkey = 'P'
endif

if !exists('g:yankring_paste_n_akey')
    let g:yankring_paste_n_akey = 'p'
endif

if !exists('g:yankring_paste_v_bkey')
    let g:yankring_paste_v_bkey = 'P'
endif

if !exists('g:yankring_paste_v_akey')
    let g:yankring_paste_v_akey = 'p'
endif

if !exists('g:yankring_paste_check_default_buffer')
    let g:yankring_paste_check_default_buffer = 1
endif

if !exists('g:yankring_replace_n_pkey')
    let g:yankring_replace_n_pkey = '<C-P>'
endif

if !exists('g:yankring_replace_n_nkey')
    let g:yankring_replace_n_nkey = '<C-N>'
endif

if !exists('g:yankring_clipboard_monitor')
    let g:yankring_clipboard_monitor = (has('clipboard')?1:0)
endif

if !exists('g:yankring_default_menu_mode')
    let g:yankring_default_menu_mode = 3
endif

" Script variables for the yankring buffer
let s:yr_buffer_name       = '[YankRing]'
let s:yr_buffer_last_winnr = -1
let s:yr_buffer_last       = -1
let s:yr_buffer_id         = -1
let s:yr_search            = ''
let s:yr_remove_omap_dot   = 0
let s:yr_history_version   = 'v2'
let s:yr_history_v1_nl     = '@@@'
let s:yr_history_v1_nl_pat = '\%(\\\)\@<!@@@'
let s:yr_history_v2_nl     = "\2" " Use double quotes for a special character
let s:yr_history_v2_nl_pat = "\2"
let s:yr_history_last_upd  = 0
let s:yr_history_file_v1   = 
            \ g:yankring_history_dir.'/'.
            \ g:yankring_history_file.
            \ (g:yankring_share_between_instances==1?'':'_'.v:servername).
            \ '.txt'
let s:yr_history_file_v2   = 
            \ g:yankring_history_dir.'/'.
            \ g:yankring_history_file.
            \ (g:yankring_share_between_instances==1?'':'_'.v:servername).
            \ '_v2.txt'


" Vim window size is changed by the yankring plugin or not
let s:yr_winsize_chgd      = 0
let s:yr_maps_created      = 0
let s:yr_maps_created_zap  = 0

" Enables or disables the yankring 
function! s:YRToggle(...)
    " Default the current state to toggle
    let new_state = ((g:yankring_enabled == 1) ? 0 : 1)

    " Allow the user to specify if enabled
    if a:0 > 0
        let new_state = ((a:1 == 1) ? 1 : 0)
    endif
            
    " YRToggle accepts an integer value to specify the state
    if new_state == g:yankring_enabled 
        return
    elseif new_state == 1
        call s:YRMapsCreate()
    else
        call s:YRMapsDelete()
    endif
endfunction
 

" Enables or disables the yankring 
function! s:YRDisplayElem(disp_nbr, script_var) 
    if g:yankring_max_display == 0
        if g:yankring_window_use_separate == 1
            let max_display = 500
        else
            let max_display = g:yankring_window_width + 
                        \ g:yankring_window_increment - 
                        \ 12
        endif
    else
        let max_display = g:yankring_max_display
    endif

    let elem = matchstr(a:script_var, '^.*\ze,.*$')
    if s:yr_history_version == 'v1'
        " v1
        " let elem = substitute(elem, '\%(\\\)\@<!@@@', '\\n', 'g')
        " v2
        let elem = substitute(elem, s:yr_history_v1_nl_pat, '\\n', 'g')
        let elem = substitute(elem, '\\@', '@', 'g')
    else
        let elem = substitute(elem, s:yr_history_v2_nl_pat, '\\n', 'g')
    endif
    let length = strlen(elem)
    " Fancy trick to align them all regardless of how many
    " digits the element # is
    return a:disp_nbr.
                \ strtrans(
                \ strpart("      ",0,(6-strlen(a:disp_nbr+1))).
                \ (
                \ (length>max_display)?
                \ (strpart(elem,0,max_display).
                \ '...'):
                \ elem
                \ )
                \ )

    return ""
endfunction
 

" Enables or disables the yankring 
function! s:YRShow(...) 
    " If no parameter was provided assume the user wants to 
    " toggle the display.
    let toggle = 1
    if a:0 > 0
        let toggle = matchstr(a:1, '\d\+')
    endif

    let show_registers = 0
    if a:0 > 1 && a:2 ==# 'R'
        let show_registers = 1
    endif

    if toggle == 1
        if bufwinnr(s:yr_buffer_id) > -1
            " If the YankRing window is already open close it
            exec bufwinnr(s:yr_buffer_id) . "wincmd w"
            hide

            " Switch back to the window which the YankRing
            " window was opened from
            if bufwinnr(s:yr_buffer_last) != -1
                " If the buffer is visible, switch to it
                exec s:yr_buffer_last_winnr . "wincmd w"
            endif

            return
        endif
    endif

    " Reset the search string, since this is automatically called
    " if the yankring window is open.  A previous search must be
    " cleared since we do not want to show new items.  The user can
    " always run the search again.
    let s:yr_search = ""

    " It is possible for registers to be changed outside of the 
    " maps of the YankRing.  Perform this quick check when we
    " show the contents (or when it is refreshed).
    if g:yankring_paste_check_default_buffer == 1 
        let save_reg = 0
        let register = ((&clipboard=~'unnamed')?'+':'"')

        if &clipboard =~ 'unnamed' && getreg('+') != s:yr_prev_clipboard
            let save_reg = 1
        endif
        if register == '"' && getreg('"') != s:yr_prev_reg_unnamed
            let save_reg = 1
        endif

        if save_reg == 1
            " The user has performed a yank / delete operation
            " outside of the yankring maps.  Add this 
            " value to the yankring.
            call YRRecord(register)
        endif
    endif

    " List is shown in order of replacement
    " assuming using previous yanks
    let output = "--- YankRing ---\n"
    let output = output . (show_registers == 1 ? 'Reg ' : 'Elem')."  Content\n"

    if show_registers == 1
        for reg_name in map( range(char2nr('0'), char2nr('9')) +
                    \ (range(char2nr('a'), char2nr('z')))
                    \, 'nr2char(v:val)' 
                    \ )
            let output  = output . s:YRDisplayElem(reg_name, getreg(reg_name).',') . "\n"
        endfor
    else
        call s:YRHistoryRead()
        let disp_item_nr = 1
        for elem in s:yr_history_list
            let output  = output . s:YRDisplayElem(disp_item_nr, elem) . "\n"
            let disp_item_nr   += 1
        endfor
    endif

    if g:yankring_window_use_separate == 1
        call s:YRWindowOpen(output)
    else 
        echo output
    endif
endfunction


" Used in omaps if a following character is required 
" like with motions (f,t)
function! s:YRGetChar()
    let msg = "YR:Enter character:"
    echomsg msg
    let c = getchar()
    if c =~ '^\d\+$'
        let c = nr2char(c)
        echomsg msg.c
    endif
    return c
endfunction


" Used in omaps if a following string is required 
" like with motions (/,?)
" function! s:YRGetSearch()
"     " let msg = "YR:Enter string:"
"     " echomsg msg
"     let str = input("YR:Enter string:")
"     " let str = ''
"     " while 1==1
"     "     let c = getchar()
"     "     if c =~ '^\d\+$'
"     "         let c = nr2char(c)
"     "         if c == "\<C-C>"
"     "             return c
"     "         endif
"     "         if c == "\<CR>"
"     "             break
"     "         endif
"     "         let str = str.c
"     "         echomsg msg.str
"     "     else
"     "         break
"     "     endif
"     " endwhile
"     return str
" endfunction
 

" Paste a certain item from the yankring
" If no parameter is provided, this function becomes interactive.  It will
" display the list (using YRShow) and allow the user to choose an element.
function! s:YRGetElem(...) 
    if s:yr_count == 0
        call s:YRWarningMsg('YR: yankring is empty')
        return -1
    endif

    let default_buffer = ((&clipboard=~'unnamed')?'+':'"')

    let direction = 'p'
    if a:0 > 1
        " If the user indicated to paste above or below
        " let direction = ((a:2 ==# 'P') ? 'P' : 'p')
        if a:2 =~ '\(p\|gp\|P\|gP\)'
            let direction = a:2
        endif
    endif

    " Check to see if a specific value has been provided
    let elem = 0
    if a:0 > 0
        " Ensure we get only the numeric value (trim it)
        let elem = matchstr(a:1, '\d\+')
        let elem = elem - 1
    else
        " If no parameter was supplied display the yankring
        " and prompt the user to enter the value they want pasted.
        call s:YRShow(0)

        if g:yankring_window_use_separate == 1
            " The window buffer is used instead of command line
            return
        endif

        let elem = input("Enter # to paste:")

        " Ensure we get only the numeric value (trim it)
        let elem = matchstr(elem, '\d\+')

        if elem == ''
            " They most likely pressed enter without entering a value
            return
        endif

        let elem = elem - 1
    endif

    if elem < 0 || elem >= s:yr_count
        call s:YRWarningMsg("YR: Invalid choice:".elem)
        return -1
    endif

    let default_buffer = ((&clipboard=~'unnamed')?'+':'"')
    call setreg(default_buffer
                \ , s:YRGetValElemNbr((elem), 'v')
                \ , s:YRGetValElemNbr((elem), 't')
                \ )
    exec "normal! ".direction

    " Set the previous action as a paste in case the user
    " press . to repeat
    call s:YRSetPrevOP('p', '', default_buffer, 'n')

endfunction
 

" Starting the top of the ring it will paste x items from it
function! s:YRGetMultiple(reverse_order, ...) 
    if s:yr_count == 0
        call s:YRWarningMsg('YR: yankring is empty')
        return
    endif

    " If the user provided a range, exit after that many
    " have been displayed
    let max  = 1
    if a:0 == 1
        " If no yank command has been supplied, assume it is
        " a full line yank
        let max = matchstr(a:1, '\d\+')
    endif
    if max > s:yr_count
        " Default to all items if they specified a very high value
        let max = s:yr_count
    endif

    " Base the increment on the sort order of the results
    let increment = ((a:reverse_order==0)?(1):(-1))
    if a:reverse_order == 0
        let increment = 1
        let elem = 0
    else
        let increment = -1
        let elem = (max - 1)
    endif

    if a:0 > 1
        let iter = 1
        while iter <= a:0
            let elem = (a:{iter} - 1)
            call s:YRGetElem(elem)
            let iter = iter + 1
        endwhile
    else
        while max > 0
            " Paste the first item, and move on to the next.
            " digits the element # is
            call s:YRGetElem(elem)
            let elem = elem + increment
            let max  = max - 1
        endwhile
    endif
endfunction
 

" Given a regular expression, check each element within
" the yankring, display only the matching items and prompt
" the user for which item to paste
function! s:YRSearch(...) 
    if s:yr_count == 0
        call s:YRWarningMsg('YR: yankring is empty')
        return
    endif

    let s:yr_search = ""
    " If the user provided a range, exit after that many
    " have been displayed
    if a:0 == 0 || (a:0 == 1 && a:1 == "")
        let s:yr_search = input('Enter [optional] regex:')
    else
        let s:yr_search = a:1
    endif

    if s:yr_search == ""
        " Show the entire yankring
        call s:YRShow(0)
        return
    endif

    " List is shown in order of replacement
    " assuming using previous yanks
    let output        = "--- YankRing ---\n"
    let output        = output . "Elem  Content\n"
    let valid_choices = []

    let search_result = filter(copy(s:yr_history_list), "v:val =~ '".s:yr_search."'")

    let disp_item_nr = 1

    for elem in s:yr_history_list
        if elem =~ s:yr_search
            let output  = output . s:YRDisplayElem(disp_item_nr, elem) . "\n"
            call add(valid_choices, disp_item_nr.'')
        endif
        let disp_item_nr   += 1
    endfor

    if len(valid_choices) == 0
        let output = output . "Search for [".s:yr_search."] did not match any items "
    endif

    if g:yankring_window_use_separate == 1
        call s:YRWindowOpen(output)
    else
        if len(valid_choices) > 0
            echo output
            let elem = input("Enter # to paste:")

            " Ensure we get only the numeric value (trim it)
            let elem = matchstr(elem, '\d\+')

            if elem == ''
                " They most likely pressed enter without entering a value
                return
            endif

            if index(valid_choices, elem) != -1
                exec 'YRGetElem ' . elem
            else
                " User did not choose one of the elements that were found
                " Remove leading ,
                call s:YRWarningMsg( "YR: Item[" . elem . "] not found, only valid choices are[" .
                            \ join(valid_choices, ',') .
                            \ "]"
                            \ )
                return -1
            endif

        else
            call s:YRWarningMsg( "YR: The pattern [" .
                        \ s:yr_search .
                        \ "] does not match any items in the yankring"
                        \ )
        endif
    endif

endfunction
 

" Resets the common script variables for managing the ring.
function! s:YRReset()
    call s:YRHistoryDelete()
    " Update the history file
    call s:YRHistorySave()
endfunction
 

" Clears the yankring by simply setting the # of items in it to 0.
" There is no need physically unlet each variable.
function! s:YRInit(...)
    let s:yr_next_idx              = 0
    let s:yr_last_paste_idx        = 0
    let s:yr_count                 = 0
    let s:yr_history_last_upd      = 0
    let s:yr_history_list          = []
    let s:yr_paste_dir             = 'p'

    " For the . op support
    let s:yr_prev_op_code          = ''
    let s:yr_prev_op_mode          = 'n'
    let s:yr_prev_count            = ''
    let s:yr_prev_reg              = ''
    let s:yr_prev_reg_unnamed      = ''
    let s:yr_prev_reg_small        = ''
    let s:yr_prev_reg_insert       = ''
    let s:yr_prev_reg_expres       = ''
    let s:yr_prev_clipboard        = ''
    let s:yr_prev_vis_lstart       = 0
    let s:yr_prev_vis_lend         = 0
    let s:yr_prev_vis_cstart       = 0
    let s:yr_prev_vis_cend         = 0
    let s:yr_prev_changenr         = 0
    let s:yr_prev_repeating        = 0

    " This is used to determine if the visual selection should be
    " reset prior to issuing the YRReplace
    let s:yr_prev_vis_mode         = 0

    if a:0 == 0 && g:yankring_persist == 0
        " The user wants the yankring reset each time Vim is started
        call s:YRClear()
    endif

    call s:YRHistoryRead()
endfunction
 

" Clears the yankring by simply setting the # of items in it to 0.
" There is no need physically unlet each variable.
function! s:YRClear()
    call s:YRReset()
    call s:YRInit('DoNotClear')

    " If the yankring window is open, refresh it
    call s:YRWindowUpdate()
endfunction
 

" Determine which register the user wants to use
" For example the 'a' register:  "ayy
function! s:YRRegister()
    " v:register can be blank in some (unknown) cases
    " so test for this condition and return the 
    " default register
    let user_register = ((v:register=='')?('"'):(v:register))
    if &clipboard =~ 'unnamed' && user_register == '"'
        let user_register = '+'
    endif
    return user_register
endfunction


" Allows you to push a new item on the yankring.  Useful if something
" is in the clipboard and you want to add it to the yankring.
" Or if you yank something that is not mapped.
function! s:YRPush(...) 
    let user_register = s:YRRegister()

    if a:0 > 0
        " If no yank command has been supplied, assume it is
        " a full line yank
        let user_register = ((a:1 == '') ? user_register : a:1)
    endif

    " If we are pushing something on to the yankring, add it to
    " the default buffer as well so the next item pasted will
    " be the item pushed
    let default_buffer = ((&clipboard=~'unnamed')?'+':'"')
    call setreg(default_buffer, getreg(user_register), 
                \ getregtype(user_register))

    call s:YRSetPrevOP('', '', '', 'n')
    call YRRecord(user_register)
endfunction


" Allows you to pop off any element from the yankring.
" If no parameters are provided the first element is removed.
" If a vcount is provided, that many elements are removed 
" from the top.
function! s:YRPop(...)
    if s:yr_count == 0
        call s:YRWarningMsg('YR: yankring is empty')
        return
    endif

    let v_count = 1
    if a:0 > 1 
        let v_count = a:2
    endif

    " If the user provided a parameter, remove that element 
    " from the yankring.  
    " If no parameter was provided assume the first element.
    let elem_index = 0
    if a:0 > 0
        " Get the element # from the parameter
        let elem_index = matchstr(a:1, '\d\+')
        let elem_index = elem_index - 1
    endif
    
    " If the user entered a count, then remove that many
    " elements from the ring.
    while v_count > 0 
        call s:YRMRUDel('s:yr_history_list', elem_index)
        let v_count = v_count - 1
    endwhile

    " If the yankring window is open, refresh it
    call s:YRWindowUpdate()
endfunction


" Adds this value to the yankring.
function! YRRecord(...) 

    let register = '"'
    if a:0 > 0
        " If no yank command has been supplied, assume it is
        " a full line yank
        let register = ((a:1 == '') ? register : a:1)
    endif

    " v:register can be blank in some (unknown) cases
    " if v:register == '' || v:register == '_'
    if v:register == '_'
        " Black hole register, ignore recording the operation
        return ""
    endif

    let register = ((&clipboard=~'unnamed')?'+':register)

    " let s:yr_prev_changenr    = changenr()
    if register == '"'
        " If the change has occurred via an omap, we must delay
        " the capture of the default register until this event
        " since register updates are not reflected until the 
        " omap function completes
        let s:yr_prev_reg_unnamed = getreg('"')
        let s:yr_prev_reg_small   = getreg('-')
    endif

    " Add item to list
    " This will also account for duplicates.
    call s:YRMRUAdd( 's:yr_history_list'
                \ , getreg(register)
                \ , getregtype(register) 
                \ )

    if register =~ '[+*]'
        let s:yr_prev_clipboard = @+
    endif

    " Manage the numbered registers
    if g:yankring_manage_numbered_reg == 1
        " Allow the user to define an autocmd to dynamically
        " setup their connection information.
        silent! doautocmd User YRSetNumberedReg
    endif

    " If the yankring window is open, refresh it
    call s:YRWindowUpdate()

    return ""
endfunction


" Adds this value to the yankring.
function! YRRecord3() 
    let register = '"'

    " v:register can be blank in some (unknown) cases
    " if v:register == '' || v:register == '_'
    if v:register == '_'
        " Black hole register, ignore recording the operation
        return ""
    endif

    let register = ((&clipboard=~'unnamed')?'+':register)

    if register == '"'
        " If the change has occurred via an omap, we must delay
        " the capture of the default register until this event
        " since register updates are not reflected until the 
        " omap function completes
        let s:yr_prev_reg_unnamed = getreg('"')
        let s:yr_prev_reg_small   = getreg('-')
    endif

    if s:yr_remove_omap_dot == 1
        call s:YRMapsCreate('add_only_zap_keys')
    endif

    " Add item to list
    " This will also account for duplicates.
    call s:YRMRUAdd( 's:yr_history_list'
                \ , getreg(register)
                \ , getregtype(register) 
                \ )

    if register =~ '[+*]'
        let s:yr_prev_clipboard = @+
    endif

    " Manage the numbered registers
    if g:yankring_manage_numbered_reg == 1
        " Allow the user to define an autocmd to dynamically
        " setup their connection information.
        silent! doautocmd User YRSetNumberedReg
    endif

    " If the yankring window is open, refresh it
    call s:YRWindowUpdate()

    return ""
endfunction


" Record the operation for the dot operator
function! s:YRSetPrevOP(op_code, count, reg, mode) 
    let s:yr_prev_op_code     = a:op_code
    let s:yr_prev_op_mode     = a:mode
    let s:yr_prev_count       = a:count
    let s:yr_prev_changenr    = changenr()
    let s:yr_prev_reg         = a:reg
    let s:yr_prev_reg_unnamed = getreg('"')
    let s:yr_prev_reg_small   = getreg('-')
    let s:yr_prev_reg_insert  = getreg('.')
    let s:yr_prev_vis_lstart  = line("'<")
    let s:yr_prev_vis_lend    = line("'>")
    let s:yr_prev_vis_cstart  = col("'<")
    let s:yr_prev_vis_cend    = col("'>")
    let s:yr_prev_reg_expres  = histget('=', -1)

    if a:mode == 'n'
        " In normal mode, the change has already
        " occurred, therefore we can mark the
        " actual position of the change.
        let s:yr_prev_chg_lstart  = line("'[")
        let s:yr_prev_chg_lend    = line("']")
        let s:yr_prev_chg_cstart  = col("'[")
        let s:yr_prev_chg_cend    = col("']")
    else
        " If in operator pending mode, the change
        " has not yet occurred.  Therefore we cannot
        " use the '[ and ]' markers.  But we can
        " store the current line position.
        let s:yr_prev_chg_lstart  = line(".")
        let s:yr_prev_chg_lend    = line(".")
        let s:yr_prev_chg_cstart  = col(".")
        let s:yr_prev_chg_cend    = col(".")
    endif

    " If storing the last change position (using '[, '])
    " is not good enough, then another option is to:
    " Use :redir on the :changes command
    " and grab the last item.  Store this value
    " and compare it is YRDoRepeat.
endfunction


" Adds this value to the yankring.
function! s:YRDoRepeat() 
    let dorepeat = 0

    if s:yr_has_voperator == 1
        " Let Vim handle the repeat, just capture the updates
        " as usual.
        return 0
    endif

    if s:yr_prev_op_code =~ '^c'
        " You cannot repeat change operations, let Vim's
        " standard mechanism handle these, or the user will
        " be prompted again, instead of repeating the
        " previous change.
        return 0
    endif

    if g:yankring_manage_numbered_reg == 1
        " When resetting the numbered register we are
        " must ignore the comparision of the " register.
        if s:yr_prev_reg_small  == getreg('-') &&
                    \ s:yr_prev_reg_insert == getreg('.') &&
                    \ s:yr_prev_reg_expres == histget('=', -1) &&
                    \ s:yr_prev_vis_lstart == line("'<") &&
                    \ s:yr_prev_vis_lend   == line("'>") &&
                    \ s:yr_prev_vis_cstart == col("'<") &&
                    \ s:yr_prev_vis_cend   == col("'>") &&
                    \ s:yr_prev_chg_lstart == line("'[") &&
                    \ s:yr_prev_chg_lend   == line("']") &&
                    \ s:yr_prev_chg_cstart == col("'[") &&
                    \ s:yr_prev_chg_cend   == col("']") 
            let dorepeat = 1
        endif
    else
        " Check the previously recorded value of the registers
        " if they are the same, we need to reissue the previous
        " yankring command.
        " If any are different, the user performed a command
        " command that did not involve the yankring, therefore
        " we should just issue the standard "normal! ." to repeat it.
        if s:yr_prev_reg_unnamed == getreg('"') &&
                    \ s:yr_prev_reg_small  == getreg('-') &&
                    \ s:yr_prev_reg_insert == getreg('.') &&
                    \ s:yr_prev_reg_expres == histget('=', -1) &&
                    \ s:yr_prev_vis_lstart == line("'<") &&
                    \ s:yr_prev_vis_lend   == line("'>") &&
                    \ s:yr_prev_vis_cstart == col("'<") &&
                    \ s:yr_prev_vis_cend   == col("'>")
            let dorepeat = 1
        endif
        if dorepeat == 1 && s:yr_prev_op_mode == 'n'
            " Hmm, not sure why I was doing this now
            " so I will remove it
            " let dorepeat = 0
            " if s:yr_prev_chg_lstart == line("'[") &&
            "             \ s:yr_prev_chg_lend   == line("']") &&
            "             \ s:yr_prev_chg_cstart == col("'[") &&
            "             \ s:yr_prev_chg_cend   == col("']") 
            "     let dorepeat = 1
            " endif
        elseif dorepeat == 1 && s:yr_prev_op_mode == 'o'
            " Hmm, not sure why I was doing this now
            " so I will remove it
            " let dorepeat = 0
            " if s:yr_prev_chg_lstart == line("'[") &&
            "             \ s:yr_prev_chg_lend   == line("']") &&
            "             \ s:yr_prev_chg_cstart == col("'[") &&
            "             \ s:yr_prev_chg_cend   == col("']") 
            "     let dorepeat = 1
            " endif
        endif
    endif

    " " If another change has happened that was not part of the
    " " yankring we cannot replay it (from the yankring).  Use
    " " the standard ".".
    " " If the previous op was a change, do not use the yankring
    " " to repeat it.
    " " changenr() is buffer specific, so anytime you move to
    " " a different buffer you will definitely perform a 
    " " standard "."
    " " Any previous op that was a change, must be replaced using "."
    " " since we do not want the user prompted to enter text again.
    " if s:yr_prev_changenr == changenr() && s:yr_prev_op_code !~ '^c'
    "     let dorepeat = 1
    " endif

    " If we are going to repeat check to see if the
    " previous command was a yank operation.  If so determine
    " if yank operations are allowed to be repeated.
    if dorepeat == 1 && s:yr_prev_op_code =~ '^y'
        " This value be default is set based on cpoptions.
        if g:yankring_dot_repeat_yank == 0
            let dorepeat = 0
        endif
    endif
    return dorepeat
endfunction


" Manages the Vim's numbered registers
function! s:YRSetNumberedReg() 

    let i = 0

    while i <= 10
        if i > s:yr_count
            break
        endif

        call setreg( (i)
                    \ , s:YRGetValElemNbr((i),'v')
                    \ , s:YRGetValElemNbr((i),'t')
                    \ )
        let i += 1
    endwhile

    " There are a few actions that Vim automatically takes
    " when modifying the numbered registers.
    " Modifying register 1 - changes the named register.
    " It is impossible to set register 2 to a value, since Vim will change it.

    " This will at least preserve the default register
    let @" = @0
endfunction


" This internal function will add and subtract values from a starting
" point and return the correct element number.  It takes into account
" the circular nature of the yankring.
function! s:YRGetNextElem(start, iter) 

    let needed_elem = a:start + a:iter

    " The yankring is a ring, so if an element is
    " requested beyond the number of elements, we
    " must wrap around the ring.
    if needed_elem > s:yr_count
        let needed_elem = needed_elem % s:yr_count
    endif

    if needed_elem == 0
        " Can happen at the end or beginning of the ring
        if a:iter == -1
            " Wrap to the bottom of the ring
            let needed_elem = s:yr_count
        else
            " Wrap to the top of the ring
            let needed_elem = 1
        endif
    elseif needed_elem < 1
        " As we step backwards through the ring we could ask for a negative
        " value, this will wrap it around to the end
        let needed_elem = s:yr_count
    endif

    return needed_elem

endfunction


" Lets Vim natively perform the operation and then stores what
" was yanked (or deleted) into the yankring.
" Supports this for example -   5"ayy
"
" This is a legacy function now since the release of Vim 7.2
" and the use of omaps with YankRing 5.0 and above.  
" If Vim 7.1 has patch205, then the new omaps and the v:operator
" variable is used instead.
function! s:YRYankCount(...) range

    let user_register = s:YRRegister()
    let v_count = v:count

    " Default yank command to the entire line
    let op_code = 'yy'
    if a:0 > 0
        " If no yank command has been supplied, assume it is
        " a full line yank
        let op_code = ((a:1 == '') ? op_code : a:1)
    endif

    if op_code == '.'
        if s:YRDoRepeat() == 1
            if s:yr_prev_op_code != ''
                let op_code       = s:yr_prev_op_code
                let v_count       = s:yr_prev_count
                let user_register = s:yr_prev_reg
            endif
        else
            " Set this flag so that YRRecord will
            " ignore repeats
            let s:yr_prev_repeating = 1
            exec "normal! ."
            return
        endif
    else
        let s:yr_prev_repeating = 0
    endif

    " Supports this for example -   5"ayy
    " A delete operation will still place the items in the
    " default registers as well as the named register
    exec "normal! ".
            \ ((v_count > 0)?(v_count):'').
            \ (user_register=='"'?'':'"'.user_register).
            \ op_code

    if user_register == '_'
        " Black hole register, ignore recording the operation
        return
    endif
    
    call s:YRSetPrevOP(op_code, v_count, user_register, 'n')

    call YRRecord(user_register)
endfunction
 

" Handles ranges.  There are visual ranges and command line ranges.
" Visual ranges are easy, since we pass through and let Vim deal
" with those directly.
" Command line ranges means we must yank the entire line, and not
" just a portion of it.
function! s:YRYankRange(do_delete_selection, ...) range

    let user_register  = s:YRRegister()
    let default_buffer = ((&clipboard=~'unnamed')?'+':'"')

    " Default command mode to normal mode 'n'
    let cmd_mode = 'n'
    if a:0 > 0
        " Change to visual mode, if command executed via
        " a visual map
        let cmd_mode = ((a:1 == 'v') ? 'v' : 'n')
    endif

    if cmd_mode == 'v' 
        " We are yanking either an entire line, or a range 
        exec "normal! gv".
                    \ (user_register==default_buffer?'':'"'.user_register).
                    \ 'y'
        if a:do_delete_selection == 1
            exec "normal! gv".
                        \ (user_register==default_buffer?'':'"'.user_register).
                        \ 'd'
        endif
    else
        " In normal mode, always yank the complete line, since this
        " command is for a range.  YRYankCount is used for parts
        " of a single line
        if a:do_delete_selection == 1
            exec a:firstline . ',' . a:lastline . 'delete '.user_register
        else
            exec a:firstline . ',' . a:lastline . 'yank ' . user_register
        endif
    endif

    if user_register == '_'
        " Black hole register, ignore
        return
    endif
    
    call s:YRSetPrevOP('', '', user_register, 'n')
    call YRRecord(user_register)
endfunction
 

" Paste from either the yankring or from a specified register
" Optionally a count can be provided, so paste the same value 10 times 
function! s:YRPaste(replace_last_paste_selection, nextvalue, direction, ...) 
    " Disabling the yankring removes the default maps.
    " But there are some maps the user can create on their own, and 
    " these would most likely call this function.  So place an extra
    " check and display a message.
    if g:yankring_enabled == 0
        call s:YRWarningMsg(
                    \ 'YR: The yankring is currently disabled, use YRToggle.'
                    \ )
        return
    endif
    

    let user_register  = s:YRRegister()
    let default_buffer = ((&clipboard =~ 'unnamed')?'+':'"')
    let v_count        = v:count

    " Default command mode to normal mode 'n'
    let cmd_mode = 'n'
    if a:0 > 0
        " Change to visual mode, if command executed via
        " a visual map
        let cmd_mode = ((a:1 == 'v') ? 'v' : 'n')
    endif

    " User has decided to bypass the yankring and specify a specific 
    " register
    if user_register != default_buffer
        if a:replace_last_paste_selection == 1
            call s:YRWarningMsg( 'YR: A register cannot be specified in replace mode' )
            return
        else
            " Check for the expression register, in this special case
            " we must copy it's evaluation into the default buffer and paste
            if user_register == '='
                " Save the default register since Vim will only
                " allow the expression register to be pasted once
                " and will revert back to the default buffer
                let save_default_reg = @"
                call setreg(default_buffer, eval(histget('=', -1)) )
            else
                let user_register = '"'.user_register
            endif
            exec "normal! ".
                        \ ((cmd_mode=='n') ? "" : "gv").
                        \ ((v_count > 0)?(v_count):'').
                        \ ((user_register=='=')?'':user_register).
                        \ a:direction
            if user_register == '='
                let @" = save_default_reg
            endif
            " In this case, we have bypassed the yankring
            " If the user hits next or previous we want the
            " next item pasted to be the top of the yankring.
            let s:yr_last_paste_idx = 0
        endif
        let s:yr_paste_dir     = a:direction
        let s:yr_prev_vis_mode = ((cmd_mode=='n') ? 0 : 1)
        return
    endif

    " Try to second guess the user to make these mappings less intrusive.
    " If the user hits paste, compare the contents of the paste register
    " to the current entry in the yankring.  If they are different, lets
    " assume the user wants the contents of the paste register.
    " So if they pressed [yt ] (yank to space) and hit paste, the yankring
    " would not have the word in it, so assume they want the word pasted.
    if a:replace_last_paste_selection != 1 
        if s:yr_count > 0 || (default_buffer == '+' && len(@+) == 0)
            " Only check the default buffer is the user wants us to.
            " This was necessary prior to version 4.0 since we did not 
            " capture as many items as 4.0 and above does. (A. Budden)
            if g:yankring_paste_check_default_buffer == 1 && 
                        \ getreg(default_buffer) != s:yr_prev_reg_unnamed
                " The user has performed a yank / delete operation
                " outside of the yankring maps.  First, add this 
                " value to the yankring.
                call YRRecord(default_buffer)
                " Now, use the most recently yanked text, rather than the
                " value from the yankring.
                exec "normal! ".
                            \ ((cmd_mode=='n') ? "" : "gv").
                            \ ((v_count > 0)?(v_count):'').
                            \ a:direction
                let s:yr_paste_dir     = a:direction
                let s:yr_prev_vis_mode = ((cmd_mode=='n') ? 0 : 1)

                " In this case, we have bypassed the yankring
                " If the user hits next or previous we want the
                " next item pasted to be the top of the yankring.
                let s:yr_last_paste_idx = 0
                return
            endif
        else
            exec "normal! ".
                        \ ((cmd_mode=='n') ? "" : "gv").
                        \ ((v_count > 0)?(v_count):'').
                        \ a:direction
            let s:yr_paste_dir     = a:direction
            let s:yr_prev_vis_mode = ((cmd_mode=='n') ? 0 : 1)
            return
        endif
    endif

    if s:yr_count == 0 || (default_buffer == '+' && len(@+) == 0)
        " Nothing to paste
        return
    endif

    if a:replace_last_paste_selection == 1
        " Replacing the previous put
        let start = line("'[")
        let end = line("']")

        if start != line('.')
            call s:YRWarningMsg( 'YR: You must paste text first, before you can replace' )
            return
        endif

        if start == 0 || end == 0
            return
        endif

        " If a count was provided (ie 5<C-P>), multiply the 
        " nextvalue accordingly and position the next paste index
        let which_elem = a:nextvalue * ((v_count > 0)?(v_count):1) * -1
        let s:yr_last_paste_idx = s:YRGetNextElem(
		    \ s:yr_last_paste_idx, which_elem
		    \ )

        let save_reg            = getreg(default_buffer)
        let save_reg_type       = getregtype(default_buffer)
        call setreg( default_buffer
                    \ , s:YRGetValElemNbr((s:yr_last_paste_idx-1),'v')
                    \ , s:YRGetValElemNbr((s:yr_last_paste_idx-1),'t')
                    \ )

        " First undo the previous paste
        exec "normal! u"
        " Check if the visual selection should be reselected
        " Next paste the correct item from the ring
        " This is done as separate statements since it appeared that if 
        " there was nothing to undo, the paste never happened.
        exec "normal! ".
                    \ ((s:yr_prev_vis_mode==0) ? "" : "gv").
                    \ s:yr_paste_dir
        call setreg(default_buffer, save_reg, save_reg_type)
        call s:YRSetPrevOP('', '', '', 'n')
    else
        " User hit p or P
        " Supports this for example -   5"ayy
        " And restores the current register
        let save_reg            = getreg(default_buffer)
        let save_reg_type       = getregtype(default_buffer)
        let s:yr_last_paste_idx = 1
        call setreg(default_buffer
                    \ , s:YRGetValElemNbr(0,'v')
                    \ , s:YRGetValElemNbr(0,'t')
                    \ )
        exec "normal! ".
                    \ ((cmd_mode=='n') ? "" : "gv").
                    \ (
                    \ ((v_count > 0)?(v_count):'').
                    \ a:direction
                    \ )
        call setreg(default_buffer, save_reg, save_reg_type)
        call s:YRSetPrevOP(
                    \ a:direction
                    \ , v_count
                    \ , default_buffer
                    \ , 'n'
                    \ )
        let s:yr_paste_dir     = a:direction
        let s:yr_prev_vis_mode = ((cmd_mode=='n') ? 0 : 1)
    endif

endfunction
 

" Handle any omaps
function! YRMapsExpression(sid, motion, ...)
    let cmds     = a:motion
    " echomsg "YRMapsE:".localtime()
    " echomsg "YRMapsE 1:".cmds.":".v:operator.":".s:yr_maps_created_zap

    if (a:motion =~ '\.' && s:yr_remove_omap_dot == 1) || a:motion =~ '@'
        " If we are repeating a series of commands we must
        " unmap the _zap_ keys so that the user is not
        " prompted when a command is replayed.
        " These maps must be re-instated in YRRecord3()
        " after the action of the replay is completed.
        call s:YRMapsDelete('remove_only_zap_keys')
    endif

    " Check if we are in operator-pending mode
    if a:motion =~ '\('.substitute(g:yankring_zap_keys, ' ', '\\|', 'g').'\)'
        if a:motion =~ '\(/\|?\)'
            let zapto = (a:0==0 ? "" : input("YR:Enter string:"))
            if zapto != ""
                let zapto = zapto . "\<CR>"
            else
                let zapto = "\<C-C>"
            endif
        else
            let zapto = (a:0==0 ? "" : s:YRGetChar())
        endif

        if zapto == "\<C-C>"
            " Abort if the user hits Control C
            call s:YRWarningMsg( "YR:Aborting command:".v:operator.a:motion )
            return "\<C-C>"
        endif

        let cmds = cmds . zapto
    endif

    " There are a variety of commands which do not change the
    " registers, so these operators should be ignored when
    " determining which operations to record
    " Simple example is '=' which simply formats the 
    " the selected text.
    if ' \('.escape(join(split(g:yankring_ignore_operator), '\|'), '/.*~$^[]' ).'\) ' !~ escape(v:operator, '/.*~$^[]') 
        " Check if we are performing an action that will
        " take us into insert mode
        if '[cCsS]' !~ escape(v:operator, '/.*~$^[]') && a:motion !~ '@'
        " if '[cCsS]' !~ escape(v:operator, '/.*~$^[]')
            " If we have not entered insert mode, feed the call
            " to record the current change when the function ends.
            " This is necessary since omaps do not update registers
            " until the function completes.
            " The InsertLeave event will handle the motions
            " that place us in insert mode and record the
            " changes when insert mode ends.
            let cmds .= a:sid. "yrrecord"
        endif
    endif
 
    " This will not work since we are already executing an expression
    " if a:motion =~ '@'
    "     let cmds = 'normal! ' . cmds
    " endif

    " echomsg "YRMapsE 5:".a:motion.":'".cmds."':".s:yr_maps_created_zap
    return cmds
 
endfunction
 

" Handle macros (@).
" This routine is not used, YRMapsExpression is used to
" handle the @ symbol.
" function! s:YRMapsMacro(bang, ...) range
"     " If we are repeating a series of commands we must
"     " unmap the _zap_ keys so that the user is not
"     " prompted when a command is replayed.
"     " These maps must be re-instated in YRRecord3()
"     " after the action of the replay is completed.
"     call s:YRMapsDelete('remove_only_zap_keys')
" 
"     " let zapto = (a:0==0 ? "" : s:YRGetChar())
"     let zapto = s:YRGetChar()
" 
"     if zapto == "\<C-C>"
"         " Abort if the user hits Control C
"         call s:YRWarningMsg( "YR:Aborting command:".v:operator.a:motion )
"         return ""
"     endif
" 
"     let v_count    = v:count
"     " If no count was specified it will have a value of 0
"     " so set it to at least 1
"     let v_count = ((v_count > 0)?(v_count):'')
" 
"     let range = ''
"     if a:firstline != a:lastline
"         let rannge = a:firstline.','.a:lastline
"     endif
" 
"     let cmd = range."normal! ".v_count.'@'.zapto
"     " DEBUG
"     " echomsg cmd
"     exec cmd
" 
"     call s:YRMapsCreate('add_only_zap_keys')
" endfunction
 

" Create the default maps
function! s:YRMapsCreate(...)
    " 7.1.patch205 introduces the v:operator function which was 
    " essential to gain the omap support.
    if s:yr_has_voperator == 1
        let s:yr_remove_omap_dot   = 1
        for key in split(g:yankring_zap_keys)
            try
                if key != '@'
                    exec 'omap <expr>' key 'YRMapsExpression("<SID>", "'. key. '", 1)'
                endif
            catch
            endtry
        endfor
    endif

    silent! nmap <expr> @ YRMapsExpression("<SID>", "@", "1")

    let s:yr_maps_created_zap = 1

    if a:0 > 0
        " We have only removed the _zap_ keys temporarily
        " so abandon further changes.
        return
    endif

    " 7.1.patch205 introduces the v:operator function which was essential
    " to gain the omap support.
    if s:yr_has_voperator == 1
        let s:yr_remove_omap_dot   = 1
        " Set option to add and remove _zap_ keys when
        " repeating commands
        let o_maps = split(g:yankring_o_keys)
        " Loop through and prompt the user for all buffer connection parameters.
        for key in o_maps
            exec 'omap <expr>' key 'YRMapsExpression("<SID>", "'. escape(key,'\"'). '")'
        endfor
    endif

    " Iterate through a space separated list of mappings and create
    " calls to the YRYankCount function
    let n_maps = split(g:yankring_n_keys)
    " Loop through and prompt the user for all buffer connection parameters.
    for key in n_maps
        " exec 'nnoremap <silent>'.key." :<C-U>YRYankCount '".key."'<CR>"
        " exec 'nnoremap <silent>'.key." :<C-U>YRYankCount '".key."'<CR>"
        " Andy Wokula's suggestion
        exec 'nmap' key key."<SID>yrrecord"
    endfor

    if g:yankring_map_dot == 1
        if s:yr_has_voperator == 1
            nmap <expr> . YRMapsExpression("<SID>", ".")
        else
            nnoremap <silent> . :<C-U>YRYankCount '.'<CR>
        endif
    endif

    if g:yankring_v_key != ''
        exec 'xnoremap <silent>'.g:yankring_v_key." :YRYankRange 'v'<CR>"
    endif
    if g:yankring_del_v_key != ''
        for v_map in split(g:yankring_del_v_key)
            if strlen(v_map) > 0
                try
                    exec 'xnoremap <silent>'.v_map." :YRDeleteRange 'v'<CR>"
                catch
                endtry
            endif
        endfor
    endif
    if g:yankring_paste_n_bkey != ''
        exec 'nnoremap <silent>'.g:yankring_paste_n_bkey." :<C-U>YRPaste 'P'<CR>"
        if g:yankring_paste_using_g == 1
            exec 'nnoremap <silent> g'.g:yankring_paste_n_bkey." :<C-U>YRPaste 'gP'<CR>"
        endif
    endif
    if g:yankring_paste_n_akey != ''
        exec 'nnoremap <silent>'.g:yankring_paste_n_akey." :<C-U>YRPaste 'p'<CR>"
        if g:yankring_paste_using_g == 1
            exec 'nnoremap <silent> g'.g:yankring_paste_n_akey." :<C-U>YRPaste 'gp'<CR>"
        endif
    endif
    if g:yankring_paste_v_bkey != ''
        exec 'xnoremap <silent>'.g:yankring_paste_v_bkey." :<C-U>YRPaste 'P', 'v'<CR>"
    endif
    if g:yankring_paste_v_akey != ''
        exec 'xnoremap <silent>'.g:yankring_paste_v_akey." :<C-U>YRPaste 'p', 'v'<CR>"
    endif
    if g:yankring_replace_n_pkey != ''
        exec 'nnoremap <silent>'.g:yankring_replace_n_pkey." :<C-U>YRReplace '-1', 'P'<CR>"
    endif
    if g:yankring_replace_n_nkey != ''
        exec 'nnoremap <silent>'.g:yankring_replace_n_nkey." :<C-U>YRReplace '1', 'p'<CR>"
    endif

    let g:yankring_enabled    = 1
    let s:yr_maps_created     = 1

    if exists('*YRRunAfterMaps') 
        " This will allow you to override the default maps if necessary
        call YRRunAfterMaps()
    endif
endfunction
 

" Create the default maps
function! s:YRMapsDelete(...)

    let o_maps = split(g:yankring_zap_keys)
    for key in o_maps
        try
            if key != '@'
                silent! exec 'ounmap' key
            endif
        catch
        endtry
    endfor

    let s:yr_maps_created_zap = 0

    if a:0 > 0
        " We have only removed the _zap_ keys temporarily
        " so abandon further changes.
        return
    endif

    " Iterate through a space separated list of mappings and create
    " calls to an appropriate YankRing function
    let n_maps = split(g:yankring_n_keys)
    " Loop through and prompt the user for all buffer connection parameters.
    for key in n_maps
        try
            silent! exec 'nunmap' key
        catch
        endtry
    endfor

    let o_maps = split(g:yankring_o_keys)
    for key in o_maps
        try
            silent! exec 'ounmap' key
        catch
        endtry
    endfor

    if g:yankring_map_dot == 1 
        silent! exec "nunmap ."
    endif
    if g:yankring_v_key != ''
        silent! exec 'vunmap '.g:yankring_v_key
    endif
    if g:yankring_del_v_key != ''
        for v_map in split(g:yankring_del_v_key)
            if strlen(v_map) > 0
                try
                    silent! exec 'vunmap '.v_map
                catch
                endtry
            endif
        endfor
    endif
    if g:yankring_paste_n_bkey != ''
        silent! exec 'nunmap '.g:yankring_paste_n_bkey
        if g:yankring_paste_using_g == 1
            silent! exec 'nunmap g'.g:yankring_paste_n_bkey
        endif
    endif
    if g:yankring_paste_n_akey != ''
        silent! exec 'nunmap '.g:yankring_paste_n_akey
        if g:yankring_paste_using_g == 1
            silent! exec 'nunmap g'.g:yankring_paste_n_akey
        endif
    endif
    if g:yankring_paste_v_bkey != ''
        silent! exec 'vunmap '.g:yankring_paste_v_bkey
    endif
    if g:yankring_paste_v_akey != ''
        silent! exec 'vunmap '.g:yankring_paste_v_akey
    endif
    if g:yankring_replace_n_pkey != ''
        silent! exec 'nunmap '.g:yankring_replace_n_pkey
    endif
    if g:yankring_replace_n_nkey != ''
        silent! exec 'nunmap '.g:yankring_replace_n_nkey
    endif

    silent! exec 'nunmap @'

    let g:yankring_enabled    = 0
    let s:yr_maps_created     = 0
endfunction

function! s:YRGetValElemNbr( position, type )
    let needed_elem = a:position

    " The List which contains the items in the yankring
    " history is also ordered, most recent at the top
    let elem = s:YRMRUGet('s:yr_history_list', needed_elem)

    if a:type == 't'
        let elem = matchstr(elem, '^.*,\zs.*$')
    else
        let elem = matchstr(elem, '^.*\ze,.*$')
        if s:yr_history_version == 'v1'
            " Match three @@@ in a row as long as it is not
            " preceeded by a @@@            
            " v1
            let elem = substitute(elem, s:yr_history_v1_nl_pat, "\n", 'g')
            let elem = substitute(elem, '\\@', '@', 'g')
        else
            let elem = substitute(elem, s:yr_history_v2_nl_pat, "\n", 'g')
        endif
    endif

    return elem
endfunction

function! s:YRMRUReset( mru_list )
    let {a:mru_list} = []

    return 1
endfunction

function! s:YRMRUSize( mru_list )
    return len({a:mru_list})
endfunction

function! s:YRMRUElemFormat( element, element_type )
    let elem    = a:element
    if g:yankring_max_element_length != 0
        let elem    = strpart(a:element, 0, g:yankring_max_element_length)
        if (g:yankring_warn_on_truncate > 0)
            let bytes = len (a:element) - len(elem)
            if (bytes > 0)
                call s:YRWarningMsg("Yankring truncated its element by ".
                                        \ bytes.
                                        \ " bytes due to a g:yankring_max_element_length of ".
                                        \ g:yankring_max_element_length
                                        \ )
            endif
        endif
    endif
    if s:yr_history_version == 'v1'
        let elem    = escape(elem, '@')
        let elem    = substitute(elem, "\n", s:yr_history_v1_nl, 'g')
    else
        let elem    = substitute(elem, "\n", s:yr_history_v2_nl, 'g')
    endif
    " Append the regtype to the end so we have it available
    let elem    = elem.",".a:element_type

    return elem
endfunction

function! s:YRMRUHas( mru_list, find_str )
    " This function will find a string and return the element #
    let find_idx = index({a:mru_list}, a:find_str)

    return find_idx
endfunction

function! s:YRMRUGet( mru_list, position )
    " This function will return the value of the item at a:position
    " Find the value of one element
    let value = get({a:mru_list}, a:position, -2)

    return value
endfunction

function! s:YRMRUAdd( mru_list, element, element_type )
    " Only add new items if they do not already exist in the MRU.
    " If the item is found, move it to the start of the MRU.
    let found   = -1
    " let elem    = a:element
    " if g:yankring_max_element_length != 0
    "     let elem    = strpart(a:element, 0, g:yankring_max_element_length)
    " endif
    " if s:yr_history_version == 'v1'
    "     let elem    = escape(elem, '@')
    "     let elem    = substitute(elem, "\n", s:yr_history_v1_nl, 'g')
    " else
    "     let elem    = substitute(elem, "\n", s:yr_history_v2_nl, 'g')
    " endif
    " " Append the regtype to the end so we have it available
    " let elem    = elem.",".a:element_type

    if strlen(a:element) < g:yankring_min_element_length
        return 1
    endif

    let elem = s:YRMRUElemFormat(a:element, a:element_type)

    " Refresh the List
    call s:YRHistoryRead()

    let found   = s:YRMRUHas(a:mru_list, elem)

    " Special case for efficiency, if it is first item in the 
    " List, do nothing
    if found != 0
        if found != -1
            " Remove found item since we will add it to the top
            call remove({a:mru_list}, found)
        endif
        call insert({a:mru_list}, elem, 0)
        call s:YRHistorySave()
    endif

    return 1
endfunction

function! s:YRMRUDel( mru_list, elem_nbr )
    if a:elem_nbr >= 0 && a:elem_nbr < s:yr_count 
        call remove({a:mru_list}, a:elem_nbr)
        call s:YRHistorySave()
    endif

    return 1
endfunction

function! s:YRHistoryDelete()
    let s:yr_history_list = []
    let yr_filename       = s:yr_history_file_{s:yr_history_version}

    if filereadable(yr_filename)
        let rc = delete(yr_filename)
        if rc != 0
            call s:YRErrorMsg(
                        \ 'YRHistoryDelete: Unable to delete the yankring history file: '.
                        \ yr_filename
                        \ )
        endif
    endif

    return 0
endfunction 

function! s:YRHistoryRead()
    let refresh_needed  = 1
    let yr_history_list = []
    let yr_filename     = s:yr_history_file_{s:yr_history_version}

    if filereadable(yr_filename)
        let last_upd = getftime(yr_filename)

        if s:yr_history_last_upd != 0 && last_upd <= s:yr_history_last_upd
            let refresh_needed = 0
        endif

        if refresh_needed == 1
            let s:yr_history_list = readfile(yr_filename)
            let s:yr_history_last_upd = last_upd
            let s:yr_count = len(s:yr_history_list)
            return
        else
            return
        endif
    else
        if s:yr_history_version == 'v2'
            " Check to see if an upgrade is required
            " else, let the empty yr_history_list be returned.
            if filereadable(s:yr_history_file_v1)
                " Perform upgrade to v2 of the history file
                call s:YRHistoryUpgrade('v1')
                return
            endif
        endif
    endif

    let s:yr_history_list = yr_history_list
    call s:YRHistorySave()

endfunction 

function! s:YRHistorySave()
    let yr_filename     = s:yr_history_file_{s:yr_history_version}

    if len(s:yr_history_list) > g:yankring_max_history
        " Remove items which exceed the max # specified
        call remove(s:yr_history_list, g:yankring_max_history)
    endif

    let rc = writefile(s:yr_history_list, yr_filename)

    if rc == 0
        let s:yr_history_last_upd = getftime(yr_filename)
        let s:yr_count = len(s:yr_history_list)
    else
        call s:YRErrorMsg(
                    \ 'YRHistorySave: Unable to save yankring history file: '.
                    \ yr_filename
                    \ )
    endif
endfunction 

function! s:YRHistoryUpgrade(version)
    if a:version == 'v1'
        if filereadable(s:yr_history_file_v1)
            let v1_list = readfile(s:yr_history_file_v1)
            let v2_list = []
            for elem in v1_list
                " Restore from version 1
                let elem = substitute(elem, s:yr_history_v1_nl_pat, "\n", 'g')
                let elem = substitute(elem, '\\@', '@', 'g')
                " Encode to version 2
                let elem = substitute(elem, "\n", s:yr_history_v2_nl, 'g')
                call add(v2_list, elem)
            endfor
            let s:yr_history_list = v2_list
            call s:YRHistorySave()
            call s:YRWarningMsg(
                        \ "YR:History file:".
                        \ s:yr_history_file_v1.
                        \ ' has been upgraded.'
                        \ )
        endif
    endif
endfunction 

" YRWindowUpdate
" Checks if the yankring window is already open.
" If it is, it will refresh it.
function! s:YRWindowUpdate()
    let orig_win_bufnr = bufwinnr('%')

    " Switch to the yankring buffer
    " only if it is already visible
    if bufwinnr(s:yr_buffer_id) != -1
        call s:YRShow(0)
        " Switch back to the original buffer
        exec orig_win_bufnr . "wincmd w"
    endif
endfunction

" YRWindowStatus
" Displays a brief command list and option settings.
" It also will toggle the Help text.
function! s:YRWindowStatus(show_help)
    let full_help      = 0
    let orig_win_bufnr = bufwinnr('%')
    let yr_win_bufnr   = bufwinnr(s:yr_buffer_id)

    if yr_win_bufnr == -1
        " Do not update the window status since the
        " yankring is not currently displayed.
        return ""
    endif
    " Switch to the yankring buffer
    if orig_win_bufnr != yr_win_bufnr 
        " If the buffer is visible, switch to it
        exec yr_win_bufnr . "wincmd w"
    endif

    let msg = 'AutoClose='.g:yankring_window_auto_close.
                \ ';ClipboardMonitor='.g:yankring_clipboard_monitor.
                \ ';Cmds:<enter>,[g]p,[g]P,d,r,s,a,c,u,R,q,<space>;Help=?'.
                \ (s:yr_search==""?"":';SearchRegEx='.s:yr_search)

    if s:yr_has_voperator == 0
        let msg = msg . "\nYankRing has limited functionality without Vim 7.2 or higher"
    endif

    " Toggle help by checking the first line of the buffer
    if a:show_help == 1 && getline(1) !~ 'selection'
        let full_help = 1
        let msg = 
                    \ '" <enter>      : [p]aste selection'."\n".
                    \ '" double-click : [p]aste selection'."\n".
                    \ '" [g]p         : [g][p]aste selection'."\n".
                    \ '" [g]P         : [g][P]aste selection'."\n".
                    \ '" r            : [p]aste selection in reverse order'."\n".
                    \ '" s            : [s]earch the yankring for text'."\n".
                    \ '" u            : [u]pdate display show YankRing'."\n".
                    \ '" R            : [R]egisters display'."\n".
                    \ '" a            : toggle [a]utoclose setting'."\n".
                    \ '" c            : toggle [c]lipboard monitor setting'."\n".
                    \ '" q            : [q]uit / close the yankring window'."\n".
                    \ '" ?            : Remove help text'."\n".
                    \ '" <space>      : toggles the width of the window'."\n".
                    \ '" Visual mode is supported for above commands'."\n".
                    \ msg
    endif 

    let saveMod = &modifiable

    " Go to the top of the buffer and remove any previous status
    " Use the blackhole register so it does not affect the yankring
    setlocal modifiable
    exec 0
    silent! exec 'norm! "_d/^---'."\n"
    call histdel("search", -1)

    silent! 0put =msg

    " Erase it's contents to the blackhole
    silent! exec '%g/^\s*$/delete _'
    call histdel("search", -1)

    call cursor(1,1)
    if full_help == 0
        call search('^\d', 'W')
    endif

    let &modifiable = saveMod

    if orig_win_bufnr != s:yr_buffer_id 
        exec orig_win_bufnr . "wincmd w"
    endif
endfunction

" YRWindowOpen
" Display the Most Recently Used file list in a temporary window.
function! s:YRWindowOpen(results)

    " Setup the cpoptions properly for the maps to work
    let old_cpoptions = &cpoptions
    set cpoptions&vim
    setlocal cpoptions-=a,A

    " Save the current buffer number. The yankring will switch back to
    " this buffer when an action is taken.
    let s:yr_buffer_last       = bufnr('%')
    let s:yr_buffer_last_winnr = winnr()

    if bufwinnr(s:yr_buffer_id) == -1
        if g:yankring_window_use_horiz == 1
            if g:yankring_window_use_bottom == 1
                let location = 'botright'
            else
                let location = 'topleft'
                " Creating the new window will offset all other
                " window numbers.  Account for that so we switch
                " back to the correct window.
                let s:yr_buffer_last_winnr = s:yr_buffer_last_winnr + 1
            endif
            let win_size = g:yankring_window_height
        else
            " Open a horizontally split window. Increase the window size, if
            " needed, to accomodate the new window
            if g:yankring_window_width &&
                        \ &columns < (80 + g:yankring_window_width)
                " one extra column is needed to include the vertical split
                let &columns             = &columns + g:yankring_window_width + 1
                let s:yr_winsize_chgd = 1
            else
                let s:yr_winsize_chgd = 0
            endif

            if g:yankring_window_use_right == 1
                " Open the window at the rightmost place
                let location = 'botright vertical'
            else
                " Open the window at the leftmost place
                let location = 'topleft vertical'
                " Creating the new window will offset all other
                " window numbers.  Account for that so we switch
                " back to the correct window.
                let s:yr_buffer_last_winnr = s:yr_buffer_last_winnr + 1
            endif
            let win_size = g:yankring_window_width
        endif

        " Special consideration was involved with these sequence
        " of commands.  
        "     First, split the current buffer.
        "     Second, edit a new file.
        "     Third record the buffer number.
        " If a different sequence is followed when the yankring
        " buffer is closed, Vim's alternate buffer is the yanking
        " instead of the original buffer before the yankring 
        " was shown.
        let cmd_mod = ''
        if v:version >= 700
            let cmd_mod = 'keepalt '
        endif
        exec 'silent! ' . cmd_mod . location . ' ' . win_size . 'split ' 

        " Using :e and hide prevents the alternate buffer
        " from being changed.
        exec ":e " . escape(s:yr_buffer_name, ' ')
        " Save buffer id
        let s:yr_buffer_id = bufnr('%') + 0
    else
        " If the buffer is visible, switch to it
        exec bufwinnr(s:yr_buffer_id) . "wincmd w"
    endif

    " Perform a double check to ensure we have entered the correct
    " buffer since we don't want to do the %d_ in the wrong buffer!
    if (bufnr('%') + 0) != s:yr_buffer_id
        call s:YRWarningMsg(
                    \ "YR:Failed to change to the yankring buffer, please contact author id:".
                    \ s:yr_buffer_id.
                    \ ' last:'.s:yr_buffer_last
                    \ )
        return -1
    endif

    " Mark the buffer as scratch
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nowrap
    setlocal nonumber
    setlocal nobuflisted
    setlocal noreadonly
    setlocal modifiable

    " set up syntax highlighting
    syn match yankringTitle #^--- YankRing ---$#hs=s+4,he=e-4
    syn match yankringHeaders #^Elem  Content$#
    syn match yankringItemNumber #^\d\+#

    syn match yankringKey #^AutoClose.*<enter>#hs=e-6
    syn match yankringKey #^AutoClose.*\[g\]p#hs=e-3 contains=yankringKey
    syn match yankringKey #^AutoClose.*\[p\]P#hs=e-3 contains=yankringKey
    syn match yankringKey #^AutoClose.*,d,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,r,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,s,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,a,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,c,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,u,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*,q,#hs=e-1,he=e-1 contains=yankringKey
    syn match yankringKey #^AutoClose.*<space>#hs=e-6 contains=yankringKey
    syn match yankringKey #^AutoClose.*?$#hs=e contains=yankringKey

    syn match yankringKey #^".*:#hs=s+1,he=e-1
    syn match yankringHelp #^".*$# contains=yankringKey

    hi link yankringTitle directory
    hi link yankringHeaders keyword
    hi link yankringItemNumber constant
    hi link yankringKey identifier
    hi link yankringHelp string

    " Clear all existing maps for this buffer
    " We should do this for all maps, but I am not sure how to do
    " this for this buffer/window only without affecting all the
    " other buffers.
    mapclear <buffer>
    " Create a mapping to act upon the yankring
    nnoremap <buffer> <silent> <2-LeftMouse> :call <SID>YRWindowActionN('p' ,'n')<CR>
    nnoremap <buffer> <silent> <CR>          :call <SID>YRWindowActionN('p' ,'n')<CR>
    xnoremap <buffer> <silent> <CR>          :call <SID>YRWindowAction ('p' ,'v')<CR>
    nnoremap <buffer> <silent> p             :call <SID>YRWindowActionN('p' ,'n')<CR>
    xnoremap <buffer> <silent> p             :call <SID>YRWindowAction ('p' ,'v')<CR>
    nnoremap <buffer> <silent> P             :call <SID>YRWindowActionN('P' ,'n')<CR>
    xnoremap <buffer> <silent> P             :call <SID>YRWindowAction ('P' ,'v')<CR>
    nnoremap <buffer> <silent> gp            :call <SID>YRWindowActionN('gp','n')<CR>
    xnoremap <buffer> <silent> gp            :call <SID>YRWindowAction ('gp','v')<CR>
    nnoremap <buffer> <silent> gP            :call <SID>YRWindowActionN('gP','n')<CR>
    xnoremap <buffer> <silent> gP            :call <SID>YRWindowAction ('gP','v')<CR>
    nnoremap <buffer> <silent> d             :call <SID>YRWindowActionN('d' ,'n')<CR>
    xnoremap <buffer> <silent> d             :call <SID>YRWindowAction ('d' ,'v')<CR>
    xnoremap <buffer> <silent> r             :call <SID>YRWindowAction ('r' ,'v')<CR>
    nnoremap <buffer> <silent> s             :call <SID>YRWindowAction ('s' ,'n')<CR>
    nnoremap <buffer> <silent> a             :call <SID>YRWindowAction ('a' ,'n')<CR>
    nnoremap <buffer> <silent> c             :call <SID>YRWindowAction ('c' ,'n')<CR>
    nnoremap <buffer> <silent> ?             :call <SID>YRWindowAction ('?' ,'n')<CR>
    nnoremap <buffer> <silent> u             :call <SID>YRWindowAction ('u' ,'n')<CR>
    nnoremap <buffer> <silent> q             :call <SID>YRWindowAction ('q' ,'n')<CR>
    nnoremap <buffer> <silent> R             :call <SID>YRWindowAction ('R' ,'n')<CR>
    nnoremap <buffer> <silent> <space>     \|:silent exec 'vertical resize '.
                \ (
                \ g:yankring_window_use_horiz!=1 && winwidth('.') > g:yankring_window_width
                \ ?(g:yankring_window_width)
                \ :(winwidth('.') + g:yankring_window_increment)
                \ )<CR>

    " Erase it's contents to the blackhole
    silent! exec '%delete _'

    " Display the status line / help 
    call s:YRWindowStatus(0)
    exec 'normal! G'

    " Display the contents of the yankring
    silent! put =a:results

    if getline('$') == ''
        " Erase last blank line
        silent! exec '$delete _'
    endif

    " Move the cursor to the first line with an element
    exec 0
    call search('^\d','W') 

    setlocal nomodifiable
    "
    " Restore the previous cpoptions settings
    let &cpoptions = old_cpoptions

endfunction

function! s:YRWindowActionN(op, cmd_mode) 
    let v_count    = v:count
    " If no count was specified it will have a value of 0
    " so set it to at least 1
    let v_count = ((v_count > 0)?(v_count):1)

    if v_count > 1
        if !exists("b:yankring_show_range_error")
            let b:yankring_show_range_error = v_count
        else
            let b:yankring_show_range_error = b:yankring_show_range_error - 1
        endif

        if b:yankring_show_range_error == 1
            call s:YRWarningMsg("YR:Use visual mode if you need to specify a count")
            unlet b:yankring_show_range_error
        endif
        return
    endif
    
    call s:YRWindowAction(a:op, a:cmd_mode)
    let v_count = v_count - 1

    if g:yankring_window_auto_close == 1 && v_count == 0 && a:op != 'd'
        " If autoclose is set close the window unless 
        " you are removing items from the YankRing
        exec 'bdelete '.s:yr_buffer_id
        return "" 
    endif

    return "" 
endfunction

function! s:YRWindowAction(op, cmd_mode) range
    let default_buffer = ((&clipboard=~'unnamed')?'+':'"')
    let opcode     = a:op
    let lines      = []
    let v_count    = v:count
    let cmd_mode   = a:cmd_mode
    let firstline  = a:firstline
    let lastline   = a:lastline

    if a:lastline < a:firstline
        let firstline = a:lastline
        let lastline  = a:firstline
    endif

    if cmd_mode == 'n'
        let v_count = 1
        " If a count was provided (5p), we want to repeat the paste
        " 5 times, but this also alters the a:firstline and a:lastline
        " ranges, which while in normal mode we do not want
        let lastline = firstline
    endif
    " If no count was specified it will have a value of 0
    " so set it to at least 1
    let v_count = ((v_count > 0)?(v_count):1)

    if '[dr]' =~ opcode 
        " Reverse the order of the lines to act on
        let begin = lastline
        while begin >= firstline 
            call add(lines, getline(begin))
            let begin = begin - 1
        endwhile
    else
        " Process the selected items in order
        let begin = firstline
        while begin <= lastline 
            call add(lines, getline(begin))
            let begin = begin + 1
        endwhile
    endif

    if opcode ==# 'q'
        " Close the yankring window
        if s:yr_winsize_chgd == 1
            " Adjust the Vim window width back to the width
            " it was before we showed the yankring window
            let &columns= &columns - (g:yankring_window_width)
        endif

        " Hide the YankRing window
        hide

        if bufwinnr(s:yr_buffer_last) != -1
            " If the buffer is visible, switch to it
            exec s:yr_buffer_last_winnr . "wincmd w"
        endif

        return
    elseif opcode ==# 's'
        " Switch back to the original buffer
        exec s:yr_buffer_last_winnr . "wincmd w"
    
        call s:YRSearch()
        return
    elseif opcode ==# 'u'
        " Switch back to the original buffer
        exec s:yr_buffer_last_winnr . "wincmd w"
    
        call s:YRShow(0)
        return
    elseif opcode ==# 'R'
        " Switch back to the original buffer
        exec s:yr_buffer_last_winnr . "wincmd w"
    
        call s:YRShow(0, 'R')
        return
    elseif opcode ==# 'a'
        let l:curr_line = line(".")
        " Toggle the auto close setting
        let g:yankring_window_auto_close = 
                    \ (g:yankring_window_auto_close == 1?0:1)
        " Display the status line / help 
        call s:YRWindowStatus(0)
        call cursor(l:curr_line,0)
        return
    elseif opcode ==# 'c'
        let l:curr_line = line(".")
        " Toggle the clipboard monitor setting
        let g:yankring_clipboard_monitor = 
                    \ (g:yankring_clipboard_monitor == 1?0:1)
        " Display the status line / help 
        call s:YRWindowStatus(0)
        call cursor(l:curr_line,0)
        return
    elseif opcode ==# '?'
        " Display the status line / help 
        call s:YRWindowStatus(1)
        return
    endif

    " Switch back to the original buffer
    exec s:yr_buffer_last_winnr . "wincmd w"
    
    " Intentional case insensitive comparision
    if opcode =~? 'p'
        let cmd   = 'YRGetElem '
        let parms = ", '".opcode."' "
    elseif opcode ==? 'r'
        let opcode = 'p'
        let cmd    = 'YRGetElem '
        let parms  = ", 'p' "
    elseif opcode ==# 'd'
        let cmd   = 'YRPop '
        let parms = ""
    endif

    " Only execute this code if we are operating on elements
    " within the yankring
    if '[auq?]' !~# opcode 
        while v_count > 0
            " let iter  = 0
            " let index = 0
            for line in lines
                let elem = matchstr(line, '^\d\+')
                if elem > 0
                    if elem > 0 && elem <= s:yr_count
                        " if iter > 0 && opcode =~# 'p'
                        if opcode =~# 'p'
                            " Move to the end of the last pasted item
                            " only if pasting after (not above)
                            " ']
                        endif
                        exec cmd . elem . parms
                        " let iter += 1
                    endif
                endif
            endfor
            let v_count = v_count - 1
        endwhile

        if opcode ==# 'd'
            call s:YRShow(0)
            return ""
        endif

        if g:yankring_window_auto_close == 1 && cmd_mode == 'v'
            exec 'bdelete '.s:yr_buffer_id
            return "" 
        endif

    endif

    return "" 

endfunction
      
function! s:YRWarningMsg(msg)
    echohl WarningMsg
    echomsg a:msg 
    echohl None
endfunction
      
function! s:YRErrorMsg(msg)
    echohl ErrorMsg
    echomsg a:msg 
    echohl None
endfunction
      
function! s:YRWinLeave()
    " Track which window we are last in.  We will use this information
    " to determine where we need to paste any contents, or which 
    " buffer to return to.
    
    if s:yr_buffer_id < 0
        " The yankring window has never been activated
        return
    endif

    if winbufnr(winnr()) == s:yr_buffer_id
        " Ignore leaving the yankring window
        return
    endif

    if bufwinnr(s:yr_buffer_id) != -1
        " YankRing window is visible, so save off the previous buffer ids
        let s:yr_buffer_last_winnr = winnr()
        let s:yr_buffer_last       = winbufnr(s:yr_buffer_last_winnr)
    " else
    "     let s:yr_buffer_last_winnr = -1
    "     let s:yr_buffer_last       = -1
    endif
endfunction
      
function! s:YRFocusGained()
    if g:yankring_clipboard_monitor == 1
        " If the clipboard has changed record it inside the yankring
        " echomsg "YRFocusGained[".len(@+)."][".@+.']['.s:yr_prev_clipboard.']'
        if len(@+) > 0 && @+ != s:yr_prev_clipboard
            let elem    = s:YRMRUElemFormat( 
                        \   getreg('+')
                        \ , getregtype('+') 
                        \ )
            let found   = s:YRMRUHas('s:yr_history_list', elem)

            " Only add the item to the "top" of the ring if it is
            " not in the ring already.
            if found == -1
                call YRRecord("+")
                " silent! call YRRecord("+")
            endif
        endif

        " If the yankring window is open, refresh it
        call s:YRWindowUpdate()
    endif
endfunction

function! s:YRInsertLeave()
    " The YankRing uses omaps to execute the prescribed motion
    " and then appends to the motion a call to a YankRing 
    " function to record the contents of the changed register.
    "
    " We cannot append a function call to the end of a motion
    " that results in Insert mode.  For example, any command
    " like 'cw' enters insert mode.  Appending a function call
    " after the w, simply writes out the call as if the user 
    " typed it.
    "
    " Using the InsertLeave event, allows us to capture the 
    " contents of any changed register after it completes.
    
    call YRRecord(s:YRRegister())

    " When performing a change (not a yank or delete)
    " it is not possible to call <SID>yrrecord at the end
    " of the command (or it's contents will be inserted 
    " into the buffer instead of executed).
    " So, when using ".", we have to remove the _zap_
    " keys and then re-add them back again after we 
    " record the updates.
    if s:yr_remove_omap_dot == 1
        call s:YRMapsCreate('add_only_zap_keys')
    endif

endfunction
      
" Deleting autocommands first is a good idea especially if we want to reload
" the script without restarting vim.
" Call YRFocusGained to check if the clipboard has been updated
augroup YankRing
    autocmd!
    autocmd VimEnter    * :if has('clipboard') | call <SID>YRFocusGained() | endif
    autocmd WinLeave    * :call <SID>YRWinLeave()
    autocmd FocusGained * :if has('clipboard') | call <SID>YRFocusGained() | endif
    autocmd InsertLeave * :call <SID>YRInsertLeave()
    autocmd User        YRSetNumberedReg :call <SID>YRSetNumberedReg()
    " autocmd User        YRSetNumberedReg :let i = 0 | while i <= 10 | if i > s:yr_count | break | endif | call setreg( (i), s:YRGetValElemNbr((i),'v'), s:YRGetValElemNbr((i),'t') ) | let i += 1 | endwhile
augroup END


" copy register
inoremap <script> <SID>YRGetChar <c-r>=YRGetChar()<CR>
" inoremap <script> <SID>YRGetSearch <c-r>=YRGetSearch()<CR>
nnoremap <silent> <SID>yrrecord :call YRRecord3()<cr>
inoremap <silent> <SID>yrrecord <C-R>=YRRecord3()<cr>


" Public commands
command!                           YRClear        call s:YRClear()
command!                  -nargs=0 YRMapsCreate   call s:YRMapsCreate()
command!                  -nargs=0 YRMapsDelete   call s:YRMapsDelete()
command! -range -bang     -nargs=? YRDeleteRange  <line1>,<line2>call s:YRYankRange(<bang>1, <args>)
command!                  -nargs=* YRGetElem      call s:YRGetElem(<args>)
command!        -bang     -nargs=? YRGetMultiple  call s:YRGetMultiple(<bang>0, <args>)
command! -count -register -nargs=* YRPaste        call s:YRPaste(0,1,<args>)
command!                  -nargs=? YRPop          <line1>,<line2>call s:YRPop(<args>)
command!        -register -nargs=? YRPush         call s:YRPush(<args>)
command! -count -register -nargs=* YRReplace      call s:YRPaste(1,<args>)
command!                  -nargs=? YRSearch       call s:YRSearch(<q-args>)
command!                  -nargs=? YRShow         call s:YRShow(<args>)
command!                  -nargs=? YRToggle       call s:YRToggle(<args>)
command! -count -register -nargs=* YRYankCount    call s:YRYankCount(<args>)
command! -range -bang     -nargs=? YRYankRange    <line1>,<line2>call s:YRYankRange(<bang>0, <args>)
" command! -range -bang     -nargs=0 YRMapsMacro    <line1>,<line2>call s:YRMapsMacro(<bang>0, <args>)

" Menus 
if has("gui_running") && has("menu") && g:yankring_default_menu_mode != 0
    if g:yankring_default_menu_mode == 1
        let menuRoot = 'YankRing'
        let menuPriority = ''
    elseif g:yankring_default_menu_mode == 2
        let menuRoot = '&YankRing'
        let menuPriority = ''
    elseif g:yankring_default_menu_mode == 3 
        let menuRoot = exists("g:yankring_menu_root") ? g:yankring_menu_root : '&Plugin.&YankRing'
        let menuPriority = exists("g:yankring_menu_priority") ? yankring_menu_priority : ''
    else
        let menuRoot = '&Plugin.&YankRing'
        let menuPriority = ''
    endif

    let leader = '\'
    if exists('g:mapleader')
        let leader = g:mapleader
    endif
    let leader = escape(leader, '\')

    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.YankRing\ Window  :YRShow<CR>'
    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.YankRing\ Search  :YRSearch<CR>'
    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.Replace\ with\ Previous<TAB>'.leader.'<C-P> :YRReplace ''-1'', ''P''<CR>'
    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.Replace\ with\ Next<TAB>'.leader.'<C-N> :YRReplace ''1'', ''P''<CR>'
    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.Clear  :YRClear<CR>'
    exec 'noremenu  <script> '.menuPriority.' '.menuRoot.'.Toggle :YRToggle<CR>'
endif

if g:yankring_enabled == 1
    " Create YankRing Maps
    call s:YRMapsCreate()
endif

call s:YRInit()
call s:YRHistoryRead()

" vim:fdm=marker:nowrap:ts=4:expandtab:
