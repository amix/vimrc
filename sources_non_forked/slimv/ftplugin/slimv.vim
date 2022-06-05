" slimv.vim:    The Superior Lisp Interaction Mode for VIM
" Version:      0.9.14
" Last Change:  24 Aug 2021
" Maintainer:   Tamas Kovacs <kovisoft at gmail dot com>
" License:      This file is placed in the public domain.
"               No warranty, express or implied.
"               *** ***   Use At-Your-Own-Risk!   *** ***
"
" =====================================================================
"
"  Load Once:
if &cp || exists( 'g:slimv_loaded' )
    finish
endif

let g:slimv_loaded = 1

let g:slimv_windows = 0
let g:slimv_cygwin  = 0
let g:slimv_osx     = 0

if has( 'win32' ) || has( 'win95' ) || has( 'win64' ) || has( 'win16' )
    let g:slimv_windows = 1
elseif has( 'win32unix' )
    let g:slimv_cygwin = 1
elseif has( 'macunix' )
    let g:slimv_osx = 1
endif

if ( !exists( 'g:slimv_python_version' ) && has( 'python3' ) ) ||
\  (  exists( 'g:slimv_python_version' ) && g:slimv_python_version == 3 )
    let s:py_cmd = 'python3 ' "note space
    let s:pyfile_cmd = 'py3file '
else
    let s:py_cmd = 'python '  "note space
    let s:pyfile_cmd = 'pyfile '
endif


" =====================================================================
"  Functions used by global variable definitions
" =====================================================================

" Convert Cygwin path to Windows path, if needed
function! s:Cygpath( path )
    let path = a:path
    if g:slimv_cygwin
        let path = system( 'cygpath -w ' . path )
        let path = substitute( path, "\n", "", "g" )
        let path = substitute( path, "\\", "/", "g" )
    endif
    return path
endfunction

" Find swank.py in the Vim ftplugin directory (if not given in vimrc)
if !exists( 'g:swank_path' )
    let plugins = split( globpath( &runtimepath, 'ftplugin/**/swank.py'), '\n' )
    if len( plugins ) > 0
        let g:swank_path = s:Cygpath( plugins[0] )
    else
        let g:swank_path = 'swank.py'
    endif
endif

" Get the filetype (Lisp dialect) used by Slimv
function! SlimvGetFiletype()
    if &ft != ''
        " Return Vim filetype if defined
        return &ft
    endif

    if match( tolower( g:slimv_lisp ), 'clojure' ) >= 0 || match( tolower( g:slimv_lisp ), 'clj' ) >= 0
        " Must be Clojure
        return 'clojure'
    endif

    " We have no clue, guess its lisp
    return 'lisp'
endfunction

" Try to autodetect SWANK and build the command to start the SWANK server
function! SlimvSwankCommand()
    if exists( 'g:slimv_swank_clojure' ) && SlimvGetFiletype() =~ '.*clojure.*'
        return g:slimv_swank_clojure
    endif
    if exists( 'g:slimv_swank_scheme' ) && SlimvGetFiletype() == 'scheme'
        return g:slimv_swank_scheme
    endif
    if exists( 'g:slimv_swank_cmd' )
        return g:slimv_swank_cmd
    endif

    if g:slimv_lisp == ''
        let g:slimv_lisp = input( 'Enter Lisp path (or fill g:slimv_lisp in your vimrc): ', '', 'file' )
    endif

    let cmd = SlimvSwankLoader()
    if cmd != ''
        if g:slimv_windows || g:slimv_cygwin
            return '!start /MIN ' . cmd
        elseif $STY != ''
            " GNU screen under Linux or macOS
            return "! screen -X eval 'title swank' 'screen " . cmd . "' 'select swank'"
        elseif $TMUX != ''
            " tmux under Linux or macOS
            return "! tmux new-window -d -n swank '" . cmd . "'"
        elseif g:slimv_osx
            let result = system('osascript -e "exists application \"iterm\""')
                if result[:-2] == 'true'
                    let path2as = globpath( &runtimepath, 'ftplugin/**/iterm.applescript')
                    return '!' . path2as . ' ' . cmd
                else
                    " doubles quotes within 'cmd' need to become '\\\"'
                    return '!osascript -e "tell application \"Terminal\" to do script \"' . escape(escape(cmd, '"'), '\"') . '\""'
                endif 
        elseif $DISPLAY == ''
            " No X, no terminal multiplexer. Cannot run swank server.
            call SlimvErrorWait( 'No X server. Run Vim from screen/tmux or start SWANK server manually.' )
            return ''
        else
            " Must be Linux
            return '! SWANK_PORT=' . g:swank_port . ' xterm -iconic -e ' . cmd . ' &'
        endif
    endif
    return ''
endfunction

" =====================================================================
"  Global variable definitions
" =====================================================================

" Host name or IP address of the SWANK server
if !exists( 'g:swank_host' )
    let g:swank_host = 'localhost'
endif

" TCP port number to use for the SWANK server
if !exists( 'g:swank_port' )
    let g:swank_port = 4005
endif

" Find Lisp (if not given in vimrc)
if !exists( 'g:slimv_lisp' )
    let lisp = ['', '']
    if exists( 'g:slimv_preferred' )
        let lisp = SlimvAutodetect( tolower(g:slimv_preferred) )
    endif
    if lisp[0] == ''
        let lisp = SlimvAutodetect( '' )
    endif
    let g:slimv_lisp = lisp[0]
    if !exists( 'g:slimv_impl' )
        let g:slimv_impl = lisp[1]
    endif
endif

" Try to find out the Lisp implementation
" if not autodetected and not given in vimrc
if !exists( 'g:slimv_impl' )
    let g:slimv_impl = SlimvImplementation()
endif

" REPL buffer name
if !exists( 'g:slimv_repl_name' )
    let g:slimv_repl_name = 'REPL'
endif

" SLDB buffer name
if !exists( 'g:slimv_sldb_name' )
    let g:slimv_sldb_name = 'SLDB'
endif

" INSPECT buffer name
if !exists( 'g:slimv_inspect_name' )
    let g:slimv_inspect_name = 'INSPECT'
endif

" THREADS buffer name
if !exists( 'g:slimv_threads_name' )
    let g:slimv_threads_name = 'THREADS'
endif

" Shall we open REPL buffer in split window?
if !exists( 'g:slimv_repl_split' )
    let g:slimv_repl_split = 1
endif

" Size of the split window
if !exists( 'g:slimv_repl_split_size' )
    let g:slimv_repl_split_size = ''
endif

" Wrap long lines in REPL buffer
if !exists( 'g:slimv_repl_wrap' )
    let g:slimv_repl_wrap = 1
endif

" Wrap long lines in SLDB buffer
if !exists( 'g:slimv_sldb_wrap' )
    let g:slimv_sldb_wrap = 0
endif

" Maximum number of lines echoed from the evaluated form
if !exists( 'g:slimv_echolines' )
    let g:slimv_echolines = 4
endif

" Syntax highlighting for the REPL buffer
if !exists( 'g:slimv_repl_syntax' )
    let g:slimv_repl_syntax = 1
endif

" Specifies the behaviour of insert mode <CR>, <Up>, <Down> in the REPL buffer:
" 1: <CR>   evaluates,      <Up>/<Down>     brings up command history
" 0: <C-CR> evaluates,      <C-Up>/<C-Down> brings up command history,
"    <CR>   opens new line, <Up>/<Down>     moves cursor up/down
if !exists( 'g:slimv_repl_simple_eval' )
    let g:slimv_repl_simple_eval = 1
endif

" Alternative value (in msec) for 'updatetime' while the REPL buffer is changing
if !exists( 'g:slimv_updatetime' )
    let g:slimv_updatetime = 500
endif

" Slimv keybinding set (0 = no keybindings)
if !exists( 'g:slimv_keybindings' )
    let g:slimv_keybindings = 1
endif

" Append Slimv menu to the global menu (0 = no menu)
if !exists( 'g:slimv_menu' )
    let g:slimv_menu = 1
endif

" Build the ctags command capable of generating lisp tags file
" The command can be run with execute 'silent !' . g:slimv_ctags
if !exists( 'g:slimv_ctags' )
    let ctags = split( globpath( '$vim,$vimruntime', 'ctags.exe' ), '\n' )
    if len( ctags ) > 0
        " Remove -a option to regenerate every time
        let g:slimv_ctags = '"' . ctags[0] . '" -a --language-force=lisp *.lisp *.clj'
    endif
endif

" Name of tags file used by slimv for find-definitions
" If this is the empty string then no tags file is used
if !exists( 'g:slimv_tags_file' )
    let g:slimv_tags_file = tempname()
endif

" Prepend tags file to the tags list
if g:slimv_tags_file != ''
    if &tags == ''
        let &tags=g:slimv_tags_file
    else
        let &tags=g:slimv_tags_file . ',' . &tags
    endif
endif

" Package/namespace handling
if !exists( 'g:slimv_package' )
    let g:slimv_package = 1
endif

" General timeout for various startup and connection events (seconds)
if !exists( 'g:slimv_timeout' )
    let g:slimv_timeout = 20
endif

" Use balloonexpr to display symbol description
if !exists( 'g:slimv_balloon' )
    let g:slimv_balloon = 1
endif

" Shall we use simple or fuzzy completion?
if !exists( 'g:slimv_simple_compl' )
    let g:slimv_simple_compl = 0
endif

" Custom <Leader> for the Slimv plugin
if !exists( 'g:slimv_leader' )
    if exists( 'mapleader' ) && mapleader != ' '
        let g:slimv_leader = mapleader
    else
        let g:slimv_leader = ','
    endif
endif

" Maximum number of lines searched backwards for indenting special forms
if !exists( 'g:slimv_indent_maxlines' )
    let g:slimv_indent_maxlines = 50
endif

" Special indentation for keyword lists
if !exists( 'g:slimv_indent_keylists' )
    let g:slimv_indent_keylists = 1
endif

" Maximum length of the REPL buffer
if !exists( 'g:slimv_repl_max_len' )
    let g:slimv_repl_max_len = 0
endif

" Shall we strip ANSI escape sequences from the REPL output?
if !exists( 'g:slimv_strip_ansi' )
    let g:slimv_strip_ansi = 0
endif

" =====================================================================
"  Template definitions
" =====================================================================

if !exists( 'g:slimv_template_apropos' )
    if SlimvGetFiletype() =~ '.*clojure.*'
        let g:slimv_template_apropos = '(find-doc "%1")'
    else
        let g:slimv_template_apropos = '(apropos "%1")'
    endif
endif


" =====================================================================
"  Other non-global script variables
" =====================================================================

let s:indent = ''                                         " Most recent indentation info
let s:last_update = 0                                     " The last update time for the REPL buffer
let s:save_updatetime = &updatetime                       " The original value for 'updatetime'
let s:save_showmode = &showmode                           " The original value for 'showmode'
let s:python_initialized = 0                              " Is the embedded Python initialized?
let s:swank_version = ''                                  " SWANK server version string
let s:swank_connected = 0                                 " Is the SWANK server connected?
let s:swank_package = ''                                  " Package to use at the next SWANK eval
let s:swank_package_form = ''                             " The entire form that was used to set current package
let s:swank_form = ''                                     " Form to send to SWANK
let s:refresh_disabled = 0                                " Set this variable temporarily to avoid recursive REPL rehresh calls
let s:sldb_level = -1                                     " Are we in the SWANK debugger? -1 == no, else SLDB level
let s:break_on_exception = 0                              " Enable debugger break on exceptions (for ritz-swank)
let s:compiled_file = ''                                  " Name of the compiled file
let s:win_id = 0                                          " Counter for generating unique window id
let s:repl_buf = -1                                       " Buffer number for the REPL buffer
let s:current_buf = -1                                    " Swank action was requested from this buffer
let s:current_win = 0                                     " Swank action was requested from this window
let s:read_string_mode = 0                                " Read string mode indicator
let s:arglist_line = 0                                    " Arglist was requested in this line ...
let s:arglist_col = 0                                     " ... and column
let s:inspect_path = []                                   " Inspection path of the current object
let s:skip_sc = 'synIDattr(synID(line("."), col("."), 0), "name") =~ "[Ss]tring\\|[Cc]omment"'
                                                          " Skip matches inside string or comment 
let s:skip_q = 'getline(".")[col(".")-2] == "\\"'         " Skip escaped double quote characters in matches
let s:frame_def = '^\s\{0,2}\d\{1,}:'                     " Regular expression to match SLDB restart or frame identifier
let s:spec_indent = 'flet\|labels\|macrolet\|symbol-macrolet'
                                                          " List of symbols need special indenting
let s:spec_param = 'defmacro'                             " List of symbols with special parameter list
let s:binding_form = 'let\|let\*'                         " List of symbols with binding list

" =====================================================================
"  General utility functions
" =====================================================================

" Check that current SWANK version is same or newer than the given parameter
function! s:SinceVersion( ver )
    " Before ver 2.18 SWANK version string was a date of form YYYY-MM-DD
    if len( a:ver ) >= 8
        " Checking for old style version string YYYY-MM-DD
        if len( s:swank_version ) < 8
            " Current version is new style -> must be newer than the one we are checking for
            return 1
        endif
    else
        " Checking for new style version string X.XX
        if len( s:swank_version ) >= 8
            " Current version is old style -> must be older than the one we are checking for
            return 0
        endif
    endif
    if s:swank_version >= a:ver
        return 1
    else
        return 0
    endif
endfunction 

" Display an error message
function! SlimvError( msg )
    echohl ErrorMsg
    echo a:msg
    echohl None
endfunction 

" Display an error message and a question, return user response
function! SlimvErrorAsk( msg, question )
    echohl ErrorMsg
    let answer = input( a:msg . a:question )
    echo ""
    echohl None
    return answer
endfunction 

" Display an error message and wait for ENTER
function! SlimvErrorWait( msg )
    call SlimvErrorAsk( a:msg, " Press ENTER to continue." )
endfunction 

" Shorten long messages to fit status line
function! SlimvShortEcho( msg )
    let saved=&shortmess
    set shortmess+=T
    exe "normal! :echomsg a:msg\n" 
    let &shortmess=saved
endfunction

" Go to the end of buffer, make sure the cursor is positioned
" after the last character of the buffer when in insert mode
function s:EndOfBuffer()
    normal! G$
    if &virtualedit != 'all'
        call cursor( line('$'), 99999 )
    endif
endfunction

" Position the cursor at the end of the REPL buffer
" Optionally mark this position in Vim mark 's'
function! SlimvEndOfReplBuffer( force )
    if line( '.' ) >= b:repl_prompt_line - 1 || a:force
        " Go to the end of file only if the user did not move up from here
        call s:EndOfBuffer()
    endif
endfunction

" Remember the end of the REPL buffer: user may enter commands here
" Also remember the prompt, because the user may overwrite it
function! SlimvMarkBufferEnd( force )
    if exists( 'b:slimv_repl_buffer' )
        setlocal nomodified
        call SlimvEndOfReplBuffer( a:force )
        let b:repl_prompt_line = line( '$' )
        let b:repl_prompt_col = len( getline('$') ) + 1
        let b:repl_prompt = getline( b:repl_prompt_line )
    endif
endfunction

" Get REPL prompt line. Fix stored prompt position when corrupted
" (e.g. some lines were deleted from the REPL buffer)
function! s:GetPromptLine()
    if b:repl_prompt_line > line( '$' )
        " Stored prompt line is corrupt
        let b:repl_prompt_line = line( '$' )
        let b:repl_prompt_col = len( getline('$') ) + 1
        let b:repl_prompt = getline( b:repl_prompt_line )
    endif
    return b:repl_prompt_line
endfunction

" Generate unique window id for the current window
function s:MakeWindowId()
    if g:slimv_repl_split && !exists('w:id')
        let s:win_id = s:win_id + 1
        let w:id = s:win_id
    endif
endfunction

" Find and switch to window with the specified window id
function s:SwitchToWindow( id )
    for winnr in range( 1, winnr('$') )
        if getwinvar( winnr, 'id' ) is a:id
            execute winnr . "wincmd w"
        endif
    endfor
endfunction

" Save caller buffer identification
function! SlimvBeginUpdate()
    call s:MakeWindowId()
    let s:current_buf = bufnr( "%" )
    let s:current_win = getwinvar( winnr(), 'id' )
endfunction

" Switch to the buffer/window that was active before a swank action
function! SlimvRestoreFocus( hide_current_buf )
    if exists("b:previous_buf")
        let new_buf = b:previous_buf
        let new_win = b:previous_win
    else
        let new_buf = s:current_buf
        let new_win = s:current_win
    endif
    let buf = bufnr( "%" )
    let win = getwinvar( winnr(), 'id' )
    if a:hide_current_buf
        set nobuflisted
        b #
    endif
    if winnr('$') > 1 && new_win != '' && new_win != win
        " Switch to the caller window
        call s:SwitchToWindow( new_win )
    endif
    if new_buf >= 0 && buf != new_buf
        " Switch to the caller buffer
        execute "buf " . new_buf
    endif
endfunction

" Handle response coming from the SWANK listener
function! SlimvSwankResponse()
    let s:swank_ok_result = ''
    let s:refresh_disabled = 1
    silent execute s:py_cmd . 'swank_output(1)'
    let s:refresh_disabled = 0
    let s:swank_action = ''
    let s:swank_result = ''
    silent execute s:py_cmd . 'swank_response("")'

    if s:swank_action == ':describe-symbol' && s:swank_result != ''
        echo substitute(s:swank_result,'^\n*','','')
    elseif s:swank_ok_result != ''
        " Display the :ok result also in status bar in case the REPL buffer is not shown
        let s:swank_ok_result = substitute(s:swank_ok_result,"\<LF>",'','g')
        if s:swank_ok_result == ''
            call SlimvShortEcho( '=> OK' )
        else
            call SlimvShortEcho( '=> ' . s:swank_ok_result )
        endif
    endif
    if s:swank_actions_pending
        let s:last_update = -1
    elseif s:last_update < 0
        " Remember the time when all actions are processed
        let s:last_update = localtime()
    endif
    if s:swank_actions_pending == 0 && s:last_update >= 0 && s:last_update < localtime() - 2
        " All SWANK output handled long ago, restore original update frequency
        if &updatetime == g:slimv_updatetime
            let &updatetime = s:save_updatetime
        endif
    else
        " SWANK output still pending, keep higher update frequency
        if &updatetime != g:slimv_updatetime
            let s:save_updatetime = &updatetime
        endif
        let &updatetime = g:slimv_updatetime
    endif
endfunction

" Execute the given command and write its output at the end of the REPL buffer
function! SlimvCommand( cmd )
    silent execute a:cmd
    if &updatetime != g:slimv_updatetime
        let s:save_updatetime = &updatetime
    endif
    " Update more frequently until all swank responses processed
    let &updatetime = g:slimv_updatetime
    let s:last_update = -1
endfunction

" Execute the given SWANK command, wait for and return the response
function! SlimvCommandGetResponse( name, cmd, timeout )
    let s:refresh_disabled = 1
    call SlimvCommand( a:cmd )
    let s:swank_action = ''
    let s:swank_result = ''
    let starttime = localtime()
    let cmd_timeout = a:timeout
    if cmd_timeout == 0
        let cmd_timeout = 3
    endif
    while s:swank_action == '' && localtime()-starttime < cmd_timeout
        execute s:py_cmd . "swank_output( 0 )"
        silent execute s:py_cmd . 'swank_response("' . a:name . '")'
    endwhile
    let s:refresh_disabled = 0
    return s:swank_result
endfunction

" Reload the contents of the REPL buffer from the output file if changed
function! SlimvRefreshReplBuffer()
    if s:refresh_disabled
        " Refresh is unwanted at the moment, probably another refresh is going on
        return
    endif

    if s:repl_buf == -1
        " REPL buffer not loaded
        return
    endif

    if s:swank_connected
        call SlimvSwankResponse()
    endif

    if exists("s:input_prompt") && s:input_prompt != ''
        let answer = input( s:input_prompt )
        unlet s:input_prompt
        echo ""
        call SlimvCommand( s:py_cmd . 'swank_return("' . answer . '")' )
    endif
endfunction

" This function re-triggers the CursorHold event
" after refreshing the REPL buffer
function! SlimvTimer()
    if v:count > 0
        " Skip refreshing if the user started a command prefixed with a count
        return
    endif
    " We don't want autocommands trigger during the quick switch to/from the REPL buffer
    noautocmd call SlimvRefreshReplBuffer()
    if mode() == 'i' || mode() == 'I' || mode() == 'r' || mode() == 'R'
        if bufname('%') != g:slimv_sldb_name && bufname('%') != g:slimv_inspect_name && bufname('%') != g:slimv_threads_name
            " Put '<Insert>' twice into the typeahead buffer, which should not do anything
            " just switch to replace/insert mode then back to insert/replace mode
            " But don't do this for readonly buffers
            call feedkeys("\<insert>\<insert>")
        endif
    else
        " Put an incomplete 'f' command and an Esc into the typeahead buffer
        call feedkeys("f\e", 'n')
    endif
endfunction

" Switch refresh mode on:
" refresh REPL buffer on frequent Vim events
function! SlimvRefreshModeOn()
    augroup SlimvCursorHold
        au!
        execute "au CursorHold   * :call SlimvTimer()"
        execute "au CursorHoldI  * :call SlimvTimer()"
    augroup END
endfunction

" Switch refresh mode off
function! SlimvRefreshModeOff()
    augroup SlimvCursorHold
        au!
    augroup END
endfunction

" Called when entering REPL buffer
function! SlimvReplEnter()
    call SlimvAddReplMenu()
    augroup SlimvReplChanged
        au!
        execute "au FileChangedRO " . g:slimv_repl_name . " :call SlimvRefreshModeOff()"
    augroup END
    call SlimvRefreshModeOn()
endfunction

" Called when leaving REPL buffer
function! SlimvReplLeave()
    try
        " Check if REPL menu exists, then remove it
        aunmenu REPL
        execute ':unmap ' . g:slimv_leader . '\'
    catch
        " REPL menu not found, we cannot remove it
    endtry
    if g:slimv_repl_split
        call SlimvRefreshModeOn()
    else
        call SlimvRefreshModeOff()
    endif
endfunction

" Refresh cursor position in the REPL buffer after new lines appended
function! SlimvReplSetCursorPos( force )
    " We do not want these autocommands to fire, the buffer switch will be temporary
    let savemark = getpos("'`'")
    let save_ei = &eventignore
    set eventignore=BufEnter,BufLeave,BufWinEnter
    let win = winnr()
    windo call SlimvMarkBufferEnd( a:force )
    execute win . "wincmd w"
    let &eventignore = save_ei
    call setpos("'`", savemark)
endfunction

" View the given file in a top/bottom/left/right split window
function! s:SplitView( filename )
    " Check if we have at least two windows used by slimv (have a window id assigned)
    let winnr1 = 0
    let winnr2 = 0
    for winnr in range( 1, winnr('$') )
        if getwinvar( winnr, 'id' ) != ''
            let winnr2 = winnr1
            let winnr1 = winnr
        endif
    endfor
    " create a unique buffer name that does not collide with any file or directory name
    let bname = a:filename
    let i = 0
    while filereadable(bname) || isdirectory(bname)
        let i = i+1
        let bname = a:filename . i
    endwhile
    if winnr1 > 0 && winnr2 > 0
        " We have already at least two windows used by slimv
        let winid = getwinvar( winnr(), 'id' )
        if bufnr("%") == s:current_buf && winid == s:current_win
            " Keep the current window on screen, use the other window for the new buffer
            if winnr1 != winnr()
                execute winnr1 . "wincmd w"
            else
                execute winnr2 . "wincmd w"
            endif
        endif
        execute "silent view! " . bname
    else
        " Generate unique window id for the old window if not yet done
        call s:MakeWindowId()
        " No windows yet, need to split
        if g:slimv_repl_split == 1
            execute "silent topleft " . g:slimv_repl_split_size . "sview! " . bname
        elseif g:slimv_repl_split == 2
            execute "silent botright " . g:slimv_repl_split_size . "sview! " . bname
        elseif g:slimv_repl_split == 3
            execute "silent topleft vertical " . g:slimv_repl_split_size . "sview! " . bname
        elseif g:slimv_repl_split == 4
            execute "silent botright vertical " . g:slimv_repl_split_size . "sview! " . bname
        else
            execute "silent view! " . bname
        endif
        " Generate unique window id for the new window as well
        call s:MakeWindowId()
    endif
    stopinsert
endfunction

" Open a buffer with the given name if not yet open, and switch to it
function! SlimvOpenBuffer( name )
    let buf = bufnr( '^' . a:name . '$' )
    if buf == -1
        " Create a new buffer
        call s:SplitView( a:name )
    else
        if g:slimv_repl_split
            " Buffer is already created. Check if it is open in a window
            let win = bufwinnr( buf )
            if win == -1
                " Create windows
                call s:SplitView( a:name )
            else
                " Switch to the buffer's window
                if winnr() != win
                    execute win . "wincmd w"
                endif
            endif
        else
            execute "buffer " . buf
            stopinsert
        endif
    endif
    if s:current_buf != bufnr( "%" )
        " Keep track of the previous buffer and window
        let b:previous_buf = s:current_buf
        let b:previous_win = s:current_win
    endif
    setlocal buftype=nofile
    setlocal noswapfile
    setlocal modifiable
endfunction

" Go to the end of the screen line
function s:EndOfScreenLine()
    if len(getline('.')) < &columns
        " g$ moves the cursor to the rightmost column if virtualedit=all
        normal! $
    else
        normal! g$
    endif
endfunction

" Set special syntax rules for the REPL buffer
function! SlimvSetSyntaxRepl()
    if SlimvGetFiletype() == 'scheme'
        syn cluster replListCluster contains=@schemeListCluster,lispList
    else
        syn cluster replListCluster contains=@lispListCluster
    endif

if exists("g:lisp_rainbow") && g:lisp_rainbow != 0

    if &bg == "dark"
        hi def hlLevel0 ctermfg=red         guifg=red1
        hi def hlLevel1 ctermfg=yellow      guifg=orange1
        hi def hlLevel2 ctermfg=green       guifg=yellow1
        hi def hlLevel3 ctermfg=cyan        guifg=greenyellow
        hi def hlLevel4 ctermfg=magenta     guifg=green1
        hi def hlLevel5 ctermfg=red         guifg=springgreen1
        hi def hlLevel6 ctermfg=yellow      guifg=cyan1
        hi def hlLevel7 ctermfg=green       guifg=slateblue1
        hi def hlLevel8 ctermfg=cyan        guifg=magenta1
        hi def hlLevel9 ctermfg=magenta     guifg=purple1
    else
        hi def hlLevel0 ctermfg=red         guifg=red3
        hi def hlLevel1 ctermfg=darkyellow  guifg=orangered3
        hi def hlLevel2 ctermfg=darkgreen   guifg=orange2
        hi def hlLevel3 ctermfg=blue        guifg=yellow3
        hi def hlLevel4 ctermfg=darkmagenta guifg=olivedrab4
        hi def hlLevel5 ctermfg=red         guifg=green4
        hi def hlLevel6 ctermfg=darkyellow  guifg=paleturquoise3
        hi def hlLevel7 ctermfg=darkgreen   guifg=deepskyblue4
        hi def hlLevel8 ctermfg=blue        guifg=darkslateblue
        hi def hlLevel9 ctermfg=darkmagenta guifg=darkviolet
    endif

 if SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*'

    syn region lispParen9 matchgroup=hlLevel9 start="`\=(" matchgroup=hlLevel9 end=")"  matchgroup=replPrompt end="^\S\+>" contains=TOP,@Spell
    syn region lispParen0 matchgroup=hlLevel8 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen0,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen1 matchgroup=hlLevel7 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen2 matchgroup=hlLevel6 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen3 matchgroup=hlLevel5 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen4 matchgroup=hlLevel4 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen5 matchgroup=hlLevel3 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen6 matchgroup=hlLevel2 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen7 matchgroup=hlLevel1 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen7,lispParen8,NoInParens
    syn region lispParen8 matchgroup=hlLevel0 start="`\=(" end=")" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen8,NoInParens

    syn region lispParen9 matchgroup=hlLevel9 start="`\=\[" matchgroup=hlLevel9 end="\]"  matchgroup=replPrompt end="^\S\+>" contains=TOP,@Spell
    syn region lispParen0 matchgroup=hlLevel8 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen0,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen1 matchgroup=hlLevel7 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen2 matchgroup=hlLevel6 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen3 matchgroup=hlLevel5 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen4 matchgroup=hlLevel4 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen5 matchgroup=hlLevel3 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen6 matchgroup=hlLevel2 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen7 matchgroup=hlLevel1 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen7,lispParen8,NoInParens
    syn region lispParen8 matchgroup=hlLevel0 start="`\=\[" end="\]" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen8,NoInParens

    syn region lispParen9 matchgroup=hlLevel9 start="`\={" matchgroup=hlLevel9 end="}"  matchgroup=replPrompt end="^\S\+>" contains=TOP,@Spell
    syn region lispParen0 matchgroup=hlLevel8 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen0,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen1 matchgroup=hlLevel7 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen1,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen2 matchgroup=hlLevel6 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen2,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen3 matchgroup=hlLevel5 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen3,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen4 matchgroup=hlLevel4 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen4,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen5 matchgroup=hlLevel3 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen5,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen6 matchgroup=hlLevel2 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen6,lispParen7,lispParen8,NoInParens
    syn region lispParen7 matchgroup=hlLevel1 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen7,lispParen8,NoInParens
    syn region lispParen8 matchgroup=hlLevel0 start="`\={" end="}" matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=TOP,lispParen8,NoInParens

 else

    syn region lispParen0           matchgroup=hlLevel0 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"              contains=@replListCluster,lispParen1,replPrompt
    syn region lispParen1 contained matchgroup=hlLevel1 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen2
    syn region lispParen2 contained matchgroup=hlLevel2 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen3
    syn region lispParen3 contained matchgroup=hlLevel3 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen4
    syn region lispParen4 contained matchgroup=hlLevel4 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen5
    syn region lispParen5 contained matchgroup=hlLevel5 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen6
    syn region lispParen6 contained matchgroup=hlLevel6 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen7
    syn region lispParen7 contained matchgroup=hlLevel7 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen8
    syn region lispParen8 contained matchgroup=hlLevel8 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen9
    syn region lispParen9 contained matchgroup=hlLevel9 start="`\=("  skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>"me=s-1,re=s-1 contains=@replListCluster,lispParen0

 endif

else

  if SlimvGetFiletype() !~ '.*clojure.*'
    syn region lispList             matchgroup=Delimiter start="("    skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>" contains=@replListCluster
    syn region lispBQList           matchgroup=PreProc   start="`("   skip="|.\{-}|" end=")"  matchgroup=replPrompt end="^\S\+>" contains=@replListCluster
  endif

endif

    syn match   replPrompt /^[^(]\S\+>/
    syn match   replPrompt /^(\S\+)>/
    hi def link replPrompt Type
endfunction

" Open a new REPL buffer
function! SlimvOpenReplBuffer()
    call SlimvOpenBuffer( g:slimv_repl_name )
    setlocal noreadonly
    let s:repl_buf = bufnr( "%" )
    let b:slimv_repl_buffer = 1
    call SlimvInitRepl()
    if g:slimv_repl_syntax
        call SlimvSetSyntaxRepl()
    else
        set syntax=
    endif

    " Prompt and its line and column number in the REPL buffer
    if !exists( 'b:repl_prompt' )
        let b:repl_prompt = ''
        let b:repl_prompt_line = 1
        let b:repl_prompt_col = 1
    endif

    " Add keybindings valid only for the REPL buffer
    inoremap <buffer> <silent>        <C-CR> <End><C-O>:call SlimvSendCommand(1)<CR><End>
    inoremap <buffer> <silent>        <C-C>  <C-O>:call SlimvInterrupt()<CR>
    inoremap <buffer> <silent> <expr> <C-W>  SlimvHandleCW()

    if g:slimv_repl_simple_eval
        inoremap <buffer> <silent>        <CR>     <C-R>=pumvisible() ? "\<lt>C-Y>"  : "\<lt>End>\<lt>C-O>:call SlimvSendCommand(0)\<lt>CR>\<lt>End>"<CR>
        inoremap <buffer> <silent>        <Up>     <C-R>=pumvisible() ? "\<lt>Up>"   : SlimvHandleUp()<CR>
        inoremap <buffer> <silent>        <Down>   <C-R>=pumvisible() ? "\<lt>Down>" : SlimvHandleDown()<CR>
    else
        inoremap <buffer> <silent>        <CR>     <C-R>=pumvisible() ? "\<lt>C-Y>"  : SlimvHandleEnterRepl()<CR><C-R>=SlimvArglistOnEnter()<CR>
        inoremap <buffer> <silent>        <C-Up>   <C-R>=pumvisible() ? "\<lt>Up>"   : SlimvHandleUp()<CR>
        inoremap <buffer> <silent>        <C-Down> <C-R>=pumvisible() ? "\<lt>Down>" : SlimvHandleDown()<CR>
    endif

    if exists( 'g:paredit_loaded' )
        inoremap <buffer> <silent> <expr> <BS>   PareditBackspace(1)
    else
        inoremap <buffer> <silent> <expr> <BS>   SlimvHandleBS()
    endif

    if g:slimv_keybindings == 1
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'.      :call SlimvSendCommand(0)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'/      :call SlimvSendCommand(1)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'<Up>   :call SlimvPreviousCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'<Down> :call SlimvNextCommand()<CR>'
    elseif g:slimv_keybindings == 2
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rs     :call SlimvSendCommand(0)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'ro     :call SlimvSendCommand(1)<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rp     :call SlimvPreviousCommand()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'rn     :call SlimvNextCommand()<CR>'
    endif

    if g:slimv_repl_wrap
        inoremap <buffer> <silent>        <Home> <C-O>g<Home>
        inoremap <buffer> <silent>        <End>  <C-O>:call <SID>EndOfScreenLine()<CR>
        noremap  <buffer> <silent>        <Up>   gk
        noremap  <buffer> <silent>        <Down> gj
        noremap  <buffer> <silent>        <Home> g<Home>
        noremap  <buffer> <silent>        <End>  :call <SID>EndOfScreenLine()<CR>
        noremap  <buffer> <silent>        k      gk
        noremap  <buffer> <silent>        j      gj
        noremap  <buffer> <silent>        0      g0
        noremap  <buffer> <silent>        $      :call <SID>EndOfScreenLine()<CR>
        setlocal wrap
    endif

    hi SlimvNormal term=none cterm=none gui=none
    hi SlimvCursor term=reverse cterm=reverse gui=reverse

    augroup SlimvReplAutoCmd
        au!
        " Add autocommands specific to the REPL buffer
        execute "au FileChangedShell " . g:slimv_repl_name . " :call SlimvRefreshReplBuffer()"
        execute "au FocusGained "      . g:slimv_repl_name . " :call SlimvRefreshReplBuffer()"
        execute "au BufEnter "         . g:slimv_repl_name . " :call SlimvReplEnter()"
        execute "au BufLeave "         . g:slimv_repl_name . " :call SlimvReplLeave()"
        execute "au BufWinEnter "      . g:slimv_repl_name . " :call SlimvMarkBufferEnd(1)"
        execute "au TabEnter *"        . " :call SlimvReplSetCursorPos(1)"
    augroup END

    call SlimvRefreshReplBuffer()
endfunction

" Clear the contents of the REPL buffer, keeping the last prompt only
function! SlimvClearReplBuffer()
    let this_buf = bufnr( "%" )
    if s:repl_buf == -1
        call SlimvError( "There is no REPL buffer." )
        return
    endif
    if this_buf != s:repl_buf
        let oldpos = winsaveview()
        execute "buf " . s:repl_buf
    endif
    if b:repl_prompt_line > 1
        execute "normal! gg0d" . (b:repl_prompt_line-1) . "GG$"
        let b:repl_prompt_line = 1
    endif
    if this_buf != s:repl_buf
        execute "buf " . this_buf
        call winrestview( oldpos )
    endif
endfunction

" Open a new Inspect buffer
function SlimvOpenInspectBuffer()
    call SlimvOpenBuffer( g:slimv_inspect_name )
    let b:range_start = 0
    let b:range_end   = 0
    let b:help = SlimvHelpInspect()

    " Add keybindings valid only for the Inspect buffer
    noremap  <buffer> <silent>        <F1>   :call SlimvToggleHelp()<CR>
    noremap  <buffer> <silent>        <CR>   :call SlimvHandleEnterInspect()<CR>
    noremap  <buffer> <silent> <Backspace>   :call SlimvSendSilent(['[-1]'])<CR>
    execute 'noremap <buffer> <silent> ' . g:slimv_leader.'q      :call SlimvQuitInspect(1)<CR>'

    if version < 703
        " conceal mechanism is defined since Vim 7.3
        syn region inspectItem   matchgroup=Ignore start="{\[\d\+\]\s*" end="\[]}"
        syn region inspectAction matchgroup=Ignore start="{<\d\+>\s*"   end="<>}"
    else
        syn region inspectItem   matchgroup=Ignore start="{\[\d\+\]\s*" end="\[]}" concealends
        syn region inspectAction matchgroup=Ignore start="{<\d\+>\s*"   end="<>}" concealends
        setlocal conceallevel=3 concealcursor=nc
    endif

    hi def link inspectItem   Special
    hi def link inspectAction String

    syn match Special /^\[<<\].*$/
    syn match Special /^\[--....--\]$/
endfunction

" Open a new Threads buffer
function SlimvOpenThreadsBuffer()
    call SlimvOpenBuffer( g:slimv_threads_name )
    let b:help = SlimvHelpThreads()

    " Add keybindings valid only for the Threads buffer
    "noremap  <buffer> <silent>        <CR>   :call SlimvHandleEnterThreads()<CR>
    noremap  <buffer> <silent>        <F1>                        :call SlimvToggleHelp()<CR>
    noremap  <buffer> <silent> <Backspace>                        :call SlimvKillThread()<CR>
    execute 'noremap <buffer> <silent> ' . g:slimv_leader.'r      :call SlimvListThreads()<CR>'
    execute 'noremap <buffer> <silent> ' . g:slimv_leader.'d      :call SlimvDebugThread()<CR>'
    execute 'noremap <buffer> <silent> ' . g:slimv_leader.'k      :call SlimvKillThread()<CR>'
    execute 'noremap <buffer> <silent> ' . g:slimv_leader.'q      :call SlimvQuitThreads()<CR>'
endfunction

" Open a new SLDB buffer
function SlimvOpenSldbBuffer()
    call SlimvOpenBuffer( g:slimv_sldb_name )

    " Add keybindings valid only for the SLDB buffer
    noremap  <buffer> <silent>        <CR>   :call SlimvHandleEnterSldb()<CR>
    if g:slimv_keybindings == 1
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'a      :call SlimvDebugAbort()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'q      :call SlimvDebugQuit()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'n      :call SlimvDebugContinue()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'N      :call SlimvDebugRestartFrame()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'si      :call SlimvDebugStepInto()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'sn      :call SlimvDebugStepNext()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'so      :call SlimvDebugStepOut()<CR>'
    elseif g:slimv_keybindings == 2
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'da     :call SlimvDebugAbort()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'dq     :call SlimvDebugQuit()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'dn     :call SlimvDebugContinue()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'dr     :call SlimvDebugRestartFrame()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'si      :call SlimvDebugStepInto()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'sn      :call SlimvDebugStepNext()<CR>'
        execute 'noremap <buffer> <silent> ' . g:slimv_leader.'so      :call SlimvDebugStepOut()<CR>'
    endif

    " Set folding parameters
    setlocal foldmethod=marker
    setlocal foldmarker={{{,}}}
    setlocal foldtext=substitute(getline(v:foldstart),'{{{','','')
    call s:SetKeyword()
    if g:slimv_sldb_wrap
        setlocal wrap
    endif

    if version < 703
        " conceal mechanism is defined since Vim 7.3
        syn match Ignore /{{{/
        syn match Ignore /}}}/
    else
        setlocal conceallevel=3 concealcursor=nc
        syn match Comment /{{{/ conceal
        syn match Comment /}}}/ conceal
    endif
    syn match Type /^\s\{0,2}\d\{1,3}:/
    syn match Type /^\s\+in "\(.*\)" \(line\|byte\) \(\d\+\)$/
endfunction

" End updating an otherwise readonly buffer
function SlimvEndUpdate()
    setlocal nomodifiable
    setlocal nomodified
endfunction

" Quit Inspector
function SlimvQuitInspect( force )
    " Clear the contents of the Inspect buffer
    if exists( 'b:inspect_pos' )
        unlet b:inspect_pos
    endif
    setlocal modifiable
    silent! %d _
    call SlimvEndUpdate()
    if a:force
        call SlimvCommand( s:py_cmd . 'swank_quit_inspector()' )
    endif
    call SlimvRestoreFocus(1)
endfunction

" Quit Threads
function SlimvQuitThreads()
    " Clear the contents of the Threads buffer
    setlocal modifiable
    silent! %d _
    call SlimvEndUpdate()
    call SlimvRestoreFocus(1)
endfunction

" Quit Sldb
function SlimvQuitSldb()
    " Clear the contents of the Sldb buffer
    setlocal modifiable
    silent! %d _
    call SlimvEndUpdate()
    call SlimvRestoreFocus(1)
endfunction

" Create help text for Inspect buffer
function SlimvHelpInspect()
    let help = []
    call add( help, '<F1>        : toggle this help' )
    call add( help, '<Enter>     : open object or select action under cursor' )
    call add( help, '<Backspace> : go back to previous object' )
    call add( help, g:slimv_leader . 'q          : quit' )
    return help
endfunction

" Create help text for Threads buffer
function SlimvHelpThreads()
    let help = []
    call add( help, '<F1>        : toggle this help' )
    call add( help, '<Backspace> : kill thread' )
    call add( help, g:slimv_leader . 'k          : kill thread' )
    call add( help, g:slimv_leader . 'd          : debug thread' )
    call add( help, g:slimv_leader . 'r          : refresh' )
    call add( help, g:slimv_leader . 'q          : quit' )
    return help
endfunction

" Write help text to current buffer at given line
function SlimvHelp( line )
    setlocal modifiable
    if exists( 'b:help_shown' )
        let help = b:help
    else
        let help = ['Press <F1> for Help']
    endif
    let b:help_line = a:line
    call append( b:help_line, help )
endfunction

" Toggle help
function SlimvToggleHelp()
    if exists( 'b:help_shown' )
        let lines = len( b:help )
        unlet b:help_shown
    else
        let lines = 1
        let b:help_shown = 1
    endif
    setlocal modifiable
    execute ":" . (b:help_line+1) . "," . (b:help_line+lines) . "d"
    call SlimvHelp( b:help_line )
    call SlimvEndUpdate()
endfunction

" Open SLDB buffer and place cursor on the given frame
function SlimvGotoFrame( frame )
    call SlimvOpenSldbBuffer()
    let bcktrpos = search( '^Backtrace:', 'bcnw' )
    let line = getline( '.' )
    let item = matchstr( line, '^\s*' . a:frame .  ':' )
    if item != '' && line('.') > bcktrpos
        " Already standing on the frame
        return
    endif

    " Must locate the frame starting from the 'Backtrace:' string
    call search( '^Backtrace:', 'bcw' )
    call search( '^\s*' . a:frame .  ':', 'w' )
endfunction

" Set 'iskeyword' option depending on file type
function! s:SetKeyword()
    let old_value = &iskeyword
    if match(old_value, '\^$') >= 0
        " remove trailing ^ because it will be added as chr 94
        setlocal iskeyword-=^
    endif
    if SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*'
        setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&
    else
        setlocal iskeyword+=+,-,*,/,%,<,=,>,:,$,?,!,@-@,94,~,#,\|,&,.,{,},[,]
    endif
endfunction

" Select symbol under cursor and return it
function! SlimvSelectSymbol()
    call s:SetKeyword()
    let oldpos = winsaveview()
    if col('.') > 1 && getline('.')[col('.')-1] =~ '\s'
        normal! h
    endif
    let symbol = expand('<cword>')
    call winrestview( oldpos )
    return symbol
endfunction

" Select symbol with possible prefixes under cursor and return it
function! SlimvSelectSymbolExt()
    let save_iskeyword = &iskeyword
    call s:SetKeyword()
    setlocal iskeyword+='
    let symbol = expand('<cword>')
    let &iskeyword = save_iskeyword
    return symbol
endfunction

" Find the matching pair
function! SlimvFindMatchingPair()
    let c = col( '.' ) - 1
    let firstchar = getline( '.' )[c]
    while c < len( getline( '.' ) ) && getline( '.' )[c] !~ '(\|)\|\[\|\]\|{\|}'
        normal! l
        let c = c + 1
    endwhile
    if getline( '.' )[c] =~ '(\|\[\|{'
        call searchpair( '(', '', ')', 'W', s:skip_sc )
    else
        call searchpair( '(', '', ')', 'bW', s:skip_sc )
    endif
endfunction

" Select bottom level form the cursor is inside and copy it to register 's'
function! SlimvSelectForm( extended )
    if SlimvGetFiletype() == 'r'
        silent! normal va(
        silent! normal "sY
        return 1
    endif
    " Search the opening '(' if we are standing on a special form prefix character
    let c = col( '.' ) - 1
    let firstchar = getline( '.' )[c]
    while c < len( getline( '.' ) ) && match( "'`#", getline( '.' )[c] ) >= 0
        normal! l
        let c = c + 1
    endwhile
    " select the whole form
"    if firstchar != '('
    if getline( '.' )[c] != '('
        call searchpair( '(', '', ')', 'bW', s:skip_sc )
    endif
    silent! normal v
    call searchpair( '(', '', ')', 'W', s:skip_sc )
    if &selection == 'exclusive' 
        silent! normal l
    endif
    let p1 = getpos('.')
    normal! o
    let p2 = getpos('.')
    if firstchar != '(' && p1[1] == p2[1] && (p1[2] == p2[2] || p1[2] == p2[2]+1)
        " Empty selection and no paren found, select current word instead
        normal! vvaw
    elseif a:extended || firstchar != '('
        " Handle '() or #'() etc. type special syntax forms (but stop at prompt)
        let c = col( '.' ) - 2
        while c >= 0 && match( ' \t()>', getline( '.' )[c] ) < 0
            normal! h
            let c = c - 1
        endwhile
    endif
    silent normal! "sy
    let sel = SlimvGetSelection()
    if sel == ''
        call SlimvError( "Form is empty." )
        return 0
    elseif sel == '(' || sel == '[' || sel == '{'
        call SlimvError( "Form is unbalanced." )
        return 0
    else
        return 1
    endif
endfunction

" Find starting '(' of a top level form
function! SlimvFindDefunStart()
    let l = line( '.' )
    let matchb = max( [l-200, 1] )
    if SlimvGetFiletype() == 'r'
        while searchpair( '(', '', ')', 'bW', s:skip_sc, matchb ) || searchpair( '{', '', '}', 'bW', s:skip_sc, matchb ) || searchpair( '\[', '', '\]', 'bW', s:skip_sc, matchb )
        endwhile
    else
        while searchpair( '(', '', ')', 'bW', s:skip_sc, matchb )
        endwhile
    endif
endfunction

" Select top level form the cursor is inside and copy it to register 's'
function! SlimvSelectDefun()
    call SlimvFindDefunStart()
    if SlimvGetFiletype() == 'r'
        " The cursor must be on the enclosing paren character
        silent! normal v%"sY
        return 1
    else
        return SlimvSelectForm( 1 )
    endif
endfunction

" Return the contents of register 's'
function! SlimvGetSelection()
    return getreg( 's' )
endfunction

" Find language specific package/namespace definition backwards
" Set it as the current package for the next swank action
function! SlimvFindPackage()
    if !g:slimv_package || SlimvGetFiletype() == 'scheme'
        return
    endif
    let oldpos = winsaveview()
    let save_ic = &ignorecase
    set ignorecase
    if SlimvGetFiletype() =~ '.*clojure.*'
        let string = '\(in-ns\|ns\)'
    else
        let string = '\(cl:\|common-lisp:\|\)in-package'
    endif
    let found = 0
    let searching = search( '(\s*' . string . '\s', 'bcW' )
    while searching
        " Search for the previos occurrence
        if synIDattr( synID( line('.'), col('.'), 0), 'name' ) !~ '[Ss]tring\|[Cc]omment'
            " It is not inside a comment or string
            let found = 1
            break
        endif
        let searching = search( '(\s*' . string . '\s', 'bW' )
    endwhile
    if found
        " Find the package name with all folds open
        normal! zn
        silent normal! w
        let l:package_command = expand('<cword>')
        silent normal! w
        let l:packagename_tokens = split(expand('<cWORD>'),')\|\s')
        normal! zN
        if l:packagename_tokens != []
            " Remove quote character from package name
            let s:swank_package = substitute( l:packagename_tokens[0], "'", '', '' )
            let s:swank_package_form = "(" . l:package_command . " " . l:packagename_tokens[0] . ")\n"
        else
            let s:swank_package = ''
            let s:swank_package_form = ''
        endif
    endif
    let &ignorecase = save_ic
    call winrestview( oldpos )
endfunction

" Execute the given SWANK command with current package defined
function! SlimvCommandUsePackage( cmd )
    call SlimvFindPackage()
    let s:refresh_disabled = 1
    call SlimvCommand( a:cmd )
    let s:swank_package = ''
    let s:swank_package_form = ''
    let s:refresh_disabled = 0
    call SlimvRefreshReplBuffer()
endfunction

" Initialize embedded Python and connect to SWANK server
function! SlimvConnectSwank()
    if !s:python_initialized
        if ( s:py_cmd == 'python3 ' && ! has('python3') ) ||
         \ ( s:py_cmd == 'python '  && ! has('python' ) )
            call SlimvErrorWait( 'Vim is compiled without the Python feature or Python is not installed. Unable to run SWANK client.' )
            return 0
        endif
        execute s:py_cmd . 'import vim'
        execute s:pyfile_cmd . g:swank_path
        let s:python_initialized = 1
    endif


    if !s:swank_connected
        let s:swank_version = ''
        let s:lisp_version = ''
        if g:swank_host == ''
            let g:swank_host = input( 'Swank server host name: ', 'localhost' )
        endif
        execute s:py_cmd . 'swank_connect("' . g:swank_host . '", ' . g:swank_port . ', "result" )'
        if result != '' && ( g:swank_host == 'localhost' || g:swank_host == '127.0.0.1' )
            " SWANK server is not running, start server if possible
            let swank = SlimvSwankCommand()
            if swank != ''
                redraw
                echon "\rStarting SWANK server..."
                silent execute swank
                let starttime = localtime()
                while result != '' && localtime()-starttime < g:slimv_timeout
                    sleep 500m
                    execute s:py_cmd . 'swank_connect("' . g:swank_host . '", ' . g:swank_port . ', "result" )'
                endwhile
                redraw!
            endif
        endif
        if result != ''
            " Display connection error message
            call SlimvErrorWait( result )
            return 0
        endif

        " Connected to SWANK server
        redraw
        echon "\rGetting SWANK connection info..."
        let starttime = localtime()
        while s:swank_version == '' && localtime()-starttime < g:slimv_timeout
            call SlimvSwankResponse()
        endwhile

        " Require some contribs
        let contribs = 'swank-presentations swank-fancy-inspector swank-c-p-c swank-arglists'
        if SlimvGetFiletype() == 'lisp'
            let contribs = 'swank-asdf swank-package-fu ' . contribs
        endif
        if g:slimv_simple_compl == 0
            let contribs = contribs . ' swank-fuzzy'
        endif
        execute s:py_cmd . "swank_require('(" . contribs . ")')"
        call SlimvSwankResponse()
        if s:SinceVersion( '2011-12-04' )
            execute s:py_cmd . "swank_require('swank-repl')"
            call SlimvSwankResponse()
        endif
        if s:SinceVersion( '2008-12-23' )
            call SlimvCommandGetResponse( ':create-repl', s:py_cmd . 'swank_create_repl()', g:slimv_timeout )
        endif
        let s:swank_connected = 1
        redraw
        echon "\rConnected to SWANK server on port " . g:swank_port . "."
        if exists( "g:swank_block_size" ) && SlimvGetFiletype() == 'lisp'
            " Override SWANK connection output buffer size
            if s:SinceVersion( '2014-09-08' )
                let cmd = "(progn (setf (slot-value (swank::connection.user-output swank::*emacs-connection*) 'swank/gray::buffer)"
            else
                let cmd = "(progn (setf (slot-value (swank::connection.user-output swank::*emacs-connection*) 'swank-backend::buffer)"
            endif
            let cmd = cmd . " (make-string " . g:swank_block_size . ")) nil)"
            call SlimvSend( [cmd], 0, 1 )
        endif
        if exists( "*SlimvReplInit" )
            " Perform implementation specific REPL initialization if supplied
            call SlimvReplInit( s:lisp_version )
        endif
    endif

    return s:swank_connected
endfunction

" Send argument to Lisp server for evaluation
function! SlimvSend( args, echoing, output )
    if ! SlimvConnectSwank()
        return
    endif

    " Send the lines to the client for evaluation
    let text = join( a:args, "\n" ) . "\n"

    let s:refresh_disabled = 1
    let s:swank_form = text
    if a:echoing && g:slimv_echolines != 0
        if g:slimv_echolines > 0
            let nlpos = match( s:swank_form, "\n", 0, g:slimv_echolines )
            if nlpos > 0
                " Echo only the first g:slimv_echolines number of lines
                let trimmed = strpart( s:swank_form, nlpos )
                let s:swank_form = strpart( s:swank_form, 0, nlpos )
                let ending = s:CloseForm( s:swank_form )
                if ending != 'ERROR'
                    if substitute( trimmed, '\s\|\n', '', 'g' ) == ''
                        " Only whitespaces are trimmed
                        let s:swank_form = s:swank_form . ending . "\n"
                    else
                        " Valuable characters trimmed, indicate it by printing "..."
                        let s:swank_form = s:swank_form . " ..." . ending . "\n"
                    endif
                endif
            endif
        endif
        if a:output
            silent execute s:py_cmd . 'append_repl("s:swank_form", 1)'
        endif
        let s:swank_form = text
    elseif a:output
        " Open a new line for the output
        silent execute s:py_cmd . " append_repl('\\n', 0)"
    endif
    call SlimvCommand( s:py_cmd . 'swank_input("s:swank_form")' )
    let s:swank_package = ''
    let s:swank_package_form = ''
    let s:refresh_disabled = 0
    call SlimvRefreshModeOn()
    call SlimvRefreshReplBuffer()
endfunction

" Eval arguments in Lisp REPL
function! SlimvEval( args )
    call SlimvSend( a:args, 1, 1 )
endfunction

" Send argument silently to SWANK
function! SlimvSendSilent( args )
    call SlimvSend( a:args, 0, 0 )
endfunction

" Set command line after the prompt
function! SlimvSetCommandLine( cmd )
    let line = getline( "." )
    if line( "." ) == s:GetPromptLine()
        " The prompt is in the line marked by b:repl_prompt_line
        let promptlen = len( b:repl_prompt )
    else
        let promptlen = 0
    endif
    if len( line ) > promptlen
        let line = strpart( line, 0, promptlen )
    endif

    if s:GetPromptLine() < line( '$' )
        " Delete extra lines after the prompt
        let c = col( '.' )
        execute (s:GetPromptLine()+1) . ',' . (line('$')) . 'd_'
        call cursor( line('.'), c )
    endif

    let lines = split( a:cmd, '\n' )
    if len(lines) > 0
        let line = line . lines[0]
    endif
    call setline( ".", line )
    if len(lines) > 1
        call append( s:GetPromptLine(), lines[1:] )
    endif
    set nomodified
endfunction

" Add command list to the command history
function! SlimvAddHistory( cmd )
    if !exists( 'g:slimv_cmdhistory' )
        let g:slimv_cmdhistory = []
    endif
    let i = 0
    let form = join( a:cmd, "\n" )
    " Trim leading and trailing whitespaces from the command
    let form = substitute( form, '^\s*\(.*[^ ]\)\s*', '\1', 'g' )
    if len( form ) > 1 || len( g:slimv_cmdhistory ) == 0 || form != g:slimv_cmdhistory[-1]
        " Add command only if differs from the last one
        call add( g:slimv_cmdhistory, form )
    endif
    let g:slimv_cmdhistorypos = len( g:slimv_cmdhistory )
endfunction

" Recall command from the command history at the marked position
function! SlimvRecallHistory( direction )
    let searchtext = ''
    let l = line( '.' )
    let c = col( '.' )
    let set_cursor_pos = 0
    if line( '.' ) == s:GetPromptLine() && c > b:repl_prompt_col
        " Search for lines beginning with the text up to the cursor position
        let searchtext = strpart( getline('.'), b:repl_prompt_col-1, c-b:repl_prompt_col )
        let searchtext = substitute( searchtext, '^\s*$', '', 'g' )
        let searchtext = substitute( searchtext, '^\s*\(.*[^ ]\)', '\1', 'g' )
    endif
    let historypos = g:slimv_cmdhistorypos
    let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos + a:direction
    while g:slimv_cmdhistorypos >= 0 && g:slimv_cmdhistorypos < len( g:slimv_cmdhistory )
        let cmd = g:slimv_cmdhistory[g:slimv_cmdhistorypos]
        if len(cmd) >= len(searchtext) && strpart(cmd, 0, len(searchtext)) == searchtext
            call SlimvSetCommandLine( g:slimv_cmdhistory[g:slimv_cmdhistorypos] )
            return
        endif
        let g:slimv_cmdhistorypos = g:slimv_cmdhistorypos + a:direction
    endwhile
    if searchtext == ''
        call SlimvSetCommandLine( "" )
    else
        let g:slimv_cmdhistorypos = historypos
    endif
endfunction

" Return missing parens, double quotes, etc to properly close form
function! s:CloseForm( form )
    let end = ''
    let i = 0
    while i < len( a:form )
        if a:form[i] == '"'
            " Inside a string
            let end = '"' . end
            let i += 1
            while i < len( a:form )
                if a:form[i] == '\'
                    " Ignore next character
                    let i += 2
                elseif a:form[i] == '"'
                    let end = end[1:]
                    break
                else
                    let i += 1
                endif
            endwhile
        elseif a:form[i] == ';'
            " Inside a comment
            let end = "\n" . end
            let cend = match(a:form, "\n", i)
            if cend == -1
                break
            endif
            let i = cend
            let end = end[1:]
        else
            " We are outside of strings and comments, now we shall count parens
            if a:form[i] == '('
                let end = ')' . end
            elseif a:form[i] == '[' && SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*'
                let end = ']' . end
            elseif a:form[i] == '{' && SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*'
                let end = '}' . end
            elseif a:form[i] == ')' || ((a:form[i] == ']' || a:form[i] == '}') && SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*')
                if len( end ) == 0 || end[0] != a:form[i]
                    " Oops, too many closing parens or invalid closing paren
                    return 'ERROR'
                endif
                let end = end[1:]
            endif
        endif
        let i += 1
    endwhile
    return end
endfunction

" Some multi-byte characters screw up the built-in lispindent()
" This function is a wrapper that tries to fix it
" TODO: implement custom indent procedure and omit lispindent()
function SlimvLispindent( lnum )
    set lisp
    if SlimvGetFiletype() =~ '.*clojure.*' && exists( '*GetClojureIndent' ) && line('.') == a:lnum
        let li = GetClojureIndent()
    else
        let li = lispindent( a:lnum )
    endif
    set nolisp
    let backline = max([a:lnum-g:slimv_indent_maxlines, 1])
    let oldpos = getpos( '.' )
    call cursor( oldpos[1], 1 )
    " Find containing form
    let [lhead, chead] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
    if lhead == 0
        " No containing form, lispindent() is OK
        call cursor( oldpos[1], oldpos[2] )
        return li
    endif
    " Find outer form
    let [lparent, cparent] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
    call cursor( oldpos[1], oldpos[2] )
    if lparent == 0 || lhead != lparent
        " No outer form or starting above inner form, lispindent() is OK
        return li
    endif
    " Count extra bytes before the function header
    let header = strpart( getline( lparent ), 0 )
    let total_extra = 0
    let extra = 0
    let c = 0
    while a:lnum > 0 && c < chead-1
        let bytes = byteidx( header, c+1 ) - byteidx( header, c )
        if bytes > 1
            let total_extra = total_extra + bytes - 1
            if c >= cparent && extra < 10
                " Extra bytes in the outer function header
                let extra = extra + bytes - 1
            endif
        endif
        let c = c + 1
    endwhile
    if total_extra == 0  
        " No multi-byte character, lispindent() is OK
        return li
    endif
    " In some cases ending spaces add up to lispindent() if there are multi-byte characters
    let ending_sp = len( matchstr( getline( lparent ), ' *$' ) )
    " Determine how wrong lispindent() is based on the number of extra bytes
    " These values were determined empirically
    if lparent == a:lnum - 1
        " Function header is in the previous line
        if extra == 0 && total_extra > 1
            let ending_sp = ending_sp + 1
        endif
        return li + [0, 1, 0, -3, -3, -3, -5, -5, -7, -7, -8][extra] - ending_sp
    else
        " Function header is in an upper line
        if extra == 0 || total_extra == extra
            let ending_sp = 0
        endif
        return li + [0, 1, 0, -2, -2, -3, -3, -3, -3, -3, -3][extra] - ending_sp
    endif
endfunction

" Return Lisp source code indentation at the given line
" Does not keep the cursor position
function! SlimvIndentUnsafe( lnum )
    if &autoindent == 0 || a:lnum <= 1
        " Start of the file
        return 0
    endif
    let pnum = prevnonblank(a:lnum - 1)
    if pnum == 0
        " Hit the start of the file, use zero indent.
        return 0
    endif
    let oldpos = getpos( '.' )
    let linenum = a:lnum

    " Handle multi-line string
    let plen = len( getline( pnum ) )
    if synIDattr( synID( pnum, plen, 0), 'name' ) =~ '[Ss]tring' && getline(pnum)[plen-1] != '"'
        " Previous non-blank line ends with an unclosed string, so this is a multi-line string
        let [l, c] = searchpairpos( '"', '', '"', 'bnW', s:skip_q )
        if l == pnum && c > 0
            " Indent to the opening double quote (if found)
            return c
        else
            return SlimvLispindent( linenum )
        endif
    endif
    if synIDattr( synID( pnum, 1, 0), 'name' ) =~ '[Ss]tring' && getline(pnum)[0] != '"'
        " Previous non-blank line is the last line of a multi-line string
        call cursor( pnum, 1 )
        " First find the end of the multi-line string (omit \" characters)
        let [lend, cend] = searchpos( '[^\\]"', 'nW' )
        if lend > 0 && strpart(getline(lend), cend+1) =~ '(\|)\|\[\|\]\|{\|}'
            " Structural change after the string, no special handling
        else
            " Find the start of the multi-line string (omit \" characters)
            let [l, c] = searchpairpos( '"', '', '"', 'bnW', s:skip_q )
            if l > 0 && strpart(getline(l), 0, c-1) =~ '^\s*$'
                " Nothing else before the string: indent to the opening "
                return c - 1
            endif
            if l > 0
                " Pretend that we are really after the first line of the multi-line string
                let pnum = l
                let linenum = l + 1
            endif
        endif
        call cursor( oldpos[1], oldpos[2] )
    endif

    " Handle special indentation style for flet, labels, etc.
    " When searching for containing forms, don't go back
    " more than g:slimv_indent_maxlines lines.
    let backline = max([pnum-g:slimv_indent_maxlines, 1])
    let indent_keylists = g:slimv_indent_keylists

    " Check if the previous line actually ends with a multi-line subform
    let parent = pnum
    let [l, c] = searchpos( ')', 'bW' )
    if l == pnum
        let [l, c] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
        if l > 0
            " Make sure it is not a top level form and the containing form starts in the same line
            let [l2, c2] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
            if l2 == l
                " Remember the first line of the multi-line form
                let parent = l
            endif
        endif
    endif

    " Find beginning of the innermost containing form
    call cursor( oldpos[1], 1 )
    let [l, c] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
    if l > 0
        if SlimvGetFiletype() =~ '.*\(clojure\|scheme\|racket\).*'
            " Is this a clojure form with [] binding list?
            call cursor( oldpos[1], oldpos[2] )
            let [lb, cb] = searchpairpos( '\[', '', '\]', 'bW', s:skip_sc, backline )
            if lb >= l && (lb > l || cb > c)
                return cb
            endif
            " Is this a multi-arity function definition?
            let line = strpart( getline(l), c-1 )
            if match( line, '(\s*\[' ) >= 0
                return c + 1
            endif
        endif
        " Is this a form with special indentation?
        let line = strpart( getline(l), c-1 )
        if match( line, '\c^(\s*\('.s:spec_indent.'\)\>' ) >= 0
            " Search for the binding list and jump to its end
            if search( '(' ) > 0
                call searchpair( '(', '', ')', '', s:skip_sc )
                if line('.') == pnum
                    " We are indenting the first line after the end of the binding list
                    return c + 1
                endif
            endif
        elseif l == pnum
            " If the containing form starts above this line then find the
            " second outer containing form (possible start of the binding list)
            let [l2, c2] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
            if l2 > 0
                let line2 = strpart( getline(l2), c2-1 )
                if match( line2, '\c^(\s*\('.s:spec_param.'\)\>' ) >= 0
                    if search( '(' ) > 0
                        if line('.') == l && col('.') == c
                            " This is the parameter list of a special form
                            return c
                        endif
                    endif
                endif
                if SlimvGetFiletype() !~ '.*clojure.*'
                    if l2 == l && match( line2, '\c^(\s*\('.s:binding_form.'\)\>' ) >= 0
                        " Is this a lisp form with binding list?
                        return c
                    endif
                    if match( line2, '\c^(\s*cond\>' ) >= 0 && match( line, '\c^(\s*t\>' ) >= 0
                        " Is this the 't' case for a 'cond' form?
                        return c
                    endif
                    if match( line2, '\c^(\s*defpackage\>' ) >= 0
                        let indent_keylists = 0
                    endif
                endif
                " Go one level higher and check if we reached a special form
                let [l3, c3] = searchpairpos( '(', '', ')', 'bW', s:skip_sc, backline )
                if l3 > 0
                    " Is this a form with special indentation?
                    let line3 = strpart( getline(l3), c3-1 )
                    if match( line3, '\c^(\s*\('.s:spec_indent.'\)\>' ) >= 0
                        " This is the first body-line of a binding
                        return c + 1
                    endif
                    if match( line3, '\c^(\s*defsystem\>' ) >= 0
                        let indent_keylists = 0
                    endif
                    " Finally go to the topmost level to check for some forms with special keyword indenting
                    let [l4, c4] = searchpairpos( '(', '', ')', 'brW', s:skip_sc, backline )
                    if l4 > 0
                        let line4 = strpart( getline(l4), c4-1 )
                        if match( line4, '\c^(\s*defsystem\>' ) >= 0
                            let indent_keylists = 0
                        endif
                    endif
                endif
            endif
        endif
    endif

    " Restore all cursor movements
    call cursor( oldpos[1], oldpos[2] )

    " Check if the current form started in the previous nonblank line
    if l == parent
        " Found opening paren in the previous line
        let line = getline(l)
        let form = strpart( line, c )
        " Determine the length of the function part up to the 1st argument
        let funclen = matchend( form, '\s*\S*\s*' ) + 1
        " Contract strings, remove comments
        let form = substitute( form, '".\{-}[^\\]"', '""', 'g' )
        let form = substitute( form, ';.*$', '', 'g' )
        " Contract subforms by replacing them with a single character
        let f = ''
        while form != f
            let f = form
            let form = substitute( form, '([^()]*)',     '0', 'g' )
            let form = substitute( form, '([^()]*$',     '0', 'g' )
            let form = substitute( form, '\[[^\[\]]*\]', '0', 'g' )
            let form = substitute( form, '\[[^\[\]]*$',  '0', 'g' )
            let form = substitute( form, '{[^{}]*}',     '0', 'g' )
            let form = substitute( form, '{[^{}]*$',     '0', 'g' )
        endwhile
        " Find out the function name
        let func = matchstr( form, '\<\k*\>' )
        " If it's a keyword, keep the indentation straight
        if indent_keylists && strpart(func, 0, 1) == ':'
            if form =~ '^:\S*\s\+\S'
                " This keyword has an associated value in the same line
                return c
            else
                " The keyword stands alone in its line with no associated value
                return c + 1
            endif
        endif
        " Fix indentation issues not handled by the default lisp.vim
        if SlimvGetFiletype() =~ '.*clojure.*'
            if match( func, 'defn$' ) >= 0
                return c + 1
            endif
        elseif SlimvGetFiletype() =~ '.*\(scheme\|racket\).*'
            if match( func, 'syntax-rules$' ) >= 0
                return c + 1
            endif
        else
            if match( func, 'defgeneric$' ) >= 0 || match( func, 'defsystem$' ) >= 0 || match( func, 'aif$' ) >= 0
                return c + 1
            endif
        endif
        if match( func, 'if$' ) >= 0 || match( func, 'do$' ) >= 0
            return c + funclen - 1
        endif
        " Remove package specification
        let func = substitute(func, '^.*:', '', '')
        if func != '' && s:swank_connected
            " Look how many arguments are on the same line
            " If an argument is actually a multi-line subform, then replace it with a single character
            let form = substitute( form, "([^()]*$", '0', 'g' )
            let form = substitute( form, "[()\\[\\]{}#'`,]", '', 'g' )
            let args_here = len( split( form ) ) - 1
            " Get swank indent info
            let s:indent = ''
            silent execute s:py_cmd . 'get_indent_info("' . func . '")'
            if s:indent != '' && s:indent == args_here
                " The next one is an &body argument, so indent by 2 spaces from the opening '('
                return c + 1
            endif
            let llen = len( line )
            if synIDattr( synID( l, llen, 0), 'name' ) =~ '[Ss]tring' && line[llen-1] != '"'
                " Parent line ends with a multi-line string
                " lispindent() fails to handle it correctly
                if s:indent == '' && args_here > 0
                    " No &body argument, ignore lispindent() and indent to the 1st argument
                    return c + funclen - 1
                endif
            endif
        endif
    endif

    " Use default Lisp indenting
    let li = SlimvLispindent(linenum)
    let line = strpart( getline(linenum-1), li-1 )
    let gap = matchend( line, '^(\s\+\S' )
    if gap >= 0
        " Align to the gap between the opening paren and the first atom
        return li + gap - 2
    endif
    return li
endfunction 

" Indentation routine, keeps original cursor position
function! SlimvIndent( lnum )
    if exists("g:slimv_indent_disable") && g:slimv_indent_disable
        let indent = lispindent( a:lnum )
    else
        let oldpos = getpos( '.' )
        let indent = SlimvIndentUnsafe( a:lnum )
        call cursor( oldpos[1], oldpos[2] )
    endif
    return indent
endfunction

" Convert indent value to spaces or a mix of tabs and spaces
" depending on the value of 'expandtab'
function! s:MakeIndent( indent )
    if &expandtab
        return repeat( ' ', a:indent )
    else
        return repeat( "\<Tab>", a:indent / &tabstop ) . repeat( ' ', a:indent % &tabstop )
    endif
endfunction

" Send command line to REPL buffer
" Arguments: close = add missing closing parens
function! SlimvSendCommand( close )
    call SlimvRefreshModeOn()
    let lastline = s:GetPromptLine()
    let lastcol  = b:repl_prompt_col
    if lastline > 0
        if line( "." ) >= lastline
            " Trim the prompt from the beginning of the command line
            " The user might have overwritten some parts of the prompt
            let cmdline = getline( lastline )
            let c = 0
            while c < lastcol - 1 && cmdline[c] == b:repl_prompt[c]
                let c = c + 1
            endwhile
            let cmd = [ strpart( getline( lastline ), c ) ]

            " Build a possible multi-line command
            let l = lastline + 1
            while l <= line("$")
                call add( cmd, strpart( getline( l ), 0) )
                let l = l + 1
            endwhile

            " Count the number of opening and closing braces
            let end = s:CloseForm( join( cmd, "\n" ) )
            if end == 'ERROR'
                " Too many closing parens
                call SlimvErrorWait( "Too many or invalid closing parens found." )
                return
            endif
            let echoing = 0
            if a:close && end != ''
                " Close form if necessary and evaluate it
                let cmd[len(cmd)-1] = cmd[len(cmd)-1] . end
                let end = ''
                let echoing = 1
            endif
            if end == ''
                " Expression finished, let's evaluate it
                " but first add it to the history
                call SlimvAddHistory( cmd )
                " Evaluate, but echo only when form is actually closed here
                call SlimvSend( cmd, echoing, 1 )
            else
                " Expression is not finished yet, indent properly and wait for completion
                " Indentation works only if lisp indentation is switched on
                call SlimvArglist()
                let l = line('.') + 1
                call append( '.', '' )
                call setline( l, s:MakeIndent( SlimvIndent(l) ) )
                normal! j$
            endif
        endif
    else
        silent execute s:py_cmd . " append_repl('Slimv error: previous EOF mark not found, re-enter last form:\\n', 0)"
    endif
endfunction

" Close current top level form by adding the missing parens
function! SlimvCloseForm()
    let l2 = line( '.' )
    call SlimvFindDefunStart()
    let l1 = line( '.' )
    let form = []
    let l = l1
    while l <= l2
        call add( form, getline( l ) )
        let l = l + 1
    endwhile
    let end = s:CloseForm( join( form, "\n" ) )
    if end == 'ERROR'
        " Too many closing parens
        call SlimvErrorWait( "Too many or invalid closing parens found." )
    elseif end != ''
        " Add missing parens
        if end[0] == "\n"
            call append( l2, end[1:] )
        else
            call setline( l2, getline( l2 ) . end )
        endif
    endif
    normal! %
endfunction

" Handle insert mode 'Enter' keypress
function! SlimvHandleEnter()
    let s:arglist_line = line('.')
    let s:arglist_col = col('.')
    if pumvisible()
        " Pressing <CR> in a pop up selects entry.
        return "\<C-Y>"
    else
        if exists( 'g:paredit_mode' ) && g:paredit_mode && g:paredit_electric_return
            " Apply electric return
            return PareditEnter()
        else
            " No electric return handling, just enter a newline
            return "\<CR>"
        endif
    endif
endfunction

" Display arglist after pressing Enter
function! SlimvArglistOnEnter()
    let retval = ""
    if s:arglist_line > 0
        if col('.') > len(getline('.'))
            " Stay at the end of line
            let retval = "\<End>"
        endif
        let l = line('.')
        if getline(l) == ''
            " Add spaces to make the correct indentation
            call setline( l, s:MakeIndent( SlimvIndent(l) ) )
            normal! $
        endif
        call SlimvArglist( s:arglist_line, s:arglist_col )
    endif
    let s:arglist_line = 0
    let s:arglist_col = 0

    " This function is called from <C-R>= mappings, return additional keypress
    return retval
endfunction

" Handle insert mode 'Tab' keypress by doing completion or indentation
function! SlimvHandleTab()
    if pumvisible()
        " Completions menu is active, go to next match
        return "\<C-N>"
    endif
    let c = col('.')
    if c > 1 && getline('.')[c-2] =~ '\k'
        " At the end of a keyword, bring up completions
        return "\<C-X>\<C-O>"
    endif
    let indent = SlimvIndent(line('.'))
    if c-1 < indent && getline('.') !~ '\S\+'
        " We are left from the autoindent position, do an autoindent
        call setline( line('.'), s:MakeIndent( indent ) )
        return "\<End>"
    endif
    " No keyword to complete, no need for autoindent, just enter a <Tab>
    return "\<Tab>"
endfunction

" Handle insert mode 'Backspace' keypress in the REPL buffer
function! SlimvHandleBS()
    if line( "." ) == s:GetPromptLine() && col( "." ) <= b:repl_prompt_col
        if col( "." ) == b:repl_prompt_col
            return "\<BS> "
        else
            " No BS allowed before the previous EOF mark
            return ""
        endif
    else
        return "\<BS>"
    endif
endfunction

" Handle insert mode Ctrl-W keypress in the REPL buffer
function! SlimvHandleCW()
    if line( "." ) == s:GetPromptLine()
        let trim_prompt = substitute( b:repl_prompt, '\s\+$', '', 'g' )
        let promptlen = len( trim_prompt )
        if col( "." ) > promptlen
            let after_prompt = strpart( getline("."), promptlen-1, col(".")-promptlen )
        else
            let after_prompt = ''
        endif
        let word = matchstr( after_prompt, '^.*\s\S' )
        if len( word ) == 0
            " No word found after prompt, C-W not allowed
            return ""
        endif
    endif
    return "\<C-W>"
endfunction

" Recall previous command from command history
function! s:PreviousCommand()
    if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos > 0
        call SlimvRecallHistory( -1 )
    endif
endfunction

" Recall next command from command history
function! s:NextCommand()
    if exists( 'g:slimv_cmdhistory' ) && g:slimv_cmdhistorypos < len( g:slimv_cmdhistory )
        call SlimvRecallHistory( 1 )
    else
        call SlimvSetCommandLine( "" )
    endif
endfunction

" Handle insert mode 'Up' keypress in the REPL buffer
function! SlimvHandleUp()
    let save_ve = &virtualedit
    set virtualedit=onemore
    if line( "." ) >= s:GetPromptLine()
        call s:PreviousCommand()
    else
        normal! gk
    endif
    let &virtualedit=save_ve
    return ''
endfunction

" Handle insert mode 'Down' keypress in the REPL buffer
function! SlimvHandleDown()
    let save_ve = &virtualedit
    set virtualedit=onemore
    if line( "." ) >= s:GetPromptLine()
        call s:NextCommand()
    else
        normal! gj
    endif
    let &virtualedit=save_ve
    return ''
endfunction

" Make a fold at the cursor point in the current buffer
function SlimvMakeFold()
    setlocal modifiable
    normal! o    }}}kA {{{0
    setlocal nomodifiable
endfunction

" Handle insert mode 'Enter' keypress in the REPL buffer
function! SlimvHandleEnterRepl()
    " Trim the prompt from the beginning of the command line
    " The user might have overwritten some parts of the prompt
    let lastline = s:GetPromptLine()
    let lastcol  = b:repl_prompt_col
    let cmdline = getline( lastline )
    let c = 0
    while c < lastcol - 1 && cmdline[c] == b:repl_prompt[c]
        let c = c + 1
    endwhile

    " Copy command line up to the cursor position
    if line(".") == lastline
        let cmd = [ strpart( cmdline, c, col(".") - c - 1 ) ]
    else
        let cmd = [ strpart( cmdline, c ) ]
    endif

    " Build a possible multi-line command up to the cursor line/position
    let l = lastline + 1
    while l <= line(".")
        if line(".") == l
            call add( cmd, strpart( getline( l ), 0, col(".") - 1) )
        else
            call add( cmd, strpart( getline( l ), 0) )
        endif
        let l = l + 1
    endwhile

    " Count the number of opening and closing braces in the command before the cursor
    let end = s:CloseForm( join( cmd, "\n" ) )
    if end != 'ERROR' && end != ''
        " Command part before cursor is unbalanced, insert newline
        let s:arglist_line = line('.')
        let s:arglist_col = col('.')
        if pumvisible()
            " Pressing <CR> in a pop up selects entry.
            return "\<C-Y>"
        else
            if exists( 'g:paredit_mode' ) && g:paredit_mode && g:paredit_electric_return && lastline > 0 && line( "." ) >= lastline
                " Apply electric return
                return PareditEnter()
            else
                " No electric return handling, just enter a newline
                return "\<CR>"
            endif
        endif
    else
        " Send current command line for evaluation
        if &virtualedit != 'all'
            call cursor( 0, 99999 )
        endif
        call SlimvSendCommand(0)
    endif
    return ''
endfunction

" Handle normal mode 'Enter' keypress in the SLDB buffer
function! SlimvHandleEnterSldb()
    let line = getline('.')
    if s:sldb_level >= 0
        " Check if Enter was pressed in a section printed by the SWANK debugger
        " The source specification is within a fold, so it has to be tested first
        let mlist = matchlist( line, '^\s\+in "\=\(.*\)"\= \(line\|byte\) \(\d\+\)$' )
        if len(mlist)
            if g:slimv_repl_split
                " Switch back to other window
                execute "wincmd p"
            endif
            " Jump to the file at the specified position
            if mlist[2] == 'line'
                exec ":edit +" . mlist[3] . " " . mlist[1]
            else
                exec ":edit +" . mlist[3] . "go " . mlist[1]
            endif
            return
        endif
        if foldlevel('.')
            " With a fold just toggle visibility
            normal za
            return
        endif
        let item = matchstr( line, s:frame_def )
        if item != ''
            let item = substitute( item, '\s\|:', '', 'g' )
            if search( '^Backtrace:', 'bnW' ) > 0
                " Display item-th frame
                call SlimvMakeFold()
                silent execute s:py_cmd . 'swank_frame_locals("' . item . '")'
                if SlimvGetFiletype() != 'scheme' && g:slimv_impl != 'clisp'
                    " Not implemented for CLISP or scheme
                    silent execute s:py_cmd . 'swank_frame_source_loc("' . item . '")'
                endif
                if SlimvGetFiletype() == 'lisp' && g:slimv_impl != 'clisp' && g:slimv_impl != 'allegro'
                    " Not implemented for CLISP or other lisp dialects
                    silent execute s:py_cmd . 'swank_frame_call("' . item . '")'
                endif
                call SlimvRefreshReplBuffer()
                return
            endif
            if search( '^Restarts:', 'bnW' ) > 0
                " Apply item-th restart
                call SlimvQuitSldb()
                silent execute s:py_cmd . 'swank_invoke_restart("' . s:sldb_level . '", "' . item . '")'
                call SlimvRefreshReplBuffer()
                return
            endif
        endif
    endif

    " No special treatment, perform the original function
    execute "normal! \<CR>"
endfunction

" Restore Inspector cursor position if the referenced title has already been visited
function SlimvSetInspectPos( title )
    if exists( 'b:inspect_pos' ) && has_key( b:inspect_pos, a:title )
        call winrestview( b:inspect_pos[a:title] )
    else
        normal! gg0
    endif
endfunction

" Handle normal mode 'Enter' keypress in the Inspector buffer
function! SlimvHandleEnterInspect()
    let line = getline('.')
    if line[0:9] == 'Inspecting'
        " Reload inspected item
        call SlimvSendSilent( ['[0]'] )
        return
    endif

    " Find the closest [dd] or <dd> token to the left of the cursor
    let [l, c] = searchpos( '{\[\d\+\]', 'bncW' )
    let [l2, c2] = searchpos( '{<\d\+>', 'bncW' )
    if l < line('.') || (l2 == line('.') && c2 > c)
        let l = l2
        let c = c2
    endif

    if l < line('.')
        " No preceding token found, find the closest [dd] or <dd> to the right
        let [l, c] = searchpos( '{\[\d\+\]', 'ncW' )
        let [l2, c2] = searchpos( '{<\d\+>', 'ncW' )
        if l == 0 || l > line('.') || (l2 == line('.') && c2 < c)
            let l = l2
            let c = c2
        endif
    endif

    if l == line( '.' )
        " Keep the relevant part of the line
        let line = strpart( line, c )
    endif

    if exists( 'b:inspect_title' ) && b:inspect_title != ''
        " Save cursor position in case we'll return to this page later on
        if !exists( 'b:inspect_pos' )
            let b:inspect_pos = {}
        endif
	let b:inspect_pos[b:inspect_title] = winsaveview()
    endif

    if line[0] == '['
        if line =~ '^\[--more--\]$'
            " More data follows, fetch next part
            call SlimvCommand( s:py_cmd . 'swank_inspector_range()' )
            call SlimvRefreshReplBuffer()
            return
        elseif line =~ '^\[--all---\]$'
            " More data follows, fetch all parts
            echon "\rFetching all entries, please wait..."
            let b:inspect_more = -1
            call SlimvCommand( s:py_cmd . 'swank_inspector_range()' )
            call SlimvRefreshReplBuffer()
            let starttime = localtime()
            while b:inspect_more < 0 && localtime()-starttime < g:slimv_timeout
                " Wait for the first swank_inspector_range() call to finish
                call SlimvRefreshReplBuffer()
            endwhile
            let starttime = localtime()
            while b:inspect_more > 0 && localtime()-starttime < g:slimv_timeout
                " There are more parts to fetch (1 entry is usually 4 parts)
                echon "\rFetching all entries, please wait [" . (b:inspect_more / 4) . "]"
                call SlimvCommand( s:py_cmd . 'swank_inspector_range()' )
                call SlimvRefreshReplBuffer()
                if getchar(1)
                    " User is impatient, stop fetching
                    break
                endif
            endwhile
            if b:inspect_more > 0
                echon "\rFetch exhausted. Select [--all---] to resume."
            else
                echon "\rSuccessfully fetched all entries."
            endif
            return
        elseif line[0:3] == '[<<]'
            " Pop back up in the inspector
            let item = '-1'
        else
            " Inspect n-th part
            let item = matchstr( line, '\d\+' )
            if item != ''
                " Add item name to the object path
                let entry = matchstr(line, '\[\d\+\]\s*\zs.\{-}\ze\s*\[\]}')
                if entry == ''
                    let entry = matchstr(line, '\[\d\+\]\s*\zs.*')
                endif
                if entry == ''
                    let entry = 'Unknown object'
                endif
                if len( entry ) > 40
                    " Crop if too long
                    let entry = strpart( entry, 0, 37 ) . '...'
                endif
                let s:inspect_path = s:inspect_path + [entry]
            endif
        endif
        if item != ''
            call SlimvSendSilent( ['[' . item . ']'] )
            return
        endif
    endif

    if line[0] == '<'
        " Inspector n-th action
        let item = matchstr( line, '\d\+' )
        if item != ''
            call SlimvSendSilent( ['<' . item . '>'] )
            return
        endif
    endif

    " No special treatment, perform the original function
    execute "normal! \<CR>"
endfunction

" Go to command line and recall previous command from command history
function! SlimvPreviousCommand()
    let save_ve = &virtualedit
    set virtualedit=onemore
    call SlimvEndOfReplBuffer(0)
    if line( "." ) >= s:GetPromptLine()
        call s:PreviousCommand()
    endif
    let &virtualedit=save_ve
endfunction

" Go to command line and recall next command from command history
function! SlimvNextCommand()
    let save_ve = &virtualedit
    set virtualedit=onemore
    call SlimvEndOfReplBuffer(0)
    if line( "." ) >= s:GetPromptLine()
        call s:NextCommand()
    endif
    let &virtualedit=save_ve
endfunction

" Handle interrupt (Ctrl-C) keypress in the REPL buffer
function! SlimvInterrupt()
    call SlimvCommand( s:py_cmd . 'swank_interrupt()' )
    call SlimvRefreshReplBuffer()
endfunction

" Select a specific restart in debugger
" Added in frame argument to support stepper
function! SlimvDebugCommand( name, cmd, frame )
    if SlimvConnectSwank()
        if s:sldb_level >= 0
            if bufname('%') != g:slimv_sldb_name
                call SlimvOpenSldbBuffer()
            endif
            if a:frame == ''
                call SlimvCommand( s:py_cmd . '' . a:cmd . '()' )
            else
                call SlimvCommand( s:py_cmd . '' . a:cmd . '(' . string(a:frame) . ')' )
            endif
            call SlimvRefreshReplBuffer()
            if s:sldb_level < 0
                " Swank exited the debugger
                if bufname('%') != g:slimv_sldb_name
                    call SlimvOpenSldbBuffer()
                endif
                call SlimvQuitSldb()
            else
                echomsg 'Debugger re-activated by the SWANK server.'
            endif
        else
            call SlimvError( "Debugger is not activated." )
        endif
    endif
endfunction

" Various debugger restarts
function! SlimvDebugAbort()
    call SlimvDebugCommand( ":sldb-abort", "swank_invoke_abort", '' )
endfunction

function! SlimvDebugQuit()
    call SlimvDebugCommand( ":throw-to-toplevel", "swank_throw_toplevel", '' )
endfunction

function! SlimvDebugContinue()
    let in_stepper = search ('STEP-CONTINUE', 'bcnw')
    if in_stepper == 0
        call SlimvDebugCommand( ":sldb-continue", "swank_invoke_continue", '' )
    else
        call SlimvQuitSldb()
        silent execute s:py_cmd . 'swank_invoke_restart("' . s:sldb_level . '", "' . string(0) . '")'
        call SlimvRefreshReplBuffer()
    endif
endfunction

" Debugger stepper functions
function! SlimvDebugStepInto()
    let frame = s:DebugFrame()
    if frame != ''
        call SlimvDebugCommand( ":sldb-step", "swank_step_into", frame )
    endif
endfunction

function! SlimvDebugStepNext()
    let frame = s:DebugFrame()
    if frame != ''
        call SlimvDebugCommand( ":sldb-next", "swank_step_next", frame )
    endif
endfunction

function! SlimvDebugStepOut()
    let frame = s:DebugFrame()
    if frame != ''
        call SlimvDebugCommand( ":sldb-out", "swank_step_out", frame )
    endif
endfunction

" Restart execution of the frame with the same arguments
function! SlimvDebugRestartFrame()
    let frame = s:DebugFrame()
    if frame != ''
        call SlimvCommand( s:py_cmd . 'swank_restart_frame("' . frame . '")' )
        call SlimvRefreshReplBuffer()
    endif
endfunction

" List current Lisp threads
function! SlimvListThreads()
    if SlimvConnectSwank()
        call SlimvCommand( s:py_cmd . 'swank_list_threads()' )
        call SlimvRefreshReplBuffer()
    endif
endfunction

" Kill thread(s) selected from the Thread List
function! SlimvKillThread() range
    if SlimvConnectSwank()
        if a:firstline == a:lastline
            let line = getline('.')
            let item = matchstr( line, '\d\+' )
            if bufname('%') != g:slimv_threads_name
                " We are not in the Threads buffer, not sure which thread to kill
                let item = input( 'Thread to kill: ', item )
            endif
            if item != ''
                call SlimvCommand( s:py_cmd . 'swank_kill_thread(' . item . ')' )
                call SlimvRefreshReplBuffer()
            endif
            echomsg 'Thread ' . item . ' is killed.'
        else
            for line in getline(a:firstline, a:lastline)
                let item = matchstr( line, '\d\+' )
                if item != ''
                    call SlimvCommand( s:py_cmd . 'swank_kill_thread(' . item . ')' )
                endif
            endfor
            call SlimvRefreshReplBuffer()
        endif
        call SlimvListThreads()
    endif
endfunction

" Debug thread selected from the Thread List
function! SlimvDebugThread()
    if SlimvConnectSwank()
        let line = getline('.')
        let item = matchstr( line, '\d\+' )
        let item = input( 'Thread to debug: ', item )
        if item != ''
            call SlimvCommand( s:py_cmd . 'swank_debug_thread(' . item . ')' )
            call SlimvRefreshReplBuffer()
        endif
    endif
endfunction

function! SlimvRFunction()
    " search backwards for the alphanums before a '('
    let l = line('.')
    let c = col('.') - 1
    let line = (getline('.'))[0:c]
    let list = matchlist(line, '\([a-zA-Z0-9_.]\+\)\s*(')
    if !len(list)
        return ""
    endif
    let valid = filter(reverse(list), 'v:val != ""')
    return valid[0]
endfunction

" Display function argument list
" Optional argument is the number of characters typed after the keyword
function! SlimvArglist( ... )
    let retval = ''
    let save_ve = &virtualedit
    set virtualedit=all
    if a:0
        " Symbol position supplied
        let l = a:1
        let c = a:2 - 1
        let line = getline(l)
    else
        " Check symbol at cursor position
        let l = line('.')
        let line = getline(l)
        let c = col('.') - 1
        if c >= len(line)
            " Stay at the end of line
            let c = len(line) - 1
            let retval = "\<End>"
        endif
        if line[c-1] == ' '
            " Is this the space we have just inserted in a mapping?
            let c = c - 1
        endif
    endif
    call s:SetKeyword()
    if s:swank_connected && !s:read_string_mode && c > 0 && line[c-1] =~ '\k\|)\|\]\|}\|"'
        " Display only if entering the first space after a keyword
        let arg = ''
        if SlimvGetFiletype() == 'r'
            let arg = SlimvRFunction()
        else
            let matchb = max( [l-200, 1] )
            let [l0, c0] = searchpairpos( '(', '', ')', 'nbW', s:skip_sc, matchb )
            if l0 > 0
                " Found opening paren, let's find out the function name
                while arg == '' && l0 <= l
                    let funcline = substitute( getline(l0), ';.*$', '', 'g' )
                    let arg = matchstr( funcline, '\<\k*\>', c0 )
                    let l0 = l0 + 1
                    let c0 = 0
                endwhile
            endif
        endif

        if arg != ''
            " Ask function argument list from SWANK
            call SlimvFindPackage()
            let msg = SlimvCommandGetResponse( ':operator-arglist', s:py_cmd . 'swank_op_arglist("' . arg . '")', 0 )
            if msg != ''
                " Print argument list in status line with newlines removed.
                " Disable showmode until the next ESC to prevent
                " immeditate overwriting by the "-- INSERT --" text.
                set noshowmode
                let msg = substitute( msg, "\n", "", "g" )
                redraw
                if SlimvGetFiletype() == 'r'
                    call SlimvShortEcho( arg . '(' . msg . ')' )
                elseif match( msg, "\\V" . arg ) != 1 " Use \V ('very nomagic') for exact string match instead of regex 
                    " Function name is not received from REPL
                    call SlimvShortEcho( "(" . arg . ' ' . msg[1:] )
                else
                    call SlimvShortEcho( msg )
                endif
            endif
        endif
    endif

    " This function is also called from <C-R>= mappings, return additional keypress
    let &virtualedit=save_ve
    return retval
endfunction

" Start and connect swank server
function! SlimvConnectServer()
    if s:swank_connected
        execute s:py_cmd . "swank_disconnect()"
        let s:swank_connected = 0
	" Give swank server some time for disconnecting
        sleep 500m
    endif 
    if SlimvConnectSwank()
        let repl_win = bufwinnr( s:repl_buf )
        if s:repl_buf == -1 || ( g:slimv_repl_split && repl_win == -1 )
            call SlimvOpenReplBuffer()
        endif
    endif
endfunction

" Get the last region (visual block)
function! SlimvGetRegion(first, last)
    let oldpos = winsaveview()
    if a:first < a:last || ( a:first == line( "'<" ) && a:last == line( "'>" ) )
        let lines = getline( a:first, a:last )
    else
        " No range was selected, select current paragraph
        normal! vap
        execute "normal! \<Esc>"
        call winrestview( oldpos ) 
        let lines = getline( "'<", "'>" )
        if lines == [] || lines == ['']
            call SlimvError( "No range selected." )
            return []
        endif
    endif
    let firstcol = col( "'<" ) - 1
    let lastcol  = col( "'>" ) - 2
    if lastcol >= 0
        let lines[len(lines)-1] = lines[len(lines)-1][ : lastcol]
    else
        let lines[len(lines)-1] = ''
    endif
    let lines[0] = lines[0][firstcol : ]

    " Find and set package/namespace definition preceding the region
    call SlimvFindPackage()
    call winrestview( oldpos ) 
    return lines
endfunction

" Eval buffer lines in the given range
function! SlimvEvalRegion() range
    if v:register == '"' || v:register == '+'
        let lines = SlimvGetRegion(a:firstline, a:lastline)
    else
        " Register was passed, so eval register contents instead
        let reg = getreg( v:register )
        let ending = ""
        if SlimvGetFiletype() != 'r'
            let ending = s:CloseForm( reg )
            if ending == 'ERROR'
                call SlimvError( 'Too many or invalid closing parens in register "' . v:register )
                return
            endif
        endif
        let lines = [reg . ending]
    endif
    if lines != []
        if SlimvGetFiletype() == 'scheme'
            " Swank-scheme requires us to pass a single s-expression
            " so embed buffer lines in a (begin ...) block
            let lines = ['(begin'] + lines + [')']
        endif
        call SlimvEval( lines )
    endif
endfunction

" Eval contents of the 's' register, optionally store it in another register
" Also optionally append a test form for quick testing (not stored in 'outreg')
" If the test form contains '%1' then it 'wraps' the selection around the '%1'
function! SlimvEvalSelection( outreg, testform )
    let sel = SlimvGetSelection()
    if a:outreg != '"' && a:outreg != '+'
        " Register was passed, so store current selection in register
        call setreg( a:outreg, s:swank_package_form . sel)
    endif
    let lines = [sel]
    if a:testform != ''
        if match( a:testform, '%1' ) >= 0
            " We need to wrap the selection in the testform
            if match( sel, "\n" ) < 0
                " The selection is a single line, keep the wrapped form in one line
                let sel = substitute( a:testform, '%1', sel, 'g' )
                let lines = [sel]
            else
                " The selection is multiple lines, wrap it by adding new lines
                let lines = [strpart( a:testform, 0, match( a:testform, '%1' ) ),
                \            sel,
                \            strpart( a:testform, matchend( a:testform, '%1' ) )]
            endif
        else
            " Append optional test form at the tail
            let lines = lines + [a:testform]
        endif
    endif
    if exists( 'b:slimv_repl_buffer' )
        " If this is the REPL buffer then go to EOF
        call s:EndOfBuffer()
    endif
    call SlimvEval( lines )
endfunction

" Eval Lisp form.
" Form given in the template is passed to Lisp without modification.
function! SlimvEvalForm( template )
    let lines = [a:template]
    call SlimvEval( lines )
endfunction

" Eval Lisp form, with the given parameter substituted in the template.
" %1 string is substituted with par1
function! SlimvEvalForm1( template, par1 )
    let p1 = escape( a:par1, '&' )
    let temp1 = substitute( a:template, '%1', p1, 'g' )
    let lines = [temp1]
    call SlimvEval( lines )
endfunction

" Eval Lisp form, with the given parameters substituted in the template.
" %1 string is substituted with par1
" %2 string is substituted with par2
function! SlimvEvalForm2( template, par1, par2 )
    let p1 = escape( a:par1, '&' )
    let p2 = escape( a:par2, '&' )
    let temp1 = substitute( a:template, '%1', p1, 'g' )
    let temp2 = substitute( temp1,      '%2', p2, 'g' )
    let lines = [temp2]
    call SlimvEval( lines )
endfunction


" =====================================================================
"  Special functions
" =====================================================================

" Evaluate and test top level form at the cursor pos
function! SlimvEvalTestDefun( testform )
    let outreg = v:register
    let oldpos = winsaveview()
    if !SlimvSelectDefun()
        return
    endif
    call SlimvFindPackage()
    call winrestview( oldpos ) 
    call SlimvEvalSelection( outreg, a:testform )
endfunction

" Evaluate top level form at the cursor pos
function! SlimvEvalDefun()
    call SlimvEvalTestDefun( '' )
endfunction

" Evaluate the whole buffer
function! SlimvEvalBuffer()
    if exists( 'b:slimv_repl_buffer' )
        call SlimvError( "Cannot evaluate the REPL buffer." )
        return
    endif
    let first_line = 1
    if getline( first_line )[0] == '#'
        " skip shebang line
        let first_line += 1
    endif
    let lines = getline( first_line, '$' )
    if SlimvGetFiletype() == 'scheme'
        " Swank-scheme requires us to pass a single s-expression
        " so embed buffer lines in a (begin ...) block
        let lines = ['(begin'] + lines + [')']
    endif
    call SlimvEval( lines )
endfunction

" Return frame number if we are in the Backtrace section of the debugger
function! s:DebugFrame()
    if s:swank_connected && s:sldb_level >= 0
        " Check if we are in SLDB
        let sldb_buf = bufnr( '^' . g:slimv_sldb_name . '$' )
        if sldb_buf != -1 && sldb_buf == bufnr( "%" )
            let bcktrpos = search( '^Backtrace:', 'bcnw' )
            let framepos = line( '.' )
            if matchstr( getline('.'), s:frame_def ) == ''
                let framepos = search( s:frame_def, 'bcnw' )
            endif
            if framepos > 0 && bcktrpos > 0 && framepos > bcktrpos
                let line = getline( framepos )
                let item = matchstr( line, s:frame_def )
                if item != ''
                    return substitute( item, '\s\|:', '', 'g' )
                endif
            endif
        endif
    endif
    return ''
endfunction

" Evaluate and test current s-expression at the cursor pos
function! SlimvEvalTestExp( testform )
    let outreg = v:register
    let oldpos = winsaveview()
    if !SlimvSelectForm( 1 )
        return
    endif
    call SlimvFindPackage()
    call winrestview( oldpos ) 
    call SlimvEvalSelection( outreg, a:testform )
endfunction

" Evaluate current s-expression at the cursor pos
function! SlimvEvalExp()
    call SlimvEvalTestExp( '' )
endfunction

" Evaluate expression entered interactively
function! SlimvInteractiveEval()
    let frame = s:DebugFrame()
    if frame != ''
        " We are in the debugger, eval expression in the frame the cursor stands on
        let e = input( 'Eval in frame ' . frame . ': ' )
        if e != ''
            let result = SlimvCommandGetResponse( ':eval-string-in-frame', s:py_cmd . 'swank_eval_in_frame("' . e . '", ' . frame . ')', 0 )
            if result != ''
                redraw
                echo result
            endif
        endif
    else
        let e = input( 'Eval: ' )
        if e != ''
            call SlimvEval([e])
        endif
    endif
endfunction

" Undefine function
function! SlimvUndefineFunction()
    if s:swank_connected
        call SlimvCommand( s:py_cmd . 'swank_undefine_function("' . SlimvSelectSymbol() . '")' )
        call SlimvRefreshReplBuffer()
    endif
endfunction

" ---------------------------------------------------------------------

" Macroexpand-1 the current top level form
function! SlimvMacroexpand()
    if SlimvConnectSwank()
        if !SlimvSelectForm( 0 )
            return
        endif
        let s:swank_form = SlimvGetSelection()
        if exists( 'b:slimv_repl_buffer' )
            " If this is the REPL buffer then go to EOF
            call s:EndOfBuffer()
        endif
        call SlimvCommandUsePackage( s:py_cmd . 'swank_macroexpand("s:swank_form")' )
    endif
endfunction

" Macroexpand the current top level form
function! SlimvMacroexpandAll()
    if SlimvConnectSwank()
        if !SlimvSelectForm( 0 )
            return
        endif
        let s:swank_form = SlimvGetSelection()
        if exists( 'b:slimv_repl_buffer' )
            " If this is the REPL buffer then go to EOF
            call s:EndOfBuffer()
        endif
        call SlimvCommandUsePackage( s:py_cmd . 'swank_macroexpand_all("s:swank_form")' )
    endif
endfunction

" Toggle debugger break on exceptions
" Only for ritz-swank 0.4.0 and above
function! SlimvBreakOnException()
    if SlimvGetFiletype() =~ '.*clojure.*' && s:SinceVersion( '2010-11-13' )
        " swank-clojure is abandoned at protocol version 20100404, so it must be ritz-swank
        if SlimvConnectSwank()
            let s:break_on_exception = ! s:break_on_exception
            call SlimvCommand( s:py_cmd . 'swank_break_on_exception(' . s:break_on_exception . ')' )
            call SlimvRefreshReplBuffer()
            echomsg 'Break On Exception ' . (s:break_on_exception ? 'enabled.' : 'disabled.')
        endif
    else
        call SlimvError( "This function is implemented only for ritz-swank." )
    endif
endfunction

" Set a breakpoint on the beginning of a function
function! SlimvBreak()
    if SlimvConnectSwank()
        let s = input( 'Set breakpoint: ', SlimvSelectSymbol() )
        if s != ''
            call SlimvCommandUsePackage( s:py_cmd . 'swank_set_break("' . s . '")' )
            redraw!
        endif
    endif
endfunction

" Switch trace on for the selected function (toggle for swank)
function! SlimvTrace()
    if SlimvGetFiletype() == 'scheme'
        call SlimvError( "Tracing is not supported by swank-scheme." )
        return
    endif
    if SlimvConnectSwank()
        let s = input( '(Un)trace: ', SlimvSelectSymbol() )
        if s != ''
            call SlimvCommandUsePackage( s:py_cmd . 'swank_toggle_trace("' . s . '")' )
            redraw!
        endif
    endif
endfunction

" Switch trace off for the selected function (or all functions for swank)
function! SlimvUntrace()
    if SlimvGetFiletype() == 'scheme'
        call SlimvError( "Tracing is not supported by swank-scheme." )
        return
    endif
    if SlimvConnectSwank()
        let s:refresh_disabled = 1
        call SlimvCommand( s:py_cmd . 'swank_untrace_all()' )
        let s:refresh_disabled = 0
        call SlimvRefreshReplBuffer()
    endif
endfunction

" Disassemble the selected function
function! SlimvDisassemble()
    let symbol = SlimvSelectSymbol()
    if SlimvConnectSwank()
        let s = input( 'Disassemble: ', symbol )
        if s != ''
            call SlimvCommandUsePackage( s:py_cmd . 'swank_disassemble("' . s . '")' )
        endif
    endif
endfunction

" Inspect symbol under cursor
function! SlimvInspect()
    if !SlimvConnectSwank()
        return
    endif
    let s:inspect_path = []
    let frame = s:DebugFrame()
    if frame != ''
        " Inspect selected for a frame in the debugger's Backtrace section
        let line = getline( '.' )
        if matchstr( line, s:frame_def ) != ''
            " This is the base frame line in form '  1: xxxxx'
            let sym = ''
        elseif matchstr( line, '^\s\+in "\(.*\)" \(line\|byte\)' ) != ''
            " This is the source location line
            let sym = ''
        elseif matchstr( line, '^\s\+No source line information' ) != ''
            " This is the no source location line
            let sym = ''
        elseif matchstr( line, '^\s\+Locals:' ) != ''
            " This is the 'Locals' line
            let sym = ''
        else
            let sym = SlimvSelectSymbolExt()
        endif
        let s = input( 'Inspect in frame ' . frame . ' (evaluated): ', sym )
        if s != ''
            let s:inspect_path = [s]
            call SlimvCommand( s:py_cmd . 'swank_inspect_in_frame("' . s . '", ' . frame . ')' )
            call SlimvRefreshReplBuffer()
        endif
    else
        let s = input( 'Inspect: ', SlimvSelectSymbolExt() )
        if s != ''
            let s:inspect_path = [s]
            call SlimvCommandUsePackage( s:py_cmd . 'swank_inspect("' . s . '")' )
        endif
    endif
endfunction

" Cross reference: who calls
function! SlimvXrefBase( text, cmd )
    if SlimvConnectSwank()
        let s = input( a:text, SlimvSelectSymbol() )
        if s != ''
            call SlimvCommandUsePackage( s:py_cmd . 'swank_xref("' . s . '", "' . a:cmd . '")' )
        endif
    endif
endfunction

" Cross reference: who calls
function! SlimvXrefCalls()
    call SlimvXrefBase( 'Who calls: ', ':calls' )
endfunction

" Cross reference: who references
function! SlimvXrefReferences()
    call SlimvXrefBase( 'Who references: ', ':references' )
endfunction

" Cross reference: who sets
function! SlimvXrefSets()
    call SlimvXrefBase( 'Who sets: ', ':sets' )
endfunction

" Cross reference: who binds
function! SlimvXrefBinds()
    call SlimvXrefBase( 'Who binds: ', ':binds' )
endfunction

" Cross reference: who macroexpands
function! SlimvXrefMacroexpands()
    call SlimvXrefBase( 'Who macroexpands: ', ':macroexpands' )
endfunction

" Cross reference: who specializes
function! SlimvXrefSpecializes()
    call SlimvXrefBase( 'Who specializes: ', ':specializes' )
endfunction

" Cross reference: list callers
function! SlimvXrefCallers()
    call SlimvXrefBase( 'List callers: ', ':callers' )
endfunction

" Cross reference: list callees
function! SlimvXrefCallees()
    call SlimvXrefBase( 'List callees: ', ':callees' )
endfunction

" ---------------------------------------------------------------------

" Switch or toggle profiling on for the selected function
function! SlimvProfile()
    if SlimvConnectSwank()
        let s = input( '(Un)profile: ', SlimvSelectSymbol() )
        if s != ''
            call SlimvCommandUsePackage( s:py_cmd . 'swank_toggle_profile("' . s . '")' )
            redraw!
        endif
    endif
endfunction

" Switch profiling on based on substring
function! SlimvProfileSubstring()
    if SlimvConnectSwank()
        let s = input( 'Profile by matching substring: ', SlimvSelectSymbol() )
        if s != ''
            let p = input( 'Package (RET for all packages): ' )
            call SlimvCommandUsePackage( s:py_cmd . 'swank_profile_substring("' . s . '","' . p . '")' )
            redraw!
        endif
    endif
endfunction

" Switch profiling completely off
function! SlimvUnprofileAll()
    if SlimvConnectSwank()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_unprofile_all()' )
    endif
endfunction

" Display list of profiled functions
function! SlimvShowProfiled()
    if SlimvConnectSwank()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_profiled_functions()' )
    endif
endfunction

" Report profiling results
function! SlimvProfileReport()
    if SlimvConnectSwank()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_profile_report()' )
    endif
endfunction

" Reset profiling counters
function! SlimvProfileReset()
    if SlimvConnectSwank()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_profile_reset()' )
    endif
endfunction

" ---------------------------------------------------------------------

" Compile the current top-level form
function! SlimvCompileDefun()
    let oldpos = winsaveview()
    if !SlimvSelectDefun()
        call winrestview( oldpos ) 
        return
    endif
    if SlimvConnectSwank()
        let s:swank_form = SlimvGetSelection()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_compile_string("s:swank_form")' )
    endif
    call winrestview( oldpos )
endfunction

" Compile and load whole file
function! SlimvCompileLoadFile()
    if exists( 'b:slimv_repl_buffer' )
        call SlimvError( "Cannot compile the REPL buffer." )
        return
    endif
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = substitute( filename, '\\', '/', 'g' )
    if &modified
        let answer = SlimvErrorAsk( '', "Save file before compiling [Y/n]?" )
        if answer[0] != 'n' && answer[0] != 'N'
            write
        endif
    endif
    if SlimvConnectSwank()
        let s:compiled_file = ''
        call SlimvCommandUsePackage( s:py_cmd . 'swank_compile_file("' . filename . '")' )
        let starttime = localtime()
        while s:compiled_file == '' && localtime()-starttime < g:slimv_timeout
            call SlimvSwankResponse()
        endwhile
        if s:compiled_file != ''
            let s:compiled_file = substitute( s:compiled_file, '\\', '/', 'g' )
            call SlimvCommandUsePackage( s:py_cmd . 'swank_load_file("' . s:compiled_file . '")' )
            let s:compiled_file = ''
        endif
    endif
endfunction

" Compile whole file
function! SlimvCompileFile()
    if exists( 'b:slimv_repl_buffer' )
        call SlimvError( "Cannot compile the REPL buffer." )
        return
    endif
    let filename = fnamemodify( bufname(''), ':p' )
    let filename = substitute( filename, '\\', '/', 'g' )
    if &modified
        let answer = SlimvErrorAsk( '', "Save file before compiling [Y/n]?" )
        if answer[0] != 'n' && answer[0] != 'N'
            write
        endif
    endif
    if SlimvConnectSwank()
        call SlimvCommandUsePackage( s:py_cmd . 'swank_compile_file("' . filename . '")' )
    endif
endfunction

" Compile buffer lines in the given range
function! SlimvCompileRegion() range
    if v:register == '"' || v:register == '+'
        let lines = SlimvGetRegion(a:firstline, a:lastline)
    else
        " Register was passed, so compile register contents instead
        let reg = getreg( v:register )
        let ending = s:CloseForm( reg )
        if ending == 'ERROR'
            call SlimvError( 'Too many or invalid closing parens in register "' . v:register )
            return
        endif
        let lines = [reg . ending]
    endif
    if lines == []
        return
    endif
    let region = join( lines, "\n" )
    if SlimvConnectSwank()
        let s:swank_form = region
        call SlimvCommandUsePackage( s:py_cmd . 'swank_compile_string("s:swank_form")' )
    endif
endfunction

" ---------------------------------------------------------------------

" Describe the selected symbol
function! SlimvDescribeSymbol()
    if SlimvConnectSwank()
        let symbol = SlimvSelectSymbol()
        if symbol == ''
            call SlimvError( "No symbol under cursor." )
            return
        endif
        call SlimvCommandUsePackage( s:py_cmd . 'swank_describe_symbol("' . symbol . '")' )
    endif
endfunction

" Display symbol description in balloonexpr
function! SlimvDescribe(arg)
    let arg=a:arg
    if a:arg == ''
        let arg = expand('<cword>')
    endif
    " We don't want to try connecting here ... the error message would just 
    " confuse the balloon logic
    if !s:swank_connected || s:read_string_mode
        return ''
    endif
    call SlimvFindPackage()
    let arglist = SlimvCommandGetResponse( ':operator-arglist', s:py_cmd . 'swank_op_arglist("' . arg . '")', 0 )
    if arglist == ''
        " Not able to fetch arglist, assuming function is not defined
        " Skip calling describe, otherwise SWANK goes into the debugger
        return ''
    endif
    let msg = SlimvCommandGetResponse( ':describe-function', s:py_cmd . 'swank_describe_function("' . arg . '")', 0 )
    if msg == ''
        " No describe info, display arglist
        if match( arglist, arg ) != 1
            " Function name is not received from REPL
            return "(" . arg . ' ' . arglist[1:]
        else
            return arglist
        endif
    else
        return substitute(msg,'^\n*','','')
    endif
endfunction

" Apropos of the selected symbol
function! SlimvApropos()
    call SlimvEvalForm1( g:slimv_template_apropos, SlimvSelectSymbol() )
endfunction

" Generate tags file using ctags
function! SlimvGenerateTags()
    if exists( 'g:slimv_ctags' ) && g:slimv_ctags != ''
        execute 'silent !' . g:slimv_ctags
    else
        call SlimvError( "Copy ctags to the Vim path or define g:slimv_ctags." )
    endif
endfunction

" ---------------------------------------------------------------------

" Find word in the CLHS symbol database, with exact or partial match.
" Return either the first symbol found with the associated URL,
" or the list of all symbols found without the associated URL.
function! SlimvFindSymbol( word, exact, all, db, root, init )
    if a:word == ''
        return []
    endif
    if !a:all && a:init != []
        " Found something already at a previous db lookup, no need to search this db
        return a:init
    endif
    let lst = a:init
    let i = 0
    let w = tolower( a:word )
    if a:exact
        while i < len( a:db )
            " Try to find an exact match
            if a:db[i][0] == w
                " No reason to check a:all here
                return [a:db[i][0], a:root . a:db[i][1]]
            endif
            let i = i + 1
        endwhile
    else
        while i < len( a:db )
            " Try to find the symbol starting with the given word
            let w2 = escape( w, '~' )
            if match( a:db[i][0], w2 ) == 0
                if a:all
                    call add( lst, a:db[i][0] )
                else
                    return [a:db[i][0], a:root . a:db[i][1]]
                endif
            endif
            let i = i + 1
        endwhile
    endif

    " Return whatever found so far
    return lst
endfunction

" Lookup word in Common Lisp Hyperspec
function! SlimvLookup( word )
    " First try an exact match
    let w = a:word
    let symbol = []
    while symbol == []
        let symbol = SlimvHyperspecLookup( w, 1, 0 )
        if symbol == []
            " Symbol not found, try a match on beginning of symbol name
            let symbol = SlimvHyperspecLookup( w, 0, 0 )
            if symbol == []
                " We are out of luck, can't find anything
                let msg = 'Symbol ' . w . ' not found. Hyperspec lookup word: '
                let val = ''
            else
                let msg = 'Hyperspec lookup word: '
                let val = symbol[0]
            endif
            " Ask user if this is that he/she meant
            let w = input( msg, val )
            if w == ''
                " OK, user does not want to continue
                return
            endif
            let symbol = []
        endif
    endwhile
    if symbol != [] && len(symbol) > 1
        " Symbol found, open HS page in browser
        if match( symbol[1], ':' ) < 0 && exists( 'g:slimv_hs_root' )
            let page = g:slimv_hs_root . symbol[1]
        else
            " URL is already a fully qualified address
            let page = symbol[1]
        endif
        if exists( "g:slimv_browser_cmd" )
            " We have a given shell command to start the browser
            if !exists( "g:slimv_browser_cmd_suffix" )
                " Fork the browser by default
                let g:slimv_browser_cmd_suffix = '&'
            endif
            silent execute '! ' . g:slimv_browser_cmd . ' ' . page . ' ' . g:slimv_browser_cmd_suffix
        elseif exists( "g:slimv_browser_cmd_ex" )
            " We have a given Ex command to start the browser
            silent execute g:slimv_browser_cmd_ex . ' ' . page
        else
            if g:slimv_windows
                " Run the program associated with the .html extension
                silent execute '! start ' . page
            else
                " On Linux it's not easy to determine the default browser
                if executable( 'xdg-open' )
                    silent execute '! xdg-open ' . page . ' &'
                else
                    " xdg-open not installed, ask help from Python webbrowser package
                    let pycmd = "import webbrowser; webbrowser.open('" . page . "')"
                    silent execute '! python -c "' . pycmd . '"'
                endif
            endif
        endif
        " This is needed especially when using text browsers
        redraw!
    endif
endfunction

" Lookup current symbol in the Common Lisp Hyperspec
function! SlimvHyperspec()
    call SlimvLookup( SlimvSelectSymbol() )
endfunction

" Complete symbol name starting with 'base'
function! SlimvComplete( base )
    " Find all symbols starting with "a:base"
    if a:base == ''
        return []
    endif
    if s:swank_connected && !s:read_string_mode
        " Save current buffer and window in case a swank command causes a buffer change
        let buf = bufnr( "%" )
        if winnr('$') < 2
            let win = 0
        else
            let win = winnr()
        endif

        call SlimvFindPackage()
        if g:slimv_simple_compl
            let msg = SlimvCommandGetResponse( ':simple-completions', s:py_cmd . 'swank_completions("' . a:base . '")', 0 )
        else
            let msg = SlimvCommandGetResponse( ':fuzzy-completions', s:py_cmd . 'swank_fuzzy_completions("' . a:base . '")', 0 )
        endif

        " Restore window and buffer, because it is not allowed to change buffer here
        if win > 0 && winnr() != win
            execute win . "wincmd w"
            let msg = ''
        endif
        if bufnr( "%" ) != buf
            execute "buf " . buf
            let msg = ''
        endif

        if msg != ''
            " We have a completion list from SWANK
            let res = split( msg, '\n' )
            return res
        endif
    endif

    " No completion yet, try to fetch it from the Hyperspec database
    let res = []
    let symbol = SlimvHyperspecLookup( a:base, 0, 1 )
    if symbol == []
        return []
    endif
    call sort( symbol )
    for m in symbol
        if m =~ '^' . escape( a:base, '~' )
            call add( res, m )
        endif
    endfor
    return res
endfunction

" Complete function that uses the Hyperspec database
function! SlimvOmniComplete( findstart, base )
    if a:findstart
        " Locate the start of the symbol name
        call s:SetKeyword()
        let upto = strpart( getline( '.' ), 0, col( '.' ) - 1)
        return match(upto, '\k\+$')
    else
        return SlimvComplete( a:base )
    endif
endfunction

" Define complete function only if none is defined yet
if &omnifunc == ''
    set omnifunc=SlimvOmniComplete
endif

" Complete function for user-defined commands
function! SlimvCommandComplete( arglead, cmdline, cursorpos )
    " Locate the start of the symbol name
    call s:SetKeyword()
    let upto = strpart( a:cmdline, 0, a:cursorpos )
    let base = matchstr(upto, '\k\+$')
    let ext  = matchstr(upto, '\S*\k\+$')
    let compl = SlimvComplete( base )
    if len(compl) > 0 && base != ext
        " Command completion replaces whole word between spaces, so we
        " need to add any prefix present in front of the keyword, like '('
        let prefix = strpart( ext, 0, len(ext) - len(base) )
        let i = 0
        while i < len(compl)
            let compl[i] = prefix . compl[i]
            let i = i + 1
        endwhile
    endif
    return compl
endfunction

" Create a tags file containing the definitions
" of the given symbol, then perform a tag lookup
function! SlimvFindDefinitionsForEmacs( symbol )
    if g:slimv_tags_file == ''
        let msg = ''
    else
        let msg = SlimvCommandGetResponse( ':find-definitions-for-emacs', s:py_cmd . 'swank_find_definitions_for_emacs("' . a:symbol . '")', 0 )
    endif
    try
        if msg != ''
            exec ":tjump " . msg
        else
            exec ":tjump " . a:symbol
        endif
    catch
        call SlimvError( "\r" . v:exception )
    endtry
endfunction

" Lookup definition(s) of the symbol under cursor
function! SlimvFindDefinitions()
    if SlimvConnectSwank()
        let symbol = SlimvSelectSymbol()
        if symbol == ''
            call SlimvError( "No symbol under cursor." )
            return
        endif
        call SlimvFindPackage()
        call SlimvFindDefinitionsForEmacs( symbol )
    endif
endfunction

" Lookup definition(s) of symbol entered in prompt
function! SlimvFindDefinitionsPrompt()
    if SlimvConnectSwank()
        let symbol = input( 'Find Definitions For: ', SlimvSelectSymbol() )
        echon "\r"
        call SlimvFindDefinitionsForEmacs( symbol )
    endif
endfunction

" Set current package
function! SlimvSetPackage()
    if SlimvConnectSwank()
        call SlimvFindPackage()
        let pkg = input( 'Package: ', s:swank_package )
        if pkg != ''
            let s:refresh_disabled = 1
            call SlimvCommand( s:py_cmd . 'swank_set_package("' . pkg . '")' )
            let s:refresh_disabled = 0
            call SlimvRefreshReplBuffer()
        endif
    endif
endfunction

" Close lisp process running the swank server
" and quit REPL buffer
function! SlimvQuitRepl()
    if s:swank_connected
        call SlimvCommand( s:py_cmd . 'swank_quit_lisp()' )
        let s:swank_connected = 0
        let buf = bufnr( '^' . g:slimv_repl_name . '$' )
        if buf != -1
            if g:slimv_repl_split
                " REPL buffer exists, check if it is open in a window
                let win = bufwinnr( buf )
                if win != -1
                    " Switch to the REPL window and close it
                    if winnr() != win
                        execute win . "wincmd w"
                    endif
                    execute "wincmd c"
                endif
            endif
            execute "bd " . buf
        endif
    endif
endfunction

" =====================================================================
"  Slimv keybindings
" =====================================================================

" <Leader> timeouts in 1000 msec by default, if this is too short,
" then increase 'timeoutlen'

" Map keyboard keyset dependant shortcut to command and also add it to menu
function! s:MenuMap( name, shortcut1, shortcut2, command )
    if g:slimv_keybindings == 1
        " Short (one-key) keybinding set
        let shortcut = a:shortcut1
    elseif g:slimv_keybindings == 2
        " Easy to remember (two-key) keybinding set
        let shortcut = a:shortcut2
    else
        " No bindings
        let shortcut = ''
    endif

    if shortcut != ''
        execute "noremap <silent> " . shortcut . " " . a:command
        if a:name != '' && g:slimv_menu == 1
            silent execute "amenu " . a:name . "<Tab>" . shortcut . " " . a:command
        endif
    elseif a:name != '' && g:slimv_menu == 1
        silent execute "amenu " . a:name . " " . a:command
    endif
endfunction

" Initialize buffer by adding buffer specific mappings
function! SlimvInitBuffer()
    " Map space to display function argument list in status line
    if SlimvGetFiletype() == 'r'
        inoremap <silent> <buffer> (          (<C-R>=SlimvArglist()<CR>
    else
        if !exists("g:slimv_unmap_space") || g:slimv_unmap_space == 0
            inoremap <silent> <buffer> <Space>    <Space><C-R>=SlimvArglist()<CR>
        endif
        if !exists("g:slimv_unmap_cr") || g:slimv_unmap_cr == 0
            inoremap <silent> <buffer> <CR>       <C-R>=pumvisible() ?  "\<lt>C-Y>" : SlimvHandleEnter()<CR><C-R>=SlimvArglistOnEnter()<CR>
        endif
    endif
    nnoremap <silent> <buffer> %          :call SlimvFindMatchingPair()<CR>
    "noremap  <silent> <buffer> <C-C>      :call SlimvInterrupt()<CR>
    augroup SlimvInsertLeave
        au!
        au InsertEnter * :let s:save_showmode=&showmode
        au InsertLeave * :let &showmode=s:save_showmode
    augroup END
    inoremap <silent> <buffer> <C-X>0     <C-O>:call SlimvCloseForm()<CR>
    if !exists("g:slimv_unmap_tab") || g:slimv_unmap_tab == 0
        inoremap <silent> <buffer> <Tab>      <C-R>=SlimvHandleTab()<CR>
    endif
    inoremap <silent> <buffer> <S-Tab>    <C-R>=pumvisible() ? "\<lt>C-P>" : "\<lt>S-Tab>"<CR>
    if g:slimv_tags_file != ''
        nnoremap <silent> <buffer> <C-]>      :call SlimvFindDefinitions()<CR>
    endif

    " Setup balloonexp to display symbol description
    if g:slimv_balloon && has( 'balloon_eval' )
        "setlocal balloondelay=100
        setlocal ballooneval
        setlocal balloonexpr=SlimvDescribe(v:beval_text)
    endif
    " This is needed for safe switching of modified buffers
    set hidden
    call s:MakeWindowId()
endfunction

" Edit commands
call s:MenuMap( 'Slim&v.Edi&t.Close-&Form',                     g:slimv_leader.')',  g:slimv_leader.'tc',  ':<C-U>call SlimvCloseForm()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.&Complete-Symbol<Tab>Tab',        '',                  '',                   '<Ins><C-X><C-O>' )
call s:MenuMap( 'Slim&v.Edi&t.Find-&Definitions\.\.\.',         g:slimv_leader.'j',  g:slimv_leader.'fd', ':call SlimvFindDefinitionsPrompt()<CR>' )

if exists( 'g:paredit_loaded' )
call s:MenuMap( 'Slim&v.Edi&t.&Paredit-Toggle',                 g:slimv_leader.'(',  g:slimv_leader.'(t',  ':<C-U>call PareditToggle()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.-PareditSep-',                    '',                  '',                   ':' )

if g:paredit_shortmaps
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Wrap<Tab>'                             .'W',  '',  '',              ':<C-U>call PareditWrap("(",")")<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Spli&ce<Tab>'                           .'S',  '',  '',              ':<C-U>call PareditSplice()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Split<Tab>'                            .'O',  '',  '',              ':<C-U>call PareditSplit()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Join<Tab>'                             .'J',  '',  '',              ':<C-U>call PareditJoin()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Ra&ise<Tab>'             .g:slimv_leader.'I',  '',  '',              ':<C-U>call PareditRaise()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Move&Left<Tab>'                         .'<',  '',  '',              ':<C-U>call PareditMoveLeft()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Move&Right<Tab>'                        .'>',  '',  '',              ':<C-U>call PareditMoveRight()<CR>' )
else
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Wrap<Tab>'              .g:slimv_leader.'W',  '',  '',              ':<C-U>call PareditWrap("(",")")<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Spli&ce<Tab>'            .g:slimv_leader.'S',  '',  '',              ':<C-U>call PareditSplice()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Split<Tab>'             .g:slimv_leader.'O',  '',  '',              ':<C-U>call PareditSplit()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-&Join<Tab>'              .g:slimv_leader.'J',  '',  '',              ':<C-U>call PareditJoin()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Ra&ise<Tab>'             .g:slimv_leader.'I',  '',  '',              ':<C-U>call PareditRaise()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Move&Left<Tab>'          .g:slimv_leader.'<',  '',  '',              ':<C-U>call PareditMoveLeft()<CR>' )
call s:MenuMap( 'Slim&v.Edi&t.Paredit-Move&Right<Tab>'         .g:slimv_leader.'>',  '',  '',              ':<C-U>call PareditMoveRight()<CR>' )
endif "g:paredit_shortmaps
endif "g:paredit_loaded

" Evaluation commands
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Defun',               g:slimv_leader.'d',  g:slimv_leader.'ed',  ':<C-U>call SlimvEvalDefun()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Eval-Current-&Exp',         g:slimv_leader.'e',  g:slimv_leader.'ee',  ':<C-U>call SlimvEvalExp()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Region',              g:slimv_leader.'r',  g:slimv_leader.'er',  ':call SlimvEvalRegion()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Eval-&Buffer',              g:slimv_leader.'b',  g:slimv_leader.'eb',  ':<C-U>call SlimvEvalBuffer()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.Interacti&ve-Eval\.\.\.',   g:slimv_leader.'v',  g:slimv_leader.'ei',  ':call SlimvInteractiveEval()<CR>' )
call s:MenuMap( 'Slim&v.&Evaluation.&Undefine-Function',        g:slimv_leader.'u',  g:slimv_leader.'eu',  ':call SlimvUndefineFunction()<CR>' )

" Debug commands
call s:MenuMap( 'Slim&v.De&bugging.Macroexpand-&1',             g:slimv_leader.'1',  g:slimv_leader.'m1',  ':<C-U>call SlimvMacroexpand()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Macroexpand-All',           g:slimv_leader.'m',  g:slimv_leader.'ma',  ':<C-U>call SlimvMacroexpandAll()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.Toggle-&Trace\.\.\.',        g:slimv_leader.'t',  g:slimv_leader.'dt',  ':call SlimvTrace()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.U&ntrace-All',               g:slimv_leader.'T',  g:slimv_leader.'du',  ':call SlimvUntrace()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.Set-&Breakpoint',            g:slimv_leader.'B',  g:slimv_leader.'db',  ':call SlimvBreak()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.Break-on-&Exception',        g:slimv_leader.'E',  g:slimv_leader.'de',  ':call SlimvBreakOnException()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.Disassemb&le\.\.\.',         g:slimv_leader.'l',  g:slimv_leader.'dd',  ':call SlimvDisassemble()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Inspect\.\.\.',             g:slimv_leader.'i',  g:slimv_leader.'di',  ':call SlimvInspect()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.-SldbSep-',                  '',                  '',                   ':' )
call s:MenuMap( 'Slim&v.De&bugging.&Abort',                     g:slimv_leader.'a',  g:slimv_leader.'da',  ':call SlimvDebugAbort()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Quit-to-Toplevel',          g:slimv_leader.'q',  g:slimv_leader.'dq',  ':call SlimvDebugQuit()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Continue',                  g:slimv_leader.'n',  g:slimv_leader.'dc',  ':call SlimvDebugContinue()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Restart-Frame',             g:slimv_leader.'N',  g:slimv_leader.'dr',  ':call SlimvDebugRestartFrame()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Step-Into',                 g:slimv_leader.'si',  g:slimv_leader.'si',  ':call SlimvDebugStepInto()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Step-Next',                 g:slimv_leader.'sn',  g:slimv_leader.'sn',  ':call SlimvDebugStepNext()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Step-Out',                  g:slimv_leader.'so',  g:slimv_leader.'so',  ':call SlimvDebugStepOut()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.-ThreadSep-',                '',                  '',                   ':' )
call s:MenuMap( 'Slim&v.De&bugging.List-T&hreads',              g:slimv_leader.'H',  g:slimv_leader.'dl',  ':call SlimvListThreads()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Kill-Thread\.\.\.',         g:slimv_leader.'K',  g:slimv_leader.'dk',  ':call SlimvKillThread()<CR>' )
call s:MenuMap( 'Slim&v.De&bugging.&Debug-Thread\.\.\.',        g:slimv_leader.'G',  g:slimv_leader.'dT',  ':call SlimvDebugThread()<CR>' )

" Compile commands
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Defun',           g:slimv_leader.'D',  g:slimv_leader.'cd',  ':<C-U>call SlimvCompileDefun()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Load-File',       g:slimv_leader.'L',  g:slimv_leader.'cl',  ':<C-U>call SlimvCompileLoadFile()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&File',            g:slimv_leader.'F',  g:slimv_leader.'cf',  ':<C-U>call SlimvCompileFile()<CR>' )
call s:MenuMap( 'Slim&v.&Compilation.Compile-&Region',          g:slimv_leader.'R',  g:slimv_leader.'cr',  ':call SlimvCompileRegion()<CR>' )

" Xref commands
call s:MenuMap( 'Slim&v.&Xref.Who-&Calls',                      g:slimv_leader.'xc', g:slimv_leader.'xc',  ':call SlimvXrefCalls()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&References',                 g:slimv_leader.'xr', g:slimv_leader.'xr',  ':call SlimvXrefReferences()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Sets',                       g:slimv_leader.'xs', g:slimv_leader.'xs',  ':call SlimvXrefSets()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Binds',                      g:slimv_leader.'xb', g:slimv_leader.'xb',  ':call SlimvXrefBinds()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-&Macroexpands',               g:slimv_leader.'xm', g:slimv_leader.'xm',  ':call SlimvXrefMacroexpands()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.Who-S&pecializes',                g:slimv_leader.'xp', g:slimv_leader.'xp',  ':call SlimvXrefSpecializes()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.&List-Callers',                   g:slimv_leader.'xl', g:slimv_leader.'xl',  ':call SlimvXrefCallers()<CR>' )
call s:MenuMap( 'Slim&v.&Xref.List-Call&ees',                   g:slimv_leader.'xe', g:slimv_leader.'xe',  ':call SlimvXrefCallees()<CR>' )

" Profile commands
call s:MenuMap( 'Slim&v.&Profiling.Toggle-&Profile\.\.\.',      g:slimv_leader.'p',  g:slimv_leader.'pp',  ':<C-U>call SlimvProfile()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-&By-Substring\.\.\.',g:slimv_leader.'P',  g:slimv_leader.'pb',  ':<C-U>call SlimvProfileSubstring()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.Unprofile-&All',             g:slimv_leader.'U',  g:slimv_leader.'pa',  ':<C-U>call SlimvUnprofileAll()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.&Show-Profiled',             g:slimv_leader.'?',  g:slimv_leader.'ps',  ':<C-U>call SlimvShowProfiled()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.-ProfilingSep-',             '',                  '',                   ':' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-Rep&ort',            g:slimv_leader.'o',  g:slimv_leader.'pr',  ':<C-U>call SlimvProfileReport()<CR>' )
call s:MenuMap( 'Slim&v.&Profiling.Profile-&Reset',             g:slimv_leader.'X',  g:slimv_leader.'px',  ':<C-U>call SlimvProfileReset()<CR>' )

" Documentation commands
call s:MenuMap( 'Slim&v.&Documentation.Describe-&Symbol',       g:slimv_leader.'s',  g:slimv_leader.'ds',  ':call SlimvDescribeSymbol()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.&Apropos',               g:slimv_leader.'A',  g:slimv_leader.'dp',  ':call SlimvApropos()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.&Hyperspec',             g:slimv_leader.'h',  g:slimv_leader.'dh',  ':call SlimvHyperspec()<CR>' )
call s:MenuMap( 'Slim&v.&Documentation.Generate-&Tags',         g:slimv_leader.']',  g:slimv_leader.'dg',  ':call SlimvGenerateTags()<CR>' )

" REPL commands
call s:MenuMap( 'Slim&v.&Repl.&Connect-Server',                 g:slimv_leader.'c',  g:slimv_leader.'rc',  ':call SlimvConnectServer()<CR>' )
call s:MenuMap( '',                                             g:slimv_leader.'g',  g:slimv_leader.'rp',  ':call SlimvSetPackage()<CR>' )
call s:MenuMap( 'Slim&v.&Repl.Interrup&t-Lisp-Process',         g:slimv_leader.'y',  g:slimv_leader.'ri',  ':call SlimvInterrupt()<CR>' )
call s:MenuMap( 'Slim&v.&Repl.Clear-&REPL',                     g:slimv_leader.'-',  g:slimv_leader.'-',   ':call SlimvClearReplBuffer()<CR>' )
call s:MenuMap( 'Slim&v.&Repl.&Quit-REPL',                      g:slimv_leader.'Q',  g:slimv_leader.'rq',  ':call SlimvQuitRepl()<CR>' )


" =====================================================================
"  Slimv menu
" =====================================================================

if g:slimv_menu == 1
    " Works only if 'wildcharm' is <Tab>
    if &wildcharm == 0
        set wildcharm=<Tab>
    endif
    if &wildcharm != 0
        execute ':map ' . g:slimv_leader.', :emenu Slimv.' . nr2char( &wildcharm )
    endif
endif

" Add REPL menu. This menu exist only for the REPL buffer.
function! SlimvAddReplMenu()
    if &wildcharm != 0
        execute ':map ' . g:slimv_leader.'\ :emenu REPL.' . nr2char( &wildcharm )
    endif

    amenu &REPL.Send-&Input                            :call SlimvSendCommand(0)<CR>
    amenu &REPL.Cl&ose-Send-Input                      :call SlimvSendCommand(1)<CR>
    amenu &REPL.Set-Packa&ge                           :call SlimvSetPackage()<CR>
    amenu &REPL.Interrup&t-Lisp-Process                <Esc>:<C-U>call SlimvInterrupt()<CR>
    amenu &REPL.-REPLSep-                              :
    amenu &REPL.&Previous-Input                        :call SlimvPreviousCommand()<CR>
    amenu &REPL.&Next-Input                            :call SlimvNextCommand()<CR>
    amenu &REPL.Clear-&REPL                            :call SlimvClearReplBuffer()<CR>
endfunction

" =====================================================================
"  Slimv commands
" =====================================================================

command! -complete=customlist,SlimvCommandComplete -nargs=* Lisp call SlimvEval([<q-args>])
command! -complete=customlist,SlimvCommandComplete -nargs=* Eval call SlimvEval([<q-args>])

" Switch on syntax highlighting
if !exists("g:syntax_on")
    syntax on
endif

