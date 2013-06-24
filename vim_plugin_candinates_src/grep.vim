" File: grep.vim
" Author: Yegappan Lakshmanan (yegappan AT yahoo DOT com)
" Version: 1.9
" Last Modified: September 10, 2007
" 
" Overview
" --------
" The grep plugin integrates the grep, fgrep, egrep, and agrep tools with
" Vim and allows you to search for a pattern in one or more files and jump
" to them.
"
" To use this plugin, you need the grep, fgrep, egrep, agrep, find and
" xargs utilities. These tools are present in most of the Unix installations.
" For MS-Windows systems, you can download the GNU grep and find utilities
" from the following sites:
"
"    http://gnuwin32.sourceforge.net/packages/grep.htm
"    http://gnuwin32.sourceforge.net/packages/findutils.htm
"
" Installation
" ------------
" 1. Copy the grep.vim file to the $HOME/.vim/plugin or $HOME/vimfiles/plugin
"    or $VIM/vimfiles/plugin directory. 
"    Refer to the following Vim help topics for more information about Vim
"    plugins:
"       :help add-plugin
"       :help add-global-plugin
"       :help runtimepath
" 2. If the grep executables are not already present in one of the directories
"    in the PATH environment variable, then set the Grep_Path and other _Path
"    variables to point to the location of the grep utilites in the .vimrc
"    file.
" 3. Restart Vim.
" 4. You can now use the ":Grep" and other commands to search for patterns in
"    files.
"
" Usage
" -----
" The grep.vim plugin introduces the following Vim commands:
"
" :Grep          - Search for the specified pattern in the specified files
" :GrepAdd       - Same as ":Grep" but adds the results to the current results
" :Rgrep         - Run recursive grep
" :RgrepAdd      - Same as ":Rgrep" but adds the results to the current results
" :GrepBuffer    - Search for a pattern on all open buffers
" :GrepBufferAdd - Same as ":GrepBuffer" but adds the results to the current
"                  results
" :Bgrep         - Same as :GrepBuffer
" :BgrepAdd      - Same as :GrepBufferAdd
" :GrepArgs      - Search for a pattern on all the Vim argument 
"                  filenames (:args)
" :GrepArgsAdd   - Same as ":GrepArgs" but adds the results to the current
"                  results
" :Fgrep         - Run fgrep
" :FgrepAdd      - Same as ":Fgrep" but adds the results to the current
"                  results
" :Rfgrep        - Run recursive fgrep
" :RfgrepAdd     - Same as ":Rfgrep" but adds the results to the current
"                  results
" :Egrep         - Run egrep
" :EgrepAdd      - Same as ":Egrep" but adds the results to the current
"                  results
" :Regrep        - Run recursive egrep
" :RegrepAdd     - Same as ":Regrep" but adds the results to the current
"                  results
" :Agrep         - Run agrep
" :AgrepAdd      - Same as ":Agrep" but adds the results to the current
"                  results
" :Ragrep        - Run recursive agrep
" :RagrepAdd     - Same as ":Ragrep" but adds the results to the current
"                  results
"
" The above commands can be invoked like this:
"
"    :Grep   [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Rgrep  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Fgrep  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Rfgrep [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Egrep  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Regrep [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Agrep  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :Ragrep [<grep_options>] [<search_pattern> [<file_name(s)>]]
"
"    :GrepAdd   [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :RgrepAdd  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :FgrepAdd  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :RfgrepAdd [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :EgrepAdd  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :RegrepAdd [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :AgrepAdd  [<grep_options>] [<search_pattern> [<file_name(s)>]]
"    :RagrepAdd [<grep_options>] [<search_pattern> [<file_name(s)>]]
"
"    :GrepBuffer [<grep_options>] [<search_pattern>]
"    :Bgrep [<grep_options>] [<search_pattern>]
"    :GrepArgs [<grep_options>] [<search_pattern>]
"
"    :GrepBufferAdd [<grep_options>] [<search_pattern>]
"    :BgrepAdd [<grep_options>] [<search_pattern>]
"    :GrepArgsAdd [<grep_options>] [<search_pattern>]
"
" In the above commands, all the arguments are optional.
"
" You can specify grep options like -i (ignore case) or -w (search for a word)
" to the above commands.  If the <grep_options> are not specified, then the
" default grep options specified by the variable Grep_Default_Options is 
" used.
"
" You can specify the grep pattern to search as an argument to the above
" commands.  If the <search_pattern> is not specified, then you will be
" prompted to enter a search pattern. By default, the keyword under the cursor
" is displayed for the search pattern prompt. You can accept the default or
" modify it.
"
" The search pattern is automatically enclosed by the character specified in
" the Grep_Shell_Quote_Char variable. You should not enclose the search
" pattern with a shell escape character.
"
" If you want to specify a search pattern with space characters or a
" multi-word pattern, then you should use the Grep command pattern input
" prompt to supply the pattern.
"
" You can specify one or more file names (or file patterns) to the above
" commands.  If the <file_names> are not specified, then you will be prompted
" to enter file names.  By default, the pattern specified by the
" Grep_Default_Filelist variable is used. To specify the file name(s) as an
" argument to the above commands, you have to specify the search pattern also.
"
" When you enter only the command name, you will be prompted to enter the
" search pattern and the files in which to search for the pattern. By default,
" the keyword under the cursor is displayed for the search pattern prompt.
" Depending on the command, you may prompted for additional parameters like
" the directories to search for the pattern.
"
" You can retrieve previously entered values for the Grep prompts using the up
" and down arrow keys. You can cancel the command by pressing the escape key.
" You can use CTRL-U to erase the default shown for the prompt and CTRL-W to
" erase the previous word in the prompt. For more information about editing
" the prompt, read ':help cmdline-editing' Vim help topic.
"
" After invoking any of the grep commands, you can cancel the command, when
" you are prompted for a search pattern or file names or a directory by
" pressing the <Esc> key. You cannot cancel (or kill) the
" grep/fgrep/egrep/agrep commands after the external command is invoked.
"
" The GrepAdd, RgrepAdd and other *Add commands append the search output to
" the current search output. This is useful if you want to see the search
" results for multiple patterns at the same time. These commands are available
" only in Vim version 7.0 and above.
"
" You can map a key to invoke any of the above commands. For example, the
" following map invokes the :Grep command to search for the keyword under the
" cursor:
"
"       nnoremap <silent> <F3> :Grep<CR>
"
" The output of the grep command will be listed in the Vim quickfix window.
" 1. You can select a line in the quickfix window and press <Enter> or double
"    click on a match to jump to that line.
" 2. You can use the ":cnext" and ":cprev" commands to the jump to the next or
"    previous output line.
" 3. You can use the ":colder" and ":cnewer" commands to go between multiple
"    grep quickfix output windows.
" 4. The quickfix window need not be opened always to use the grep output.
"    You can close the quickfix window and use the quickfix commands to jump
"    to the grep matches.  Use the ":copen" command to open the quickfix
"    window again.
"
" For more information about other quickfix commands read ":help quickfix"
" 
" When using GUI Vim, the Tools->Search menu item with a few sub-menu items is
" created for few variations of the search command.
"
" Configuration
" -------------
" By changing the following variables you can configure the behavior of this
" plugin. Set the following variables in your .vimrc file using the 'let'
" command.
"
" The 'Grep_Path' variable is used to locate the grep utility. By default,
" this is set to grep. You can change this using the let command:
"
"       :let Grep_Path = 'd:\tools\grep.exe'
"
" The 'Fgrep_Path' variable is used to locate the fgrep utility. By default,
" this is set to fgrep. You can change this using the let command:
"
"       :let Fgrep_Path = 'd:\tools\fgrep.exe'
"
" The 'Egrep_Path' variable is used to locate the egrep utility. By default,
" this is set to egrep. You can change this using the let command:
"
"       :let Egrep_Path = 'd:\tools\egrep.exe'
"
" The 'Agrep_Path' variable is used to locate the agrep utility. By default,
" this is set to agrep. You can change this using the let command:
"
"       :let Agrep_Path = 'd:\tools\agrep.exe'
"
" The 'Grep_Find_Path' variable is used to locate the find utility. By
" default, this is set to d:\tools\find.exe. You can change this using the let
" command:
"
"       :let Grep_Find_Path = 'd:\tools\find.exe'
"
" The 'Grep_Xargs_Path' variable is used to locate the xargs utility. By
" default, this is set to xargs. You can change this using the let
" command:
"
"       :let Grep_Xargs_Path = 'd:\tools\xargs.exe'
"
" When running any one of the Grep commands, you will be prompted for the
" files in which to search for the pattern. The 'Grep_Default_Filelist'
" variable is used to specify to default for this prompt. By default, this
" variable is set to '*'. You can specify multiple matching patterns separated
" by spaces. You can change this settings using the let command:
"
"       :let Grep_Default_Filelist = '*.[chS]'
"       :let Grep_Default_Filelist = '*.c *.cpp *.asm'
"
" The 'Grep_Default_Options' is used to pass default command line options to
" the grep/fgrep/egrep/agrep utilities. By default, this is set to an empty
" string. You can change this using the let command:
"
"       :let Grep_Default_Options = '-i'
"
" The 'Grep_Skip_Dirs' variable specifies the list of directories to skip
" while doing recursive searches. By default, this is set to 'RCS CVS SCCS'.
" You can change this using the let command:
"
"       :let Grep_Skip_Dirs = 'dir1 dir2 dir3'
"
" The 'Grep_Skip_Files' variable specifies the list of files to skip while
" doing recursive searches. By default, this is set to '*~ *,v s.*'. You can
" change this using the let command:
"
"       :let Grep_Skip_Files = '*.bak *~'
"
" By default, when you invoke the Grep commands the quickfix window will be
" opened with the grep output.  You can disable opening the quickfix window,
" by setting the 'Grep_OpenQuickfixWindow' variable  to zero:
"
"       :let Grep_OpenQuickfixWindow = 0
"
" You can manually open the quickfix window using the :cwindow command.
"
" By default, for recursive searches, the 'find' and 'xargs' utilities are
" used.  If you don't have the 'xargs' utility or don't want to use the
" 'xargs' utility, " then you can set the 'Grep_Find_Use_Xargs' variable to
" zero. If this is set to zero, then only the 'find' utility is used for
" recursive searches:
"
"       :let Grep_Find_Use_Xargs = 0
" 
" To handle file names with space characters in them, the xargs utility
" is invoked with the '--null' argument. If the xargs utility in your system
" doesn't accept the '--null' argument, then you can change the
" Grep_Xargs_Options variable. For example, to use the '--print0' xargs
" argument, you can use the following command:
"
" 	:let Grep_Xargs_Options = '--print0'
"
" The Grep_Cygwin_Find variable should be set to 1, if you are using the find
" utility from the cygwin package. This setting is used to handle the
" difference between the backslash and forward slash path separators.
"
"       :let Grep_Cygwin_Find = 1
" 
" The 'Grep_Null_Device' variable specifies the name of the null device to
" pass to the grep commands. This is needed to force the grep commands to
" print the name of the file in which a match is found, if only one filename
" is specified. For Unix systems, this is set to /dev/null and for MS-Windows
" systems, this is set to NUL. You can modify this by using the let command:
"
"       :let Grep_Null_Device = '/dev/null'
"
" The 'Grep_Shell_Quote_Char' variable specifies the quote character to use
" for protecting patterns from being interpreted by the shell. For Unix
" systems, this is set to "'" and for MS-Window systems, this is set to an
" empty string.  You can change this using the let command:
"
"       :let Grep_Shell_Quote_Char = "'"
"
" The 'Grep_Shell_Escape_Char' variable specifies the escape character to use
" for protecting special characters from being interpreted by the shell.  For
" Unix systems, this is set to '\' and for MS-Window systems, this is set to
" an empty string.  You can change this using the let command:
"
"       :let Grep_Shell_Escape_Char = "'"
"
" --------------------- Do not modify after this line ---------------------
if exists("loaded_grep")
    finish
endif
let loaded_grep = 1

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" Location of the grep utility
if !exists("Grep_Path")
    let Grep_Path = 'grep'
endif

" Location of the fgrep utility
if !exists("Fgrep_Path")
    let Fgrep_Path = 'fgrep'
endif

" Location of the egrep utility
if !exists("Egrep_Path")
    let Egrep_Path = 'egrep'
endif

" Location of the agrep utility
if !exists("Agrep_Path")
    let Agrep_Path = 'agrep'
endif

" Location of the find utility
if !exists("Grep_Find_Path")
    let Grep_Find_Path = 'find'
endif

" Location of the xargs utility
if !exists("Grep_Xargs_Path")
    let Grep_Xargs_Path = 'xargs'
endif

" Open the Grep output window.  Set this variable to zero, to not open
" the Grep output window by default.  You can open it manually by using
" the :cwindow command.
if !exists("Grep_OpenQuickfixWindow")
    let Grep_OpenQuickfixWindow = 1
endif

" Default grep file list
if !exists("Grep_Default_Filelist")
    let Grep_Default_Filelist = '*'
endif

" Default grep options
if !exists("Grep_Default_Options")
    let Grep_Default_Options = ''
endif

" Use the 'xargs' utility in combination with the 'find' utility. Set this
" to zero to not use the xargs utility.
if !exists("Grep_Find_Use_Xargs")
    let Grep_Find_Use_Xargs = 1
endif

" The command-line arguments to supply to the xargs utility
if !exists('Grep_Xargs_Options')
    let Grep_Xargs_Options = '--null'
endif

" The find utility is from the cygwin package or some other find utility.
if !exists("Grep_Cygwin_Find")
    let Grep_Cygwin_Find = 0
endif

" NULL device name to supply to grep.  We need this because, grep will not
" print the name of the file, if only one filename is supplied. We need the
" filename for Vim quickfix processing.
if !exists("Grep_Null_Device")
    if has("win32") || has("win16") || has("win95")
        let Grep_Null_Device = 'NUL'
    else
        let Grep_Null_Device = '/dev/null'
    endif
endif

" Character to use to quote patterns and filenames before passing to grep.
if !exists("Grep_Shell_Quote_Char")
    if has("win32") || has("win16") || has("win95")
        let Grep_Shell_Quote_Char = ''
    else
        let Grep_Shell_Quote_Char = "'"
    endif
endif

" Character to use to escape special characters before passing to grep.
if !exists("Grep_Shell_Escape_Char")
    if has("win32") || has("win16") || has("win95")
        let Grep_Shell_Escape_Char = ''
    else
        let Grep_Shell_Escape_Char = '\'
    endif
endif

" The list of directories to skip while searching for a pattern. Set this
" variable to '', if you don't want to skip directories.
if !exists("Grep_Skip_Dirs")
    let Grep_Skip_Dirs = 'RCS CVS SCCS'
endif

" The list of files to skip while searching for a pattern. Set this variable
" to '', if you don't want to skip any files.
if !exists("Grep_Skip_Files")
    let Grep_Skip_Files = '*~ *,v s.*'
endif

" RunGrepCmd()
" Run the specified grep command using the supplied pattern
function! s:RunGrepCmd(cmd, pattern, action)
    let cmd_output = system(a:cmd)

    if cmd_output == ""
        echohl WarningMsg | 
        \ echomsg "Error: Pattern " . a:pattern . " not found" | 
        \ echohl None
        return
    endif

    let tmpfile = tempname()

    let old_verbose = &verbose
    set verbose&vim

    exe "redir! > " . tmpfile
    silent echon '[Search results for pattern: ' . a:pattern . "]\n"
    silent echon cmd_output
    redir END

    let &verbose = old_verbose

    let old_efm = &efm
    set efm=%f:%\\s%#%l:%m

    if v:version >= 700 && a:action == 'add'
        execute "silent! caddfile " . tmpfile
    else
        if exists(":cgetfile")
            execute "silent! cgetfile " . tmpfile
        else
            execute "silent! cfile " . tmpfile
        endif
    endif

    let &efm = old_efm

    " Open the grep output window
    if g:Grep_OpenQuickfixWindow == 1
        " Open the quickfix window below the current window
        botright copen
    endif

    call delete(tmpfile)
endfunction

" RunGrepRecursive()
" Run specified grep command recursively
function! s:RunGrepRecursive(cmd_name, grep_cmd, action, ...)
    if a:0 > 0 && (a:1 == "-?" || a:1 == "-h")
        echo 'Usage: ' . a:cmd_name . " [<grep_options>] [<search_pattern> " .
                        \ "[<file_name(s)>]]"
        return
    endif

    let grep_opt    = ""
    let pattern     = ""
    let filepattern = ""

    let argcnt = 1
    while argcnt <= a:0
        if a:{argcnt} =~ '^-'
            let grep_opt = grep_opt . " " . a:{argcnt}
        elseif pattern == ""
            let pattern = g:Grep_Shell_Quote_Char . a:{argcnt} . 
                            \ g:Grep_Shell_Quote_Char
        else
            let filepattern = filepattern . " " . a:{argcnt}
        endif
        let argcnt= argcnt + 1
    endwhile
    if grep_opt == ""
        let grep_opt = g:Grep_Default_Options
    endif

    if a:grep_cmd != 'agrep'
        " Don't display messages about non-existent files
        " Agrep doesn't support the -s option
        let grep_opt = grep_opt . " -s"
    endif

    if a:grep_cmd == 'grep'
        let grep_path = g:Grep_Path
        let grep_expr_option = '--'
    elseif a:grep_cmd == 'fgrep'
        let grep_path = g:Fgrep_Path
        let grep_expr_option = '-e'
    elseif a:grep_cmd == 'egrep'
        let grep_path = g:Egrep_Path
        let grep_expr_option = '-e'
    elseif a:grep_cmd == 'agrep'
        let grep_path = g:Agrep_Path
        let grep_expr_option = ''
    else
        return
    endif

    " No argument supplied. Get the identifier and file list from user
    if pattern == "" 
        let pattern = input("Search for pattern: ", expand("<cword>"))
        if pattern == ""
            return
        endif
        let pattern = g:Grep_Shell_Quote_Char . pattern . 
                        \ g:Grep_Shell_Quote_Char
    endif

    let cwd = getcwd()
    if g:Grep_Cygwin_Find == 1
        let cwd = substitute(cwd, "\\", "/", "g")
    endif
    if v:version >= 700
        let startdir = input("Start searching from directory: ", cwd, "dir")
    else
        let startdir = input("Start searching from directory: ", cwd)
    endif
    if startdir == ""
        return
    endif

    if filepattern == ""
        let filepattern = input("Search in files matching pattern: ", 
                                          \ g:Grep_Default_Filelist)
        if filepattern == ""
            return
        endif
    endif

    let txt = filepattern . ' '
    let find_file_pattern = ''
    while txt != ''
        let one_pattern = strpart(txt, 0, stridx(txt, ' '))
        if find_file_pattern != ''
            let find_file_pattern = find_file_pattern . ' -o'
        endif
        let find_file_pattern = find_file_pattern . ' -name ' .
              \ g:Grep_Shell_Quote_Char . one_pattern . g:Grep_Shell_Quote_Char
        let txt = strpart(txt, stridx(txt, ' ') + 1)
    endwhile
    let find_file_pattern = g:Grep_Shell_Escape_Char . '(' .
                    \ find_file_pattern . ' ' . g:Grep_Shell_Escape_Char . ')'

    let txt = g:Grep_Skip_Dirs
    let find_prune = ''
    if txt != ''
        let txt = txt . ' '
        while txt != ''
            let one_dir = strpart(txt, 0, stridx(txt, ' '))
            if find_prune != ''
                let find_prune = find_prune . ' -o'
            endif
            let find_prune = find_prune . ' -name ' . one_dir
            let txt = strpart(txt, stridx(txt, ' ') + 1)
        endwhile
        let find_prune = '-type d ' . g:Grep_Shell_Escape_Char . '(' .
                         \ find_prune
        let find_prune = find_prune . ' ' . g:Grep_Shell_Escape_Char . ')'
    endif

    let txt = g:Grep_Skip_Files
    let find_skip_files = '-type f'
    if txt != ''
        let txt = txt . ' '
        while txt != ''
            let one_file = strpart(txt, 0, stridx(txt, ' '))
            let find_skip_files = find_skip_files . ' ! -name ' .
                                  \ g:Grep_Shell_Quote_Char . one_file .
                                  \ g:Grep_Shell_Quote_Char
            let txt = strpart(txt, stridx(txt, ' ') + 1)
        endwhile
    endif

    if g:Grep_Find_Use_Xargs == 1
        let cmd = g:Grep_Find_Path . " " . startdir
        let cmd = cmd . " " . find_prune . " -prune -o"
        let cmd = cmd . " " . find_skip_files
        let cmd = cmd . " " . find_file_pattern
        let cmd = cmd . " -print0 | "
        let cmd = cmd . g:Grep_Xargs_Path . ' ' . g:Grep_Xargs_Options
        let cmd = cmd . ' ' . grep_path . " " . grep_opt . " -n "
        let cmd = cmd . grep_expr_option . " " . pattern
        let cmd = cmd . ' ' . g:Grep_Null_Device 
    else
        let cmd = g:Grep_Find_Path . " " . startdir
        let cmd = cmd . " " . find_prune . " -prune -o"
        let cmd = cmd . " " . find_skip_files
        let cmd = cmd . " " . find_file_pattern
        let cmd = cmd . " -exec " . grep_path . " " . grep_opt . " -n "
        let cmd = cmd . grep_expr_option . " " . pattern
        let cmd = cmd . " {} " . g:Grep_Null_Device . ' ' .
                         \ g:Grep_Shell_Escape_Char . ';'
    endif

    call s:RunGrepCmd(cmd, pattern, a:action)
endfunction

" RunGrepSpecial()
" Search for a pattern in all the opened buffers or filenames in the
" argument list
function! s:RunGrepSpecial(cmd_name, which, action, ...)
    if a:0 > 0 && (a:1 == "-?" || a:1 == "-h")
        echo 'Usage: ' . a:cmd_name . " [<grep_options>] [<search_pattern>]"
        return
    endif

    " Search in all the Vim buffers
    if a:which == 'buffer'
        " Get a list of all the buffer names
        let last_bufno = bufnr("$")

        let i = 1
        let filenames = ""

        while i <= last_bufno
            if bufexists(i) && buflisted(i)
                let filenames = filenames . " " . bufname(i)
            endif
            let i = i + 1
        endwhile

        " No buffers
        if filenames == ""
            return
        endif
    elseif a:which == 'args'
        " Search in all the filenames in the argument list
        let arg_cnt = argc()

        if arg_cnt == 0
            echohl WarningMsg
            echomsg "Error: Argument list is empty"
            echohl None
            return
        endif

        let i = 0
        let filenames = ""

        while i < arg_cnt
            let filenames = filenames . " " . argv(i)
            let i = i + 1
        endwhile

        " No arguments
        if filenames == ""
            echohl WarningMsg
            echomsg "Error: Argument list is empty"
            echohl None
            return
        endif
    endif

    let grep_opt = ""
    let pattern = ""

    " Get the list of optional grep command-line options (if present)
    " supplied by the user. All the grep options will be preceded
    " by a '-'
    let argcnt= 1
    while argcnt <= a:0 && a:{argcnt} =~ '^-'
        let grep_opt = grep_opt . " " . a:{argcnt}
        let argcnt = argcnt + 1
    endwhile

    " If the user didn't specify the option, then use the defaults
    if grep_opt == ""
        let grep_opt = g:Grep_Default_Options
    endif

    " Don't display messages about non-existent files
    let grep_opt = grep_opt . " -s"

    " The last argument specified by the user is the pattern
    if argcnt == a:0
        let pattern = a:{argcnt}
    else
        " No argument supplied. Get the identifier and file list from user
        let pattern = input("Search for pattern: ", expand("<cword>"))
        if pattern == ""
            return
        endif
    endif

    let pattern = g:Grep_Shell_Quote_Char . pattern . g:Grep_Shell_Quote_Char

    " Add /dev/null to the list of filenames, so that grep print the
    " filename and linenumber when grepping in a single file
    let filenames = filenames . " " . g:Grep_Null_Device
    let cmd = g:Grep_Path . " " . grep_opt . " -n -- "
    let cmd = cmd . pattern . " " . filenames

    call s:RunGrepCmd(cmd, pattern, a:action)
endfunction

" RunGrep()
" Run the specified grep command
function! s:RunGrep(cmd_name, grep_cmd, action, ...)
    if a:0 > 0 && (a:1 == "-?" || a:1 == "-h")
        echo 'Usage: ' . a:cmd_name . " [<grep_options>] [<search_pattern> " .
                        \ "[<file_name(s)>]]"
        return
    endif

    let grep_opt  = ""
    let pattern   = ""
    let filenames = ""

    " Parse the arguments
    " grep command-line flags are specified using the "-flag" format
    " the next argument is assumed to be the pattern
    " and the next arguments are assumed to be filenames or file patterns
    let argcnt = 1
    while argcnt <= a:0
        if a:{argcnt} =~ '^-'
            let grep_opt = grep_opt . " " . a:{argcnt}
        elseif pattern == ""
            let pattern = g:Grep_Shell_Quote_Char . a:{argcnt} .
                            \ g:Grep_Shell_Quote_Char
        else
            let filenames= filenames . " " . a:{argcnt}
        endif
        let argcnt = argcnt + 1
    endwhile

    if grep_opt == ""
        let grep_opt = g:Grep_Default_Options
    endif

    if a:grep_cmd != 'agrep'
        " Don't display messages about non-existent files
        " Agrep doesn't support the -s option
        let grep_opt = grep_opt . " -s"
    endif

    if a:grep_cmd == 'grep'
        let grep_path = g:Grep_Path
        let grep_expr_option = '--'
    elseif a:grep_cmd == 'fgrep'
        let grep_path = g:Fgrep_Path
        let grep_expr_option = '-e'
    elseif a:grep_cmd == 'egrep'
        let grep_path = g:Egrep_Path
        let grep_expr_option = '-e'
    elseif a:grep_cmd == 'agrep'
        let grep_path = g:Agrep_Path
        let grep_expr_option = ''
    else
        return
    endif

    " Get the identifier and file list from user
    if pattern == "" 
        let pattern = input("Search for pattern: ", expand("<cword>"))
        if pattern == ""
            return
        endif
        let pattern = g:Grep_Shell_Quote_Char . pattern .
                        \ g:Grep_Shell_Quote_Char
    endif

    if filenames == ""
        if v:version >= 700
            let filenames = input("Search in files: ", g:Grep_Default_Filelist,
                        \ "file")
        else
            let filenames = input("Search in files: ", g:Grep_Default_Filelist)
        endif
        if filenames == ""
            return
        endif
    endif

    " Add /dev/null to the list of filenames, so that grep print the
    " filename and linenumber when grepping in a single file
    let filenames = filenames . " " . g:Grep_Null_Device
    let cmd = grep_path . " " . grep_opt . " -n "
    let cmd = cmd . grep_expr_option . " " . pattern
    let cmd = cmd . " " . filenames

    call s:RunGrepCmd(cmd, pattern, a:action)
endfunction

" Define the set of grep commands
command! -nargs=* -complete=file Grep
            \ call s:RunGrep('Grep', 'grep', 'set', <f-args>)
command! -nargs=* -complete=file Rgrep
            \ call s:RunGrepRecursive('Rgrep', 'grep', 'set', <f-args>)
command! -nargs=* GrepBuffer
            \ call s:RunGrepSpecial('GrepBuffer', 'buffer', 'set', <f-args>)
command! -nargs=* Bgrep
            \ call s:RunGrepSpecial('Bgrep', 'buffer', 'set', <f-args>)
command! -nargs=* GrepArgs
            \ call s:RunGrepSpecial('GrepArgs', 'args', 'set', <f-args>)

command! -nargs=* -complete=file Fgrep
            \ call s:RunGrep('Fgrep', 'fgrep', 'set', <f-args>)
command! -nargs=* -complete=file Rfgrep
            \ call s:RunGrepRecursive('Rfgrep', 'fgrep', 'set', <f-args>)
command! -nargs=* -complete=file Egrep
            \ call s:RunGrep('Egrep', 'egrep', 'set', <f-args>)
command! -nargs=* -complete=file Regrep
            \ call s:RunGrepRecursive('Regrep', 'egrep', 'set', <f-args>)
command! -nargs=* -complete=file Agrep
            \ call s:RunGrep('Agrep', 'agrep', 'set', <f-args>)
command! -nargs=* -complete=file Ragrep
            \ call s:RunGrepRecursive('Ragrep', 'agrep', 'set', <f-args>)

if v:version >= 700
command! -nargs=* -complete=file GrepAdd
            \ call s:RunGrep('GrepAdd', 'grep', 'add', <f-args>)
command! -nargs=* -complete=file RgrepAdd
            \ call s:RunGrepRecursive('RgrepAdd', 'grep', 'add', <f-args>)
command! -nargs=* GrepBufferAdd
            \ call s:RunGrepSpecial('GrepBufferAdd', 'buffer', 'add', <f-args>)
command! -nargs=* BgrepAdd
            \ call s:RunGrepSpecial('BgrepAdd', 'buffer', 'add', <f-args>)
command! -nargs=* GrepArgsAdd
            \ call s:RunGrepSpecial('GrepArgsAdd', 'args', 'add', <f-args>)

command! -nargs=* -complete=file FgrepAdd
            \ call s:RunGrep('FgrepAdd', 'fgrep', 'add', <f-args>)
command! -nargs=* -complete=file RfgrepAdd
            \ call s:RunGrepRecursive('RfgrepAdd', 'fgrep', 'add', <f-args>)
command! -nargs=* -complete=file EgrepAdd
            \ call s:RunGrep('EgrepAdd', 'egrep', 'add', <f-args>)
command! -nargs=* -complete=file RegrepAdd
            \ call s:RunGrepRecursive('RegrepAdd', 'egrep', 'add', <f-args>)
command! -nargs=* -complete=file AgrepAdd
            \ call s:RunGrep('AgrepAdd', 'agrep', 'add', <f-args>)
command! -nargs=* -complete=file RagrepAdd
            \ call s:RunGrepRecursive('RagrepAdd', 'agrep', 'add', <f-args>)
endif

" Add the Tools->Search Files menu
if has('gui_running')
    anoremenu <silent> Tools.Search.Current\ Directory<Tab>:Grep
                \ :Grep<CR>
    anoremenu <silent> Tools.Search.Recursively<Tab>:Rgrep
                \ :Rgrep<CR>
    anoremenu <silent> Tools.Search.Buffer\ List<Tab>:Bgrep
                \ :Bgrep<CR>
    anoremenu <silent> Tools.Search.Argument\ List<Tab>:GrepArgs
                \ :GrepArgs<CR>
endif

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save

