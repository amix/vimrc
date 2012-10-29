" genutils: Useful buffer, file and window related functions.
" Author: Hari Krishna Dara (hari_vim at yahoo dot com)
" Last Change: 15-Sep-2009 @ 19:25
" Requires: Vim-7.0
" Version: 2.5.1
" Licence: This program is free software; you can redistribute it and/or
"          modify it under the terms of the GNU General Public License.
"          See http://www.gnu.org/copyleft/gpl.txt 
" Acknowledgements:
"     - The genutils#GetNextWinnrInStack() function is based on the WinStackMv()
"       function posted by Charles E. Campbell, Jr. on vim mailing list on Jul
"       14, 2004.
"     - The genutils#CommonPath() function is based on the thread,
"       "computing relative path" on Jul 29, 2002.
"     - The genutils#ShowLinesWithSyntax() function is based on a posting by
"       Gary Holloway (gary at castandcrew dot com) on Jan, 16 2002.
"     - Robert Webb for the original "quick sort" algorithm from eval.txt.
"     - Peit Delport's (pjd at 303 dot za dot net) for his original BISort()
"       algorithm on which the genutils#BinInsertSort() and
"       genutils#BinInsertSort2() functions are based on.
" Download From:
"     http://www.vim.org/script.php?script_id=197
" See Also: autoload/genutils.vim
"
" Description:
"   - Read the "Documentation With Function Prototypes" section below.
"   - Misc. window/buffer related functions, genutils#NumberOfWindows(),
"     genutils#FindBufferForName(), genutils#MoveCursorToWindow(),
"     genutils#MoveCurLineToWinLine(), genutils#SetupScratchBuffer(),
"     genutils#MapAppendCascaded()
"   - Save/Restore all the window height/width settings to be restored later.
"   - Save/Restore position in the buffer to be restored later. Works like the
"     built-in marks feature, but has more to it.
"   - genutils#AddNotifyWindowClose() to get notifications *after* a window
"     with the specified buffer has been closed or the buffer is unloaded. The
"     built-in autocommands can only notify you *before* the window is closed.
"     You can use this with the Save/Restore window settings feature to
"     restore the dimensions of existing windows, after your window is closed
"     (just like how Vim does while closing help windows). See selectbuf.vim
"     or perforce.vim for examples.
"     There is also a test function called RunNotifyWindowCloseTest() that
"     demos the usage (you need to uncomment RunNotifyWindowCloseTest and
"     NotifyWindowCloseF functions).
"   - genutils#ShowLinesWithSyntax() function to echo lines with syntax coloring.
"   - genutils#ShiftWordInSpace(), genutils#CenterWordInSpace() and
"     genutils#AlignWordWithWordInPreviousLine() utility functions to move
"     words in the space without changing the width of the field. A
"     genutils#GetSpacer() function to return a spacer of specified width.
"   - Binary search function genutils#BinSearchList() for sorted lists, to
"     find the index after which a given item can be inserted to keep the list
"     in sorted order. You can also use these functions to just search for
"     boundaries.
"     There are also a couple of functions genutils#BinSearchForInsert() and
"     genutils#BinSearchForInsert2() to find the location for a newline to be
"     inserted in an already sorted buffer or arbitrary data.
"     There are also a few comparison functions that can be used with sort() or
"     the above functions.
"   - ExecMap function has now been separated as a plugin called execmap.vim.
"   - New genutils#CommonPath() function to extract the common part of two
"     paths, and genutils#RelPathFromFile() and genutils#RelPathFromDir() to
"     find relative paths (useful HTML href's). A side effect is the
"     genutils#CommonString() function to find the common string of two
"     strings.
"   - genutils#UnEscape() and genutils#DeEscape() functions to reverse and
"     genutils#Escape() to compliment what built-in escape() does. There is
"     also an genutils#EscapeCommand() function to escape external command
"     strings.
"   - Utility functions genutils#CurLineHasSign() and genutils#ClearAllSigns()
"     to fill in the gaps left by Vim.
"   - genutils#GetVimCmdOutput() function to capture the output of Vim built-in
"     commands, in a safe manner.
"   - genutils#OptClearBuffer() function to clear the contents and undo
"     history of the current buffer in an optimal manner. Ideal to be used
"     when plugins need to refresh their windows and don't care about
"     preserving the current contents (which is the most usual case).
"   - genutils#GetPreviewWinnr() function.
"   - Functions to have persistent data, genutils#PutPersistentVar() and
"     genutils#GetPersistentVar(). You don't need to worry about saving in
"     files and reading them back. To disable, set g:genutilsNoPersist in your
"     vimrc.
"   - A function to emulate the default Vim behavior for |timestamp| changes.
"     It also provides hooks to get call backs before and after handling the
"     default FileChangedShell autocommand (effectively splitting it into a
"     Pre and a Post event). Suggested usage is to use
"     genutils#AddToFCShellPre() and either install a default event handling
"     mechanism for all files by calling genutils#DefFCShellInstall() or
"     create your own autocommand on a matching pattern to call
"     genutils#DefFileChangedShell() function. Most useful for the source
"     control plugins to conditionally reload a file, while being able to
"     default to the Vim's standard behavior of asking the user. See
"     perforce.vim for usage examples.
"   - Utility function genutils#ExtractFuncListing() that is useful to to
"     create snippets (see breakpts.vim, ntservices.vim and ntprocesses.vim
"     for interesting ideas on how to use this function).
"
"   Function Prototypes:
"       The types in prototypes of the functions mimic Java.
"       This is just a full list for a quick reference, see
"         "Documentation With Function Prototypes" for more information on the
"         functions.
"
"   void    genutils#DebugShowArgs(...)
"   String  genutils#ExtractFuncListing(String funcName, String hLines, String tLines)
"   int     genutils#NumberOfWindows()
"   int     genutils#FindBufferForName(String fileName)
"   String  genutils#GetBufNameForAu(String bufName)
"   void    genutils#MoveCursorToWindow(int winno)
"   void    genutils#MoveCurLineToWinLine(int winLine)
"   void    genutils#CloseWindow(int winnr, boolean force)
"   void    genutils#MarkActiveWindow()
"   void    genutils#RestoreActiveWindow()
"   void    genutils#IsOnlyVerticalWindow()
"   void    genutils#IsOnlyHorizontalWindow()
"   int     genutils#GetNextWinnrInStack(char dir)
"   int     genutils#GetLastWinnrInStack(char dir)
"   void    genutils#MoveCursorToNextInWinStack(char dir)
"   void    genutils#MoveCursorToLastInWinStack(char dir)
"   void    genutils#OpenWinNoEa(String openWinCmd)
"   void    genutils#CloseWinNoEa(int winnr, boolean force)
"   void    genutils#SetupScratchBuffer()
"   void    genutils#CleanDiffOptions()
"   boolean genutils#ArrayVarExists(String varName, int index)
"   void    genutils#MapAppendCascaded(String lhs, String rhs, String mapMode)
"   void    genutils#SaveWindowSettings()
"   void    genutils#RestoreWindowSettings()
"   void    genutils#ResetWindowSettings()
"   void    genutils#SaveWindowSettings2(String id, boolean overwrite)
"   void    genutils#RestoreWindowSettings2(String id)
"   void    genutils#ResetWindowSettings2(String id)
"   void    genutils#SaveVisualSelection(String id)
"   void    genutils#RestoreVisualSelection(String id)
"   void    genutils#SaveSoftPosition(String id)
"   void    genutils#RestoreSoftPosition(String id)
"   void    genutils#ResetSoftPosition(String id)
"   void    genutils#SaveHardPosition(String id)
"   void    genutils#RestoreHardPosition(String id)
"   void    genutils#ResetHardPosition(String id)
"   int     genutils#GetLinePosition(String id)
"   int     genutils#GetColPosition(String id)
"   boolean genutils#IsPositionSet(String id)
"   String  genutils#CleanupFileName(String fileName)
"   String  genutils#CleanupFileName2(String fileName, String win32ProtectedChars)
"   boolean genutils#OnMS()
"   boolean genutils#PathIsAbsolute(String path)
"   boolean genutils#PathIsFileNameOnly(String path)
"   void    genutils#AddNotifyWindowClose(String windowTitle, String functionName)
"   void    genutils#RemoveNotifyWindowClose(String windowTitle)
"   void    genutils#CheckWindowClose()
"   void    genutils#ShowLinesWithSyntax() range
"   void    genutils#ShiftWordInSpace(int direction)
"   void    genutils#CenterWordInSpace()
"   int     genutils#BinSearchList(List list, int start, int end, Object item,
"                              [Funcref|String] cmp, int direction)
"   int     genutils#BinSearchForInsert(int start, int end, String line,
"                              String cmp, int direction)
"   int     genutils#BinSearchForInsert2(int start, int end, line, String cmp,
"                               int direction, String accessor, String context)
"   String  genutils#CommonPath(String path1, String path2)
"   String  genutils#CommonString(String str1, String str2)
"   String  genutils#RelPathFromFile(String srcFile, String tgtFile)
"   String  genutils#RelPathFromDir(String srcDir, String tgtFile)
"   String  genutils#Roman2Decimal(String str)
"   String  genutils#Escape(String str, String chars)
"   String  genutils#UnEscape(String str, String chars)
"   String  genutils#DeEscape(String str)
"   String  genutils#CrUnProtectedCharsPattern(String chars)
"   String  genutils#EscapeCommand(String cmd, List/String args, List/String pipe)
"   int     genutils#GetShellEnvType()
"   String  genutils#ExpandStr(String str)
"   String  genutils#QuoteStr(String str)
"   boolean genutils#CurLineHasSign()
"   void    genutils#ClearAllSigns()
"   String  genutils#UserFileComplete(String ArgLead, String CmdLine,
"                  String CursorPos, String smartSlash, String searchPath)
"   String  genutils#UserFileComplete2(String ArgLead, String CmdLine,
"                  String CursorPos, [Map params])
"   String  genutils#UserDirComplete2(String ArgLead, String CmdLine,
"                  String CursorPos, [Map params])
"   String  genutils#UserFileExpand(String fileArgs)
"   String  genutils#GetVimCmdOutput(String cmd)
"   void    genutils#OptClearBuffer()
"   int     genutils#GetPreviewWinnr()
"   void    genutils#PutPersistentVar(String pluginName, String persistentVar,
"                  String value)
"   void    genutils#GetPersistentVar(String pluginName, String persistentVar,
"                  String default)
"   void    genutils#AddToFCShellPre(String funcName)
"   void    genutils#RemoveFromFCShellPre(String funcName)
"   void    genutils#DefFCShellInstall()
"   void    genutils#DefFCShellUninstall()
"   boolean genutils#DefFileChangedShell()
"   void    genutils#SilentSubstitute(String pat, String cmd)
"   void    genutils#SilentDelete(String pat)
"   void    genutils#SilentDelete(String range, String pat)
"   String  genutils#GetSpacer(int width)
"   String  genutils#PromptForElement(List array,
"		   [String defaultValue | int defaultIndex], String msg,
"		   String skip, boolean useDialog, int nCols)
"   int     genutils#GetSelectedIndex()
"
" Documentation With Function Prototypes:
" -----------------------
" Useful function to debug passing arguments to functions. See exactly what
"   you would receive on the other side.
" Ex: :exec 'call genutils#DebugShowArgs('. genutils#CreateArgString("a 'b' c", ' ') . ')' 
"
" void    genutils#DebugShowArgs(...)
" -----------------------
" This function returns the body of the specified function ( the name should be
"   complete, including any scriptid prefix in case of a script local
"   function), without the function header and tail. You can also pass in the
"   number of additional lines to be removed from the head and or tail of the
"   function.
"
" String  genutils#ExtractFuncListing(String funcName, String hLines, String tLines)
" -----------------------
" -----------------------
" Return the number of windows open currently.
"
" int     genutils#NumberOfWindows()
" -----------------------
" Returns the buffer number of the given fileName if it is already loaded.
" The fileName argument is treated literally, unlike the bufnr() which treats
"   the argument as a filename-pattern. The function first escape all the
"   |filename-pattern| characters before passing it to bufnr(). It should work
"   in most of the cases, except when backslashes are used in non-windows
"   platforms, when the result could be unpredictable.
"
" Note: The function removes protections for "#%" characters because, these
"   are special characters on Vim commandline, and so are usually escaped
"   themselves, but bufnr() wouldn't like them.
"
" int     genutils#FindBufferForName(String fileName)
" -----------------------
" Returns the transformed buffer name that is suitable to be used in
"   autocommands.
"
" String  genutils#GetBufNameForAu(String bufName)
" -----------------------
" Given the window number, moves the cursor to that window.
"
" void    genutils#MoveCursorToWindow(int winno)
" -----------------------
" Moves the current line such that it is going to be the nth line in the window
"   without changing the column position.
"
" void    genutils#MoveCurLineToWinLine(int winLine)
" -----------------------
" Closes the given window and returns to the original window. It the simplest,
" this is equivalent to:
"
"   let curWin = winnr()
"   exec winnr 'wincmd w'
"   close
"   exec curWin 'wincmd w'
"
" But the function keeps track of the change in window numbers and restores
" the current window correctly. It also restores the previous window (the
" window that the cursor would jump to when executing "wincmd p" command).
" This is something that all plugins should do while moving around in the
" windows, behind the scenes.
"
" Pass 1 to force closing the window (:close!).
"
" void    genutils#CloseWindow(int winnr, boolean force)
" -----------------------
" Remembers the number of the current window as well as the previous-window
" (the one the cursor would jump to when executing "wincmd p" command). To
" determine the window number of the previous-window, the function temporarily
" jumps to the previous-window, so if your script intends to avoid generating
" unnecessary window events, consider disabling window events before calling
" this function (see :h 'eventignore').
"
" void    genutils#MarkActiveWindow()
" -----------------------
" Restore the cursor to the window that was previously marked as "active", as
" well as its previous-window (the one the cursor would jump to when executing
" "wincmd p" command). To restore the window number of the previous-window,
" the function temporarily jumps to the previous-window, so if your script
" intends to avoid generating unnecessary window events, consider disabling
" window events before calling this function (see :h 'eventignore').
"
" void    genutils#RestoreActiveWindow()
" -----------------------
" Returns 1 if the current window is the only window vertically.
"
" void    genutils#IsOnlyVerticalWindow()
" -----------------------
" Returns 1 if the current window is the only window horizontally.
"
" void    genutils#IsOnlyHorizontalWindow()
" -----------------------
" Returns the window number of the next window while remaining in the same
"   horizontal or vertical window stack (or 0 when there are no more). Pass
"   hjkl characters to indicate direction.
"   Usage:
"     let wn = genutils#GetNextWinnrInStack('h') left  window number in stack.
"     let wn = genutils#GetNextWinnrInStack('l') right window number in stack.
"     let wn = genutils#GetNextWinnrInStack('j') upper window number in stack.
"     let wn = genutils#GetNextWinnrInStack('k') lower window number in stack.
"
" int     genutils#GetNextWinnrInStack(char dir)
" -----------------------
" Returns the window number of the last window while remaining in the same
"   horizontal or vertical window stack (or 0 when there are no more, or it is
"   already the last window). Pass hjkl characters to indicate direction.
"   Usage:
"     let wn = genutils#GetLastWinnrInStack('h') leftmost  window number in stack.
"     let wn = genutils#GetLastWinnrInStack('l') rightmost window number in stack.
"     let wn = genutils#GetLastWinnrInStack('j') top       window number in stack.
"     let wn = genutils#GetLastWinnrInStack('k') bottom    window number in stack.
"
" int     genutils#GetLastWinnrInStack(char dir)
" -----------------------
" Move cursor to the next window in stack. See genutils#GetNextWinnrInStack()
"   for more information.
"
" void    genutils#MoveCursorToNextInWinStack(char dir)
" -----------------------
" Move cursor to the last window in stack. See genutils#GetLastWinnrInStack()
"   for more information.
"
" void    genutils#MoveCursorToLastInWinStack(char dir)
" -----------------------
" This function, which stands for "execute the given command that creates a
"   window, while disabling the 'equalalways' setting", is a means for plugins
"   to create new windows without disturbing the existing window dimensions as
"   much as possible. This function would not be required if 'equalalways' is
"   not set by the user. Even if set, the below code, though intuitive,
"   wouldn't work:
"       let _equalalways = &equalalways
"       set noequalalways
"       " open window now.
"       let &equalalways = _equalalways
"
" The problem is that while restoring the value of equalalways, if the user
"   originally had it set, Vim would immediately try to equalize all the
"   window dimensions, which is exactly what we tried to avoid by setting
"   'noequalalways'. The function works around the problem by temporarily
"   setting 'winfixheight' in all the existing windows and restoring them
"   after done.
"   Usage:
"     call genutils#OpenWinNoEa('sb ' pluginBuf)
"
" Note: The function doesn't catch any exceptions that are generated by the
"   operations, so it is advisable to catch them by the caller itself.
"
" void    genutils#OpenWinNoEa(String openWinCmd)
" -----------------------
" This is for the same purpose as described for genutils#OpenWinNoEa()
"   function, except that it is used to close a given window. This is just a
"   convenience function.
"
" void    genutils#CloseWinNoEa(int winnr, boolean force)
" -----------------------
" Turn on some buffer settings that make it suitable to be a scratch buffer.
"
" void    genutils#SetupScratchBuffer()
" -----------------------
" Turns off those options that are set by diff to the current window.
"   Also removes the 'hor' option from scrollopt (which is a global option).
" Better alternative would be to close the window and reopen the buffer in a
"   new window. 
"
" void    genutils#CleanDiffOptions()
" -----------------------
" This function is an alternative to exists() function, for those odd array
"   index names for which the built-in function fails. The var should be
"   accessible to this functions, so it shouldn't be a local or script local
"   variable.
"     if genutils#ArrayVarExists("array", id)
"       let val = array{id}
"     endif
"
" boolean genutils#ArrayVarExists(String varName, int index)
" -----------------------
" If lhs is already mapped, this function makes sure rhs is appended to it
"   instead of overwriting it. If you are rhs has any script local functions,
"   make sure you use the <SNR>\d\+_ prefix instead of the <SID> prefix (or the
"   <SID> will be replaced by the SNR number of genutils script, instead of
"   yours).
" mapMode is used to prefix to "oremap" and used as the map command. E.g., if
"   mapMode is 'n', then the function call results in the execution of noremap
"   command.
"
" void    genutils#MapAppendCascaded(String lhs, String rhs, String mapMode)
" -----------------------
" -----------------------
" Saves the heights and widths of the currently open windows for restoring
"   later.
"
" void    genutils#SaveWindowSettings()
" -----------------------
" Restores the heights of the windows from the information that is saved by
"  genutils#SaveWindowSettings(). Works only when the number of windows
"  haven't changed since the genutils#SaveWindowSettings is called.
"
" void    genutils#RestoreWindowSettings()
" -----------------------
" Reset the previously saved window settings using genutils#SaveWindowSettings.
"
" void    genutils#ResetWindowSettings()
" -----------------------
" Same as genutils#SaveWindowSettings, but uses the passed in id to create a
"   private copy for the calling script. Pass in a unique id to avoid
"   conflicting with other callers. If overwrite is zero and if the settings
"   are already stored for the passed in id, it will overwrite previously
"   saved settings.
"
" void    genutils#SaveWindowSettings2(String id, boolean overwrite)
" -----------------------
" Same as genutils#RestoreWindowSettings, but uses the passed in id to get the
"   settings. The settings must have been previously saved using this
"   id. Call genutils#ResetWindowSettings2() to explicitly reset the saved
"   settings.
"
" void    genutils#RestoreWindowSettings2(String id)
" -----------------------
" Reset the previously saved window settings using genutils#SaveWindowSettings2.
"   Releases the variables.
"
" void    genutils#ResetWindowSettings2(String id)
" -----------------------
" -----------------------
" Save the current/last visual selection such that it can be later restored
"   using genutils#RestoreVisualSelection(). Pass a unique id such that it will
"   not interfere with the other callers to this function. Saved selections
"   are not associated with the window so you can later restore the selection
"   in any window, provided there are enough lines/columns.
"
" void    genutils#SaveVisualSelection(String id)
" -----------------------
" Restore the visual selection that was previuosly saved using
"   genutils#SaveVisualSelection().
"
" void    genutils#RestoreVisualSelection(String id)
" -----------------------
" -----------------------
" This method tries to save the hard position along with the line context This
"   is like the vim builtin marker. Pass in a unique id to avoid
"   conflicting with other callers.
"
" void    genutils#SaveSoftPosition(String id)
" -----------------------
" Restore the cursor position using the information saved by the previous call
"   to genutils#SaveSoftPosition. This first calls
"   genutils#RestoreHardPosition() and then searches for the original line
"   first in the forward direction and then in the backward and positions the
"   cursor on the line if found. If the original line is not found it still
"   behaves like a call to genutils#RestoreHardPosition. This is similar to
"   the functionality of the built-in marker, as Vim is capable of maintaining
"   the marker even when the line is moved up or down. However, if there are
"   identical lines in the buffer and the original line has moved, this
"   function might get confused.
"
" void    genutils#RestoreSoftPosition(String id)
" -----------------------
" Reset the previously cursor position using genutils#SaveSoftPosition.
"   Releases the variables.
"
" void    genutils#ResetSoftPosition(String id)
" -----------------------
" Useful when you want to go to the exact (line, col), but marking will not
"   work, or if you simply don't want to disturb the marks. Pass in a unique
"   id.
"
" void    genutils#SaveHardPosition(String id)
" -----------------------
" Restore the cursor position using the information saved by the previous call
"   to genutils#SaveHardPosition. 
"
" void    genutils#RestoreHardPosition(String id)
" -----------------------
" Reset the previously cursor position using genutils#SaveHardPosition.
"   Releases the variables.
"
" void    genutils#ResetHardPosition(String id)
" -----------------------
" Return the line number of the previously saved position for the id.
"   This is like calling line() builtin function for a mark.
"
" int     genutils#GetLinePosition(String id)
" -----------------------
" Return the column number of the previously saved position for the id.
"   This is like calling col() builtin function for a mark.
"
" int     genutils#GetColPosition(String id)
" -----------------------
" A convenience function to check if a position has been saved (and not reset)
"   using the id given.
"
" boolean genutils#IsPositionSet(String id)
" -----------------------
" -----------------------
" Cleanup file name such that two *cleaned up* file names are easy to be
"   compared. This probably works only on windows and unix platforms. Also
"   recognizes UNC paths. Always returns paths with forward slashes only,
"   irrespective of what your 'shellslash' setting is. The return path will
"   always be a valid path for use in Vim, provided the original path itself
"   was valid for the platform (a valid cygwin path after the cleanup will
"   still be valid in a cygwin vim). The CleanupFileName2() variant is meant
"   for win32, to avoid translating some backslash protections to be treated
"   as regular path separators. Pass the characters that are protected, and
"   the backslashes infront of them are preserved.
" This function also expands the path, so any environment variables and others
"   (such as the "~" for paths relative to home) get expanded (see help on
"   expand()).
"
" String  genutils#CleanupFileName(String fileName)
" String  genutils#CleanupFileName2(String fileName, String win32ProtectedChars)
" -----------------------
" Returns true if the current OS is any of the Microsoft OSes. Most useful to
"   know if the path separator is "\".
"
" boolean genutils#OnMS()
" -----------------------
" Returns true if the given path could be an absolute path. Probably works
"   only on Unix and Windows platforms.
"
" boolean genutils#PathIsAbsolute(String path)
" -----------------------
" Returns true if the given path doesn't have any directory components.
"   Probably works only on Unix and Windows platforms.
"
" boolean genutils#PathIsFileNameOnly(String path)
" -----------------------
" -----------------------
" Add a notification to know when a buffer with the given name (referred to as
"   windowTitle) is no longer visible in any window. This by functionality is
"   like a BufWinLeavePost event. The function functionName is called back
"   with the title (buffer name) as an argument. The notification gets removed
"   after excuting it, so for future notifications, you need to reregister
"   your function. You can only have one notification for any buffer. The
"   function should be accessible from the script's local context.
"
" void    genutils#AddNotifyWindowClose(String windowTitle, String functionName)
" -----------------------
" Remove the notification previously added using genutils#AddNotifyWindowClose
"   function.
"
" void    genutils#RemoveNotifyWindowClose(String windowTitle)
" -----------------------
" Normally the plugin checks for closed windows for every WinEnter event, but
"   you can force a check at anytime by calling this function.
"
" void    genutils#CheckWindowClose()
" -----------------------
" -----------------------
" Displays the given line(s) from the current file in the command area (i.e.,
"   echo), using that line's syntax highlighting (i.e., WYSIWYG).  If no line
"   number is given, display the current line.
" Originally,
"   From: Gary Holloway "gary at castandcrew dot com"
"   Date: Wed, 16 Jan 2002 14:31:56 -0800
"
" void    genutils#ShowLinesWithSyntax() range
" -----------------------
" This function shifts the current word in the space without changing the
"   column position of the next word. Doesn't work for tabs.
"
" void    genutils#ShiftWordInSpace(int direction)
" -----------------------
" This function centers the current word in the space without changing the
"   column position of the next word. Doesn't work for tabs.
" 
" void    genutils#CenterWordInSpace()
" -----------------------
" -----------------------
" Find common path component of two filenames.
"   Based on the thread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" The last two arguments are optional and default to 0 (false), but you can
"   pass a value of 1 (true) to indicate that the path represents a directory.
" Ex:
"   genutils#CommonPath('/a/b/c/d.e', '/a/b/f/g/h.i') => '/a/b/'
"   genutils#CommonPath('/a/b/c/d.e', '/a/b/') => '/a/b'
"   genutils#CommonPath('/a/b/c/d.e', '/a/b/', 0, 1) => '/a/b/'
"
" String  genutils#CommonPath(String path1, String path2 [, boolean path1IsDir, boolean path2IsDir])
" -----------------------
" Find common string component of two strings.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   genutils#CommonString('abcde', 'abfghi') => 'ab'
"
" String  genutils#CommonString(String str1, String str2)
" -----------------------
" Find the relative path of tgtFile from the directory of srcFile.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   genutils#RelPathFromFile('/a/b/c/d.html', '/a/b/e/f.html') => '../f/g.html'
"
" String  genutils#RelPathFromFile(String srcFile, String tgtFile)
" -----------------------
" Find the relative path of tgtFile from the srcDir.
"   Based on the tread, "computing relative path".
"   Date: Mon, 29 Jul 2002 21:30:56 +0200 (CEST)
" Ex:
"   genutils#RelPathFromDir('/a/b/c/d', '/a/b/e/f/g.html') => '../../e/f/g.html'
"
" String  genutils#RelPathFromDir(String srcDir, String tgtFile)
" -----------------------
" -----------------------
" Convert Roman numerals to decimal. Doesn't detect format errors.
" Originally,
"   From: "Preben Peppe Guldberg" <c928400@student.dtu.dk>
"   Date: Fri, 10 May 2002 14:28:19 +0200
"
" String  genutils#Roman2Decimal(String str)
" -----------------------
" -----------------------
" Works like the built-in escape(), except that it escapes the specified
"   characters only if they are not already escaped, so something like
"   genutils#Escape('a\bc\\bd', 'b') would give 'a\bc\\\bd'. The chars value
"   directly goes into the [] collection, so it can be anything that is
"   accepted in [].
"
" String  genutils#Escape(String str, String chars)
" -----------------------
" Works like the reverse of the builtin escape() function, but un-escapes the
"   specified characters only if they are already escaped (essentially the
"   opposite of genutils#Escape()). The chars value directly goes into the []
"   collection, so it can be anything that is acceptable to [].
"
" String  genutils#UnEscape(String str, String chars)
" -----------------------
" Works like the reverse of the built-in escape() function. De-escapes all the
"   escaped characters. Essentially removes one level of escaping from the
"   string, so something like: 'a\b\\\\c\\d' would become 'ab\\c\d'.
"
" String  genutils#DeEscape(String str)
" ----------------------- 
" This function creates a pattern that avoids the given protected characters'
"   from getting treated as separators, when used with split(). The argument
"   goes directly into the [] atom, so make sure you pass in a valid string.
"   When optional argument capture is true, the characters are placed in a
"   capturing group.
" Ex:
"   let paths = split(&path, genutils#CrUnProtectedCharsPattern(','))
"
" String  genutils#CrUnProtectedCharsPattern(String chars, [boolean capture = false])
" ----------------------- 
" genutils#Escape the passed in shell command with quotes and backslashes such
"   a way that the arguments reach the command literally (avoids shell
"   interpretations). See the function header for the kind of escapings that
"   are done. The first argument is the actual command name, the second
"   argument is the arguments to the command and third argument is any pipe
"   command that should be appended to the command. The reason the function
"   requires them to be passed separately is that the escaping is minimized
"   for the first and third arguments. It is preferable to pass args as a Vim7
"   List, but it can be passed as a single string with spaces separating the
"   arguments (spaces in side each argument then needs to be protected)
"   Usage:
"     let fullCmd = genutils#EscapeCommand('ls', ['-u', expand('%:h')], ['|', 'grep', 'xxx'])
"   Note:
"     If the escaped command is used on Vim command-line (such as with ":w !",
"     ":r !" and ":!"), you need to further protect '%', '#' and '!' chars,
"     even if they are in quotes, to avoid getting expanded by Vim before
"     invoking external cmd. However this is not required for using it with
"     system() function. The easiest way to escape them is by using the
"     genutils#Escape() function as in "Escape(fullCmd, '%#!')".
" String  genutils#EscapeCommand(String cmd, List/String args, List/String pipe)
" -----------------------
" Returns the global ST_* constants (g:ST_WIN_CMD, g:ST_WIN_SH, g:ST_UNIX)
" based on the values of shell related settings and the OS on which Vim is
" running.
"
" int     genutils#GetShellEnvType()
" -----------------------
"
" Expands the string for the special characters. The return value should
"   essentially be what you would see if it was a string constant with
"   double-quotes.
" Ex:
"   genutils#ExpandStr('a\tA') => 'a     A'
" String  genutils#ExpandStr(String str)
" -----------------------
" Quotes the passed in string such that it can be used as a string expression
" in :execute. It sorrounds the passed in string with single-quotes while
" escaping any existing single-quotes in the string.
"
" String  genutils#QuoteStr(String str)
" -----------------------
" -----------------------
" Returns true if the current line has a sign placed.
"
" boolean genutils#CurLineHasSign()
" -----------------------
" Clears all signs in the current buffer.
"
" void    genutils#ClearAllSigns()
" -----------------------
" -----------------------
" This function is suitable to be used by custom command completion functions
"   for expanding filenames conditionally. The user function could, based on
"   the context, decide whether to do a file completion or a different custom
"   completion. See breakpts and perforce plugins for examples.
" It is also designed such that it can be used directly as a completion
"   function, though it would mean you can't pass any parameters.
" Opts parameter is a dictionary of optional parameters. Each parameter has a
"   predefined default, but can be overridden by specifying a value in the
"   dictionary and change the behavior. Here are the list of params supported:
"     - smartSlash: (default: 1)
"     -- 1: indicates that the path separator must be chosen based on the
"           ArgLead as well as user settings ('shellslash').
"     -- 0: always uses forwardslash as the path separator.
"     - searchPath: (default: '.')
"     -- A comma-separated list of directories, similar to how globpath() works.
"     - relativePaths: (default: 0)
"     -- 1: return relative paths. Most useful when searchPath has only one
"           path, but also useful with multiple paths, when using with
"           'runtimepath' (see :Runtime command in breakpts plugin).
"     -- 0: return absolute paths.
"     - completionTypes: (default: ['file', 'dir']
"     -- 'file': Include plain file types.
"     -- 'dir': Include directory types.
"     - anchorAtStart: (default: 1)
"     -- 1: indicates that the ArgLead should be anchored to the start.
"     -- 0: indicates that the ArgLead can occur anywhere in the filename.
"     - resultsAsList: (default: 1)
"     -- 1: results will be returned as a list
"     -- 0: results will be returned as a string with one filename per line.
"     - includeOriginal: (default: 1)
"     -- 1: ArgLead will be included at the end of results.
"     -- 0: ArgLead will not be included.
"     - dedupe: (default: 0), useful when relativePaths is 1 or paths repeat
"       in searchPath.
"     -- 1: expend extra effort to remove duplicates
"     -- 0: all results are includes, including any duplicates.
"
" String  genutils#UserFileComplete2(String ArgLead, String CmdLine, String
"                          CursorPos, [Dictionary params])
" -----------------------
" Same as genutils#UserFileComplete2, with the following two parameters (for
"   backwards compatibility).
"     resultsAsList: 0
"     relativePaths: 1
"
" String  genutils#UserFileComplete(String ArgLead, String CmdLine, String
"                          CursorPos, String smartSlash, String searchPath)
" -----------------------
" Same as genutils#UserFileComplete2, except that the "completionTypes" is
"   always set to ["dir"].
"
" String  genutils#UserDirComplete2(String ArgLead, String CmdLine, String
"                          CursorPos, [Dictionary params])
" -----------------------
" This is a convenience function to expand filename meta-sequences in the
"   given arguments just as Vim would have if given to a user-defined command
"   as arguments with completion mode set to "file". Useful
"   if you set the completion mode of your command to anything
"   other than the "file", and later conditionally expand arguments (for
"   characters such as % and # and other sequences such as #10 and <cword>)
"   after deciding which arguments represent filenames/patterns.
"
" String  genutils#UserFileExpand(String fileArgs)
" -----------------------
" This returns the output of the vim command as a string, without corrupting
"   any registers. Returns empty string on errors. Check for v:errmsg after
"   calling this function for any error messages.
"
" String  genutils#GetVimCmdOutput(String cmd)
" -----------------------
" Clear the contents of the current buffer in an optimum manner. For plugins
" that keep redrawing the contents of its buffer, executing "1,$d" or its
" equivalents result in overloading Vim's undo mechanism. Using this function
" avoids that problem.
"
" void    genutils#OptClearBuffer()
" -----------------------
" Returns the window number of the preview window if open or -1 if not.
" int     genutils#GetPreviewWinnr()
" -----------------------
" -----------------------
" These functions provide a persistent storage mechanism.
"
"     Example: Put the following in a file called t.vim in your plugin
"     directory and watch the magic. You can set new value using SetVar() and
"     see that it returns the same value across session when GetVar() is
"     called.
"     >>>>t.vim<<<<
"       au VimEnter * call LoadSettings()
"       au VimLeavePre * call SaveSettings()
"       
"       function! LoadSettings()
"         let s:tVar = genutils#GetPersistentVar("T", "tVar", "defVal")
"       endfunction
"       
"       function! SaveSettings()
"         call genutils#PutPersistentVar("T", "tVar", s:tVar)
"       endfunction
"       
"       function! SetVar(val)
"         let s:tVar = a:val
"       endfunction
"       
"       function! GetVar()
"         return s:tVar
"       endfunction
"     <<<<t.vim>>>>
"
" The pluginName and persistentVar have to be unique and are case insensitive.
"   Ideally called from your VimLeavePre autocommand handler of your plugin.
"   This simply creates a global variable which will be persisted by Vim
"   through viminfo. The variable can be read back in the next session by the
"   plugin using genutils#GetPersistentVar() function, ideally from your
"   VimEnter autocommand handler. The pluginName is to provide a name space
"   for different plugins, and avoid conflicts in using the same persistentVar
"   name.
" This feature uses the '!' option of viminfo, to avoid storing all the
"   temporary and other plugin specific global variables getting saved.
"
" void    genutils#PutPersistentVar(String pluginName, String persistentVar,
"                          String value)
" -----------------------
" Ideally called from VimEnter, this simply reads the value of the global
"   variable for the persistentVar that is saved in the viminfo in a previous
"   session using genutils#PutPersistentVar() and returns it (and default if
"   the variable is not found). It removes the variable from global space
"   before returning the value, so can be called only once. It also means that
"   genutils#PutPersistentVar should be called again in the next VimLeavePre
"   if the variable continues to be persisted.
"
" void    genutils#GetPersistentVar(String pluginName, String persistentVar,
"                          String default)
" -----------------------
" -----------------------
" These functions channel the FileChangedShell autocommand and extend it to
" create an additional fictitious FileChangedShellPre and FileChangedShellPost
" events.
"
" Add the given noarg function to the list of functions that need to be
"   notified before processing the FileChangedShell event. The function when
"   called can expand "<abuf>" or "<afile>" to get the details of the buffer
"   for which this autocommand got executed. It should return 0 to mean
"   noautoread and 1 to mean autoread the current buffer. It can also return
"   -1 to make its return value ignored and use default autoread mechanism
"   (which could still be overridden by the return value of other functions).
"   The return value of all the functions is ORed to determine the effective
"   autoread value.
"
" void    genutils#AddToFCShellPre(String funcName)
" -----------------------
" Remove the given function previously added by calling
"   genutils#AddToFCShellPre.
"
" void    genutils#RemoveFromFCShellPre(String funcName)
" -----------------------
" Same as genutils#AddToFCShellPre except that the function is called after
"   the event is processed, so this is like a fictitious FileChangedShellPost
"   event.
" 
" void    genutils#DefFCShellInstall()
" -----------------------
" Uninstall the default autocommand handler that was previously installed
"   using genutils#DefFCShellInstall. Calling this function may not actually
"   result in removing the handler, in case there are other callers still
"   dependent on it (which is kept track of by the number of times
"   genutils#DefFCShellInstall has been called).
"
" void    genutils#DefFCShellUninstall()
" -----------------------
" This function emulates the Vim's default behavior when a |timestamp| change
"   is detected. Register your functions by calling genutils#AddToFCShellPre
"   and have this function called during the FileChangedShell event (or just
"   install the default handler by calling genutils#DefFCShellInstall).  From
"   your callbacks, return 1 to mean autoread, 0 to mean noautoread and -1 to
"   mean system default (or ignore).  The return value of this method is 1 if
"   the file was reloaded and 0 otherwise. The return value of all the
"   functions is ORed to determine the effective autoread value. See my
"   perforce plugin for usage example.
"
" boolean genutils#DefFileChangedShell()
" -----------------------
" Execute a substitute command silently and without corrupting the search
"   register. It also preserves the cursor position.
" Ex:
"   To insert a tab infrontof all lines:
"         call genutils#SilentSubstitute('^', '%s//\t/e')
"   To remote all carriage returns at the line ending:
"         call genutils#SilentSubstitute("\<CR>$", '%s///e')
"
" void    genutils#SilentSubstitute(String pat, String cmd)
" -----------------------
" Delete all lines matching the given pattern silently and without corrupting
"   the search register. The range argument if passed should be a valid prefix
"   for the :global command. It also preserves the cursor position.
" Ex:
"   To delete all lines that are empty:
"         call genutils#SilentDelete('^\s*$')
"   To delete all lines that are empty only in the range 10 to 100:
"         call genutils#SilentDelete('10,100', '^\s*$')
"
" void    genutils#SilentDelete(String pat)
" void    genutils#SilentDelete(String range, String pat)
" -----------------------
" Can return a spacer from 0 to 80 characters width.
"
" String  genutils#GetSpacer(int width)
" -----------------------
" Function to prompt user for an element out of the passed in array. The
"   user will be prompted with a list of choices to make. The elements will be
"   formatted in to the given number of columns. Each element will be given a
"   number that the user can enter to indicate the selection. This is very
"   much like the inputlist() method, but better for a large number of options
"   formatted into multiple columns (instead of one per row). However, if the
"   formatted options run for multiple pages, no special handling is done.
" Params:
"   default - The default value for the selection. Default can be the
"               element-index or the element itself. If number (type() returns
"               0), it is treated as an index.
"   msg - The message that should appear in the prompt (passed to input()).
"   skip - The element that needs to be skipped from selection (pass a
"            non-existent element to disable this, such as an empty value '').
"   useDialog - if true, uses dialogs for prompts, instead of the command-line(
"                 inputdialog() instead of input()). But personally, I don't
"                 like this because the power user then can't use the
"                 expression register.
"   nCols - Number of columns to use for formatting the options. Using "1"
"	    will make the output look very like that of inputlist()
" Returns:
"   the selected element or empty string, "" if nothing is selected. Call
"   genutils#GetSelectedIndex() for the index entered by the user.
"
" Ex:
"   echo genutils#PromptForElement(map(range(0,25),
"	  \ "nr2char(char2nr('a')+v:val)") , 'd', 'Enter: ', 'x', 1, 5)
" String  genutils#PromptForElement(List array,
"     	   [String defaultValue | int defaultIndex], String msg,
"     	   String skip, boolean useDialog, int nCols)
"
" Returns the index of the element selected by the user in the previous
"   genutils#PromptForElement call. Returns -1 when the user didn't select
"   any element (aborted the selection). This function is useful if there are
"   empty or duplicate elements in the selection.
" int     genutils#GetSelectedIndex()
" -----------------------
" Deprecations:
"   - UserFileComplete() is now deprecated, use UserFileComplete2().
"   - CleanDiffOptions() is deprecated as Vim now has the :diffoff command.
"   - MakeArgumentString, MakeArgumentList and CreateArgString are deprecated.
"     Vim7 now includes call() function to receive and pass argument lists
"     around.
"   - The g:makeArgumentString and g:makeArgumentList are obsolete and are
"     deprecated, please use MakeArgumentString() and MakeArgumentList()
"     instead.
"   - FindWindowForBuffer() function is now deprecated, as the corresponding
"     Vim bugs are fixed. Use the below expr instead:
"       bufwinnr(genutils#FindBufferForName(fileName))
"   - QSort(), QSort2(), BinInsertSort() and BinInsertSort2() functions are
"     now deprecated in favor of sort() function.
"       
"
" Sample Usages Or Tips:
"   - Add the following commands to create simple sort commands.
"       command! -nargs=0 -range=% SortByLength <line1>,<line2>call
"           \ genutils#QSort('genutils#CmpByLineLengthNname', 1)
"       command! -nargs=0 -range=% RSortByLength <line1>,<line2>call
"           \ genutils#QSort('genutils#CmpByLineLengthNname', -1)
"       command! -nargs=0 -range=% SortJavaImports <line1>,<line2>call
"           \ genutils#QSort('genutils#CmpJavaImports', 1)
"
"   - You might like the following mappings to adjust spacing:
"       nnoremap <silent> <C-Space> :call genutils#ShiftWordInSpace(1)<CR>
"       nnoremap <silent> <C-BS> :call genutils#ShiftWordInSpace(-1)<CR>
"       nnoremap <silent> \cw :call genutils#CenterWordInSpace()<CR>
"       nnoremap <silent> \va :call
"           \ genutils#AlignWordWithWordInPreviousLine()<CR>
"
"   - The :find command is very useful to search for a file in path, but it
"     doesn't support file completion. Add the following command in your vimrc
"     to add this functionality:
"       command! -nargs=1 -bang -complete=customlist,genutils#UserFileComplete2
"             \ FindInPath :find<bang> <args>
"
"   - If you are running commands that generate multiple pages of output, you
"     might find it useful to redirect the output to a new buffer. Put the
"     following command in your vimrc:
"       command! -nargs=* -complete=command Redir
"             \ :new | put! =genutils#GetVimCmdOutput('<args>') |
"             \ setl bufhidden=wipe | setl nomodified
"
" Changes in 2.5:
"   - Improved genutils#CleanupFileName() to expand "~" and environment
"     variables. It also works more reliably now.
"   - More user friendly version of genutils#UserFileComplete() function
"     added as genutils#UserFileComplete2(). It can now be used directly as
"     the custom or customlist function in commands (so saves trouble for
"     users, as no wrapper needs be written), and also offer various
"     customizations via passing params.
"   - New function genutils#UserDirComplete2(), which is a customization on
"     top of genutils#UserFileComplete2() for the sake of restricting the
"     completions to directories only.
"   - Minor improvements in genutils#PathIsAbsolute() and
"     genutils#GetVimCmdOutput()
" Changes in 2.4:
"   - Fixed some corner cases in RelPathFromDir()/RelPathFromFile().
"   - Made the default comparators sort() function friendly.
" Changes in 2.3:
"   - SilentSubstitute() and SilentDelete() should preserve cursor position.
"   - CleanupFileName() should also remove any leading or trailing whitespace.
" Changes in 2.2:
"   - EscapeCommand() now supports Lists as arguments.
"   - CrUnProtectedCharsPattern() now accepts an optional "capture" argument.
"   - Renamed PromptForElement2 to PromptForElement. It was a typo.
" Changes in 2.1:
"   - Fixed a typo in AddNotifyWindowClose() in the previous release.
"   - Added BinSearchList() function.
" Changes in 2.0:
"   - Converted to Vim7 autoload script. Since there is no common prefix to
"     find all the usages of genutils functions in your script, Use the
"     pattern \<\(:\|>\|#\)\@<!\zs\u\w\+( to find all the global functions and
"     prefix the ones from genutils with genutils#.
"   - The new version is not backwards compatible with prior versions. If you
"     have plugins that depend on the older versions of genutils, you should try
"     to request the author to port their plugin to use the new genutils. If
"     having them to coexist is a must, then use the below trick:
"	- Install the latest version of genutils first. Overwriting all existing
"	  files.
"	- Open the plugin/genutils.vim file and note the value set to
"	  loaded_genutils variable.
"	- Install the older version of genutils (non autoload version) in to
"	  plugin directory, overwriting the existing file.
"	- Open the plugin/genutils.vim again and change the value of
"	  loaded_genutils variable to the value you noted before and save it.
"   - Fix for Save/RestoreHardPosition() not working right when there are
"     wrapped lines in the window.
"   - Dropped the AddToFCShell and RemoveFromFCShell functions as these can't be
"     implemented in Vim7 because of new restrictions on FileChangedShell
"     autocommand. Use AddToFcShellPre and RemoveFromFCShellPre functions
"     instead.
"   - No longer depends on multvals plugin. Inherits some useful functions from
"     multvals to make way for it to be retired. New functions are:
"     genutils#CrUnProtectedCharsPattern
"     PromptForElement/GetSelectedIndex

if exists('loaded_genutils')
  finish
endif
if v:version < 700
  echomsg 'genutils: You need at least Vim 7.0'
  finish
endif

let loaded_genutils = 205
