*minibufexpl.txt*    Mini Buffer Explorer

                                                                 *MiniBufExpl*
==============================================================================
CONTENTS                                                *MiniBufExpl-contents*

    1. Install................................|MiniBufExplInstall|
    2. Mappings...............................|MiniBufExplMappings|
       2.1 Global Mappings                    |MiniBufExplMappings-Global|
       2.2 Window Mappings                    |MiniBufExplMappings-Window|
    3. Commands...............................|MiniBufExplCommands|
    4. Options................................|MiniBufExplOptions|
       4.1 Splits.............................|MiniBufExplSplits|
       4.2 Window Size........................|MiniBufExplWindowSize|
       4.3 Automatic Opening..................|MiniBufExplAutoOpen|
       4.4 Keyboard Control...................|MiniBufExplKeyboard|
       4.5 Misc...............................|MiniBufExplMisc|
       4.6 Debugging..........................|MiniBufExplDebugging|
    5. Highlighting...........................|MiniBufExplHighlighting|
    6. Known Issues...........................|MiniBufExplKnownIssues|
    7. About..................................|MiniBufExplAbout|
    8. Changelog..............................|MiniBufExplChangelog|

==============================================================================
1. Install                                                *MiniBufExplInstall*

Normally, this file should reside in the plugins directory and be
automatically sourced. If not, you must manually source this file using
':source minibufexplorer.vim'.

==============================================================================
2. Key Mappings                                          *MiniBufExplMappings*

------------------------------------------------------------------------------
2.1 Global Mappings                               *MiniBufExplMappings-Global*

MBE has no longer provide default key bindings for controling MBE window.

You may want to add something like the following to your .vimrc file:

  map <Leader>mbe :MBEOpen<cr>
  map <Leader>mbc :MBEClose<cr>
  map <Leader>mbt :MBEToggle<cr>

Or, much shorter form:

  map <Leader>e :MBEOpen<cr>
  map <Leader>c :MBEClose<cr>
  map <Leader>t :MBEToggle<cr>

Either way, the key bindings used in these mappings should fit with your
configuration of Vim.

------------------------------------------------------------------------------
2.2 Window Mappings                               *MiniBufExplMappings-Window*

    o                                                  *MiniBufExpl-Mapping-o*
    e                                                  *MiniBufExpl-Mapping-e*
    <CR>                                            *MiniBufExpl-Mapping-<CR>*
                                   Open selected buffer in the previous window

    s                                                  *MiniBufExpl-Mapping-s*
             Open selected buffer in a horizontal split of the previous window

    v                                                  *MiniBufExpl-Mapping-v*
               Open selected buffer in a vertical split of the previous window

    d                                                  *MiniBufExpl-Mapping-d*
                  Delete selected buffer without closing the window it located

    k                                                  *MiniBufExpl-Mapping-k*
    <up>                                            *MiniBufExpl-Mapping-<up>*
                                            Move one line up in the MBE window

    j                                                  *MiniBufExpl-Mapping-j*
    <down>                                        *MiniBufExpl-Mapping-<down>*
                                          Move one line down in the MBE window

    l                                                  *MiniBufExpl-Mapping-l*
    <right>                                      *MiniBufExpl-Mapping-<right>*
                                     Move one buffer forward in the MBE window

    h                                                  *MiniBufExpl-Mapping-h*
    <left>                                        *MiniBufExpl-Mapping-<left>*
                                    Move one buffer backward in the MBE window

==============================================================================
3. Commands                                              *MiniBufExplCommands*

:MiniBufExplorer (depreciated)                              *:MiniBufExplorer*
    Use :MBEOpen instead

:CMiniBufExplorer (depreciated)                            *:CMiniBufExplorer*
    Use :MBEClose instead

:TMiniBufExplorer (depreciated)                            *:TMiniBufExplorer*
    Use :MBEToggle instead

:UMiniBufExplorer (Obsolete)                               *:UMiniBufExplorer*
    Command has been removed

:MBEFocus                                                          *:MBEFocus*
    Focus into the MBE window.

:MBEFocusAll                                                    *:MBEFocusAll*
    Focus into the MBE window in every tab page.

:MBEOpen[!]                                                         *:MBEOpen*
    Open the Explorer.
    With '!' appended, MBE will be reopened if it is already open.

:MBEOpenAll[!]                                                   *:MBEOpenAll*
    Open the Explorer in every tab page.
    With '!' appended, MBE will be reopened if it is already open.

:MBEClose[!]                                                       *:MBEClose*
    Close the Explorer if it's open.
    With '!' appended, auto-updating will be disabled even if it does not
    meet the required numbers of eligible buffers. Otherwise, auto-updating
    will only be disabled if the required numbers of eligible buffers has
    been meet.

:MBECloseAll[!]                                                 *:MBECloseAll*
    Close the Explorer in every tab page if it's open.
    With '!' appended, auto-updating will be disabled even if it does not
    meet the required numbers of eligible buffers. Otherwise, auto-updating
    will only be disabled if the required numbers of eligible buffers has
    been meet.

:MBEToggle[!]                                                     *:MBEToggle*
    Toggle the Explorer open and closed.
    With '!' appended: On the open action, it does not have any effect; On
    the close action, auto-updating will be disabled even if it does not
    meet the required numbers of eligible buffers, otherwise, auto-updating
    will only be disabled if the required numbers of eligible buffers has
    been meet.

:MBEToggleAll[!]                                               *:MBEToggleAll*
    Toggle the Explorer open and closed in every tab page.
    With '!' appended: On the open action, MBE will be reopened if it is
    already open; On the close action, auto-updating will be disabled even
    if it does not meet the required numbers of eligible buffers, otherwise,
    auto-updating will only be disabled if the required numbers of eligible
    buffers has been meet.

:MBEToggleMRU                                                  *:MBEToggleMRU*
    Toggle the buffer listing order of the MBE window between its default and
    most recently used.

:MBEToggleMRUAll                                            *:MBEToggleMRUAll*
    Toggle the buffer listing order of the MBE window between its default and
    most recently used in every tab page.

:MBEbn                                                                *:MBEbn*
    Switch to next normal buffer in current window.

:MBEbp                                                                *:MBEbp*
    Switch to previous normal buffer in current window.

:MBEbf                                                                *:MBEbf*
    Move one buffer forward in the most recent used buffer list.

:MBEbb                                                                *:MBEbb*
    Move one buffer backward in the most recent used buffer list.

:MBEbd[!] [N1] [N2] ...                                               *:MBEbd*
    Delete buffers but preserve the window that holding them.
    [N*] is the buffer's number or name to be deleted. If no [N*] is given,
    then current buffer will be deleted. Multiple buffers could be deleted
    at the same time.

:MBEbw[!] [N1] [N2] ...                                               *:MBEbw*
    Wipe out buffers but preserve the window that holding them.
    [N*] is the buffer's number or name to be wiped out. If no [N*] is given,
    then current buffer will be deleted. Multiple buffers could be unloaded
    at the same time.

:MBEbun[!] [N1] [N2] ...                                             *:MBEbun*
    Unload buffers but preserve the window that holding them.
    [N*] is the buffer's number or name to be unloaded. If no [N*] is given,
    then current buffer will be deleted. Multiple buffers could be wiped out
    at the same time.

==============================================================================
4. Options                                                *MiniBufExplOptions*

------------------------------------------------------------------------------
4.1 Splits                                                 *MiniBufExplSplits*

                                                       *'g:miniBufExplVSplit'*
If you would like a vertical explorer you can assign the column width (in
characters) you want for your explorer window with the following .vimrc
variable (this was introduced in 6.3.0):

  let g:miniBufExplVSplit = 20   " column width in chars

                                                      *'g:miniBufExplBRSplit'*
To control where the new split window goes relative to the current window, use
the setting:

  let g:miniBufExplBRSplit = 0   " Put new window above
                                 " current or on the
                                 " left for vertical split
  let g:miniBufExplBRSplit = 1   " Put new window below
                                 " current or on the
                                 " right for vertical split

The default for this is read from the 'splitbelow' or 'splitright' Vim option
which depends on 'g:miniBufExplVSplit' option.

                                                   *'g:miniBufExplSplitBelow'*
This option has been depreciated by 'g:miniBufExplBRSplit'.

                                                  *'g:miniBufExplSplitToEdge'*
By default we are now (as of 6.0.2) forcing the -MiniBufExplorer- window to
open up at the edge of the screen. You can turn this off by setting the
following variable in your .vimrc:

  let g:miniBufExplSplitToEdge = 0

------------------------------------------------------------------------------
4.2 Window Size                                        *MiniBufExplWindowSize*

                                                      *'g:miniBufExplMaxSize'*
------------------------------------------------------------------------------
Horizontal

It is now (as of 6.1.1) possible to set a maximum height
for the -MiniBufExplorer- window. You can set the max height by letting the
following variable in your .vimrc:

  let g:miniBufExplMaxSize = <max lines: default 0>

setting this to 0 will mean the window gets as big as needed to fit all your
buffers.

------------------------------------------------------------------------------
Vertical

(as of 6.3.0)

By default the vertical explorer has a fixed width . If you put:

  let g:miniBufExplMaxSize = <max width: default 0>

into your .vimrc then MBE will attempt to set the width of the MBE window to
be as wide as your widest tab. The width will not exceed MaxSize even if you
have wider tabs.

Accepting the default value of 0 for this will give you a fixed width MBE
window.

NOTE: This was *'g:miniBufExplMaxHeight'* before 6.3.0; the old
setting is backwards compatible if you don't use MaxSize.

                                                      *'g:miniBufExplMinSize'*
------------------------------------------------------------------------------
Horizontal

As of 6.2.2 it is possible to set a minimum height for the -MiniBufExplorer-
window. You can set the min height by letting the following variable in your
.vimrc:

  let g:miniBufExplMinSize = <min height: default 1>

------------------------------------------------------------------------------
Vertical

You can specify a MinSize for the vertical explorer window by putting the
following in your .vimrc:

  let g:miniBufExplMinSize = <min width: default 1>

This will have no effect unless you also specify MaxSize.

NOTE: This was *'g:miniBufExplMinHeight'* before 6.3.0; the old
setting is backwards compatible if you don't use MinSize.

------------------------------------------------------------------------------
4.3 Automatic Opening                                    *MiniBufExplAutoOpen*

                                                    *'g:miniBufExplAutoStart'*
If you only want open MBE manually when needed, you can put the following into
your .vimrc:

  let g:miniBufExplorerAutoStart = 0

                                                *'g:miniBufExplorerAutoStart'*
This option has been depreciated by 'g:miniBufExplAutoStart'.

                                                *'g:miniBufExplBuffersNeeded'*
This variable controls how man buffers should be available until MBE shows up,
if 'g:miniBufExplorerAutoStart' is enabled. By default, at least 2 buffers are
needed.

You can put the following into your .vimrc to let MBE start as soon as a normal
buffer is available.

  let g:miniBufExplBuffersNeeded = 1

You can put the following into your .vimrc to skip the eligible buffer checking.

  let g:miniBufExplBuffersNeeded = 0

                                              *'g:miniBufExplorerMoreThanOne'*
This option has been depreciated by 'g:miniBufExplBuffersNeeded'.

                                                 *'g:miniBufExplHideWhenDiff'*
Disable automatic opening if vim is started in diff mode(vimdiff, or with '-d'
option). This option only affected the first tab page created at vim start.

                                             *'g:miniBufExplorerHideWhenDiff'*
This option has been depreciated by 'g:miniBufExplHideWhenDiff'.

------------------------------------------------------------------------------
4.4 Keyboard Control                                     *MiniBufExplKeyboard*

                                              *'g:miniBufExplMapWindowNavVim'*
                                           *'g:miniBufExplMapWindowNavArrows'*
                                            *'g:miniBufExplMapCTabSwitchBufs'*
                                         *'g:miniBufExplMapCTabSwitchWindows'*
These configuration variabes have been obsolete, MBE does no longer provide
this functionality any more. If you'd like them be back, please add the
following mappings to your .vimrc file.

    " If you like control + vim direction key to navigate
    " windows then perform the remapping
    "
    noremap <C-J>     <C-W>j
    noremap <C-K>     <C-W>k
    noremap <C-H>     <C-W>h
    noremap <C-L>     <C-W>l

    " If you like control + arrow key to navigate windows
    " then perform the remapping
    "
    noremap <C-Down>  <C-W>j
    noremap <C-Up>    <C-W>k
    noremap <C-Left>  <C-W>h
    noremap <C-Right> <C-W>l

    " If you like <C-TAB> and <C-S-TAB> to switch buffers
    " in the current window then perform the remapping
    "
    noremap <C-TAB>   :MBEbn<CR>
    noremap <C-S-TAB> :MBEbp<CR>
    "
    " Or, in MRU fashion
    "
    noremap <C-TAB>   :MBEbf<CR>
    noremap <C-S-TAB> :MBEbb<CR>

    "
    " If you like <C-TAB> and <C-S-TAB> to switch windows
    " then perform the remapping
    "
    noremap <C-TAB>   <C-W>w
    noremap <C-S-TAB> <C-W>W


------------------------------------------------------------------------------
4.5 Misc                                                     *MiniBufExplMisc*

                                                        *'g:miniBufExplSetUT'*
Set the 'updatetime' to a proper value.

                                                 *'g:miniBufExplCycleArround'*

Should buffer be cycled arround if hits the begining or the end while
using MBE's buffer movement commands.

                                               *'g:miniBufExplStatusLineText'*
MBE window status line text.

                                               *'g:miniBufExplUseSingleClick'*
As of MBE 6.3.0, you can put the following into your .vimrc:

  let g:miniBufExplUseSingleClick = 1

If you would like to single click on tabs rather than double clicking on them
to goto the selected buffer.

                                                *'g:miniBufExplCloseOnSelect'*
If you would like MBE to close when you select a buffer, put:

  let g:miniBufExplCloseOnSelect = 1

into your .vimrc in order to force MBE to try to place selected buffers into a
window that does not have a nonmodifiable buffer. The upshot of this should be
that if you go into MBE and select a buffer, the buffer should not show up in
a window that is hosting an explorer.

                                               *'g:miniBufExplShowBufNumbers'*
If you would like to omit the buffer number from MBE's buffer display, put the
following in your .vimrc:

    let g:miniBufExplShowBufNumbers = 0

------------------------------------------------------------------------------
4.6 Debugging                                           *MiniBufExplDebugging*

                                                    *'g:miniBufExplDebugMode'*
You can also set a DebugMode to cause output to be target as follows (default
is mode 3):

  let g:miniBufExplDebugMode  = 0      " Errors will show up in
                                       " a Vim window
  let g:miniBufExplDebugMode  = 1      " Uses Vim's echo function
                                       " to display on the screen
  let g:miniBufExplDebugMode  = 2      " Writes to a file
                                       " MiniBufExplorer.DBG
  let g:miniBufExplDebugMode  = 3      " Store output in global variable
                                       " g:miniBufExplDebugOutput

                                                *'g:miniBufExplorerDebugMode'*
This option has been depreciated by 'g:miniBufExplDebugMode'.

                                                   *'g:miniBufExplDebugLevel'*
MBE has had a basic debugging capability for quite some time. However, it has
not been very friendly in the past. As of 6.0.8, you can put one of each of
the following into your .vimrc:

  let g:miniBufExplDebugLevel = 0      " MBE serious errors output
  let g:miniBufExplDebugLevel = 4      " MBE all errors output
  let g:miniBufExplDebugLevel = 10     " MBE reports everything

                                               *'g:miniBufExplorerDebugLevel'*
This option has been depreciated by 'g:miniBufExplDebugLevel'.

Or if you are able to start Vim, you might just perform these at a command
prompt right before you do the operation that is failing.

==============================================================================
5. Highlighting                                      *MiniBufExplHighlighting*

It is possible to customize the highlighting for the tabs in the MBE by
configuring the following highlighting groups:

  MBENormal               - for buffers that have NOT CHANGED and are
                            NOT VISIBLE
  MBEChanged              - for buffers that have CHANGED and are
                            NOT VISIBLE
  MBEVisibleNormal        - for buffers that have NOT CHANGED and are
                            VISIBLE
  MBEVisibleChanged       - for buffers that have CHANGED and are
                            VISIBLE
  MBEVisibleActiveNormal  - for buffers that have NOT CHANGED and are
                            VISIBLE and is the active buffer
  MBEVisibleActiveChanged - for buffers that have CHANGED and are
                            VISIBLE and is the active buffer

You can either link to an existing highlighting group by adding a command
like:

  hi link MBEVisibleChanged Error

to your .vimrc or you can specify exact foreground and background colors using
the following syntax:

  hi MBEChanged guibg=darkblue ctermbg=darkblue termbg=white

If you have customized the above highlight groups, remember to set
'g:did_minibufexplorer_syntax_inits = 1' so that MBE would not override
your settings.

NOTE: If you set a colorscheme in your .vimrc you should do it
      BEFORE updating the MBE highlighting groups.

==============================================================================
6. Known Issues                                       *MiniBufExplKnownIssues*

==============================================================================
6. Todo                                                      *MiniBufExplTodo*

- Add the ability to specify a regexp for eligible buffers
  allowing the ability to filter out certain buffers that
  you don't want to control from MBE.

==============================================================================
7. About                                                    *MiniBufExplAbout*

    Copyright: Copyright (C) 2002 & 2003 Bindu Wavell
               Copyright (C) 2010 Oliver Uvman
               Copyright (C) 2010 Danielle Church
               Copyright (C) 2010 Stephan Sokolow
               Copyright (C) 2010 & 2011 Federico Holgado
               Copyright (C) 2012 & 2013 Techlive Zheng

               Permission is hereby granted to use and distribute this code,
               with or without modifications, provided that this copyright
               notice is copied with it. Like anything else that's free,
               minibufexpl.vim is provided *as is* and comes with no
               warranty of any kind, either expressed or implied. In no
               event will the copyright holder be liable for any damages
               resulting from the use of this software.

  Description: Mini Buffer Explorer Vim Plugin
   Maintainer: Techlive Zheng <techlivezheng@gmail.com>
 Last Updated: Techlive Zheng <techlivezheng@gmail.com>
          URL: http://vim.sourceforge.net/scripts/script.php?script_id=159
   GitHub URL: https://github.com/techlivezheng/vim-plugin-minibufexpl
      Version: 6.5.0

==============================================================================
8. Changelog                                            *MiniBufExplChangelog*

6.5.0

    - Configuration variables changing:

        * 'g:miniBufExplModSelTarget' is absolete.
          MBE will try to avoid using any other plugin's winow.
          (cba0147)

        * 'g:miniBufExplCheckDupeBufs' is obsolete.
          (176cf27,fd737d3)

        * 'g:miniBufExplorerAutoUpdate' is obsolete.
          (3cee9f9)

        * 'g:miniBufExplForceSyntaxEnable' is obsolete.
          (373691f)

        * Several configuration variables have been renamed.
          (48f2934)

            - 'g:miniBufExplorerAutoStart'    -> 'g:miniBufExplAutoStart'
            - 'g:miniBufExplorerDebugMode'    -> 'g:miniBufExplDebugMode'
            - 'g:miniBufExplorerDebugLevel'   -> 'g:miniBufExplDebugLevel'
            - 'g:miniBufExplorerDebugOutput'  -> 'g:miniBufExplDebugOutput'
            - 'g:miniBufExplorerHideWhenDiff' -> 'g:miniBufExplHideWhenDiff'

        * 'g:statusLineText' is depreciated by 'g:miniBufExplStatusLineText'.
          (1aeb012)

        * 'g:miniBufExplSplitBelow' is depreciated by 'g:miniBufExplBRSplit'.
          Window spliting is now controled by '&splitbelow' and '&splitright'
          settings.
          (282025e)

        * 'g:miniBufExplorerAutoStart' could be used to control whether MBE
          should be startd at the startup or not.
          (f581428)

        * 'g:miniBufExplorerMoreThanOne' is depreciated by 'g:miniBufExplBuffersNeeded'.
          (4e4acec)

        * New configuration option 'g:miniBufExplCycleArround' to control
          whether buffer list should be cycled arround if hits the start of
          the end.
          (2df9b2b)

    - Commands and mappings interface changing:

        * Key binding 'p' in MBE window has been removed.
          (b45cd77)

        * New commands ':MBEbf' and ':MBEbb' now could be used to go forward
          or backward the buffer entering history, aka in MRU fashion.
          (b7d2ad5,a4e9ce9)

        * Command ':UMiniBufExplorer' has been removed, function
          'AutoUpdate()' is not supposed to be triggered manually.
          (9c12f71)

        * Several main commands have been renamed.
          (8d88b9a)

            * ':MiniBufExplorer' is depreciated by ':MBEOpen'
            * ':CMiniBufExplorer' is depreciated by ':MBEClose'
            * ':TMiniBufExplorer' is depreciated by ':MBEToggle'

        * Introduce commands ':MBEOpenAll', ':MBECloseAll',
          ':MBEToggleAll' to manage MBEs in all tabs.
          (e839c1d)

        * Introduce ':MBEFocus' and ':MBEFocusAll' to focus into
          the MBE window.
          (fa9e5d5)

        * Introduce commands ':MBEbd', ':MBEbw' and
          ':MBEbun' to delete buffer but preserve the window that it
          was in.
          (e4f9294,9b7635e)

        * Key bindings '<TAB>' and '<S-TAB>' in MBE window have been changed
          to '<right>' and '<left>' to avoid unnecessary conflict with some
          system's key bindings.
          (ca0cc8d,d788c7a)

        * MBE has no longer provide the key-bindings for switching
          between buffers and windows, this should be up to the user.
          (417e952)

        * MBE has no longer provide the default mappings for MBE's
          management, this should be up to the user.
          (a0977cd)

        * Introduce command 'MBEToggleMRU' to change the MBE listing order.
          (8a7baaa)

    - Bugfixes, Enhancements, Closed Issues, Merged Pull Requests:

      See https://github.com/fholgado/minibufexpl.vim/issues?milestone=1&page=1&state=closed
      and https://github.com/techlivezheng/vim-plugin-minibufexpl/issues?milestone=1&page=1&state=closed

        * Quit MBE if there is no more normal window open.
          (cea185b,7ebc049,b0d4c4a)

            - Fix fholgado/minibufexpl.vim#1
            - Fix fholgado/minibufexpl.vim#41
            - Fix fholgado/minibufexpl.vim#78

          These changes should also fix the following issues which state the remaining
          buffer's syntax highlighting gets lost. More details please refer to
          https://github.com/fholgado/minibufexpl.vim/issues/71#issuecomment-17781092.

            - Fix fholgado/minibufexpl.vim#24
            - Fix fholgado/minibufexpl.vim#71
            - Fix fholgado/minibufexpl.vim#73

        * Update MBE will no longer interfere window entering history.
          (3add236,4155048)

            - Fix fholgado/minibufexpl.vim#3
            - Fix fholgado/minibufexpl.vim#7
            - Fix fholgado/minibufexpl.vim#17
            - Fix fholgado/minibufexpl.vim#21
            - Fix fholgado/minibufexpl.vim#44
            - Fix fholgado/minibufexpl.vim#48
            - Fix fholgado/minibufexpl.vim#75

        * Mechanism for checking buffers with duplicate name and generating a
          unique name for each of these buffers have been completely refactored,
          it is more efficient now and each buffer should be uniquely identified.
          Thanks to jmnas@github for the orginal work.
          (f3723aa,5715dd9,950a81a,763a623)

            - Fix fholgado/minibufexpl.vim#5
            - Fix fholgado/minibufexpl.vim#6
            - Fix fholgado/minibufexpl.vim#9
            - Fix fholgado/minibufexpl.vim#13
            - Fix fholgado/minibufexpl.vim#61
            - Fix fholgado/minibufexpl.vim#77

        * Buffer now could be cycled in a MRU fashion
          (b7d2ad5,a4e9ce9)

            - Fix fholgado/minibufexpl.vim#20.

        * Prevent duplicate MBE window from opening.
          (bfd4b9a)

            - Fix fholgado/minibufexpl.vim#31
            - Fix fholgado/minibufexpl.vim#37
            - Fix fholgado/minibufexpl.vim#82
            - Fix fholgado/minibufexpl.vim#83

        * Use "very nomagic" in search() function.
          (9474294)

            - Fix fholgado/minibufexpl.vim#33
            - Merge fholgado/minibufexpl.vim#67 -- By mr-vinn@github

        * Removing trailing whitespace
          (6fd9d11)

            - Merge fholgado/minibufexpl.vim#35 -- By ubunatic@github

        * Avoid MBE window being affected by 'relativenumber' option.
          (adbac4d,eba2fc5)

            - Merge fholgado/minibufexpl.vim#43 -- By hoopes@github
            - Fix techlivezheng/vim-plugin-minibufexpl#7
            - Merge techlivezheng/vim-plugin-minibufexpl#9 -- By chadburrus@github

        * Improve documentation.
          (ed9c12f,a9d7810,ccd5f14)

            - Merge fholgado/minibufexpl.vim#45 -- By Inkane@github
            - Merge fholgado/minibufexpl.vim#53 -- By jesboat@github
            - Merge fholgado/minibufexpl.vim#57 -- By hfs@github
            - Fix techlivezheng/vim-plugin-minibufexpl#10

        * Fix a bug that a buffer with no name would be force closed.
          (c06f82d)

            - Fix fholgado/minibufexpl.vim#47
            - Fix fholgado/minibufexpl.vim#79

        * Fix a bug that switching between buffers is unfunctional when
          'g:miniBufExplShowBufNumbers' is set to 0.
          (51fb73c,40ffb01)

            - Fix fholgado/minibufexpl.vim#49
            - Merge fholgado/minibufexpl.vim#69

        * Make Powerline plugin be happy with MBE.
          (9410d5c,7584a7a)

            - Fix fholgado/minibufexpl.vim#50
            - Partially merge fholgado/minibufexpl.vim#64 -- By s5unty@github

        * Cooperate with Windows machine.

            - Fix fholgado/minibufexpl.vim#51
            - Fix fholgado/minibufexpl.vim#70
            - Fix fholgado/minibufexpl.vim#74

        * Handle buffers without a name correctly.
          (8fad45b)

            - Fix fholgado/minibufexpl.vim#54
            - Fix techlivezheng/vim-plugin-minibufexpl#1

        * No startup on diff mode.
          (391a9b2,ff9fcfe)

            - Merge fholgado/minibufexpl.vim#55 -- By dccmx@github

        * Fix focusing current buffer in vsplit MBE window.
          (917d4dc)

            - Merge fholgado/minibufexpl.vim#56 -- By tyamaz@github

        * Suppress "-- no lines in buffer --" message.
          (dcec2d1)

            - Merge fholgado/minibufexpl.vim#58 -- By tyamaz@github

        * Respect user's 'updatetime' setting, only change it when instructed
          and the original setting is not changed.
          (c99ea42,87543bc)

            - Fix fholgado/minibufexpl.vim#59
            - Fix fholgado/minibufexpl.vim#62

        * MBE buffer now has its filetype set to 'minibufexpl'.
          (02c1fa8)

            - Fix fholgado/minibufexpl.vim#60.

        * Fix bufname('%') and bufnr('%') mixing up.
          (984cffb,dd2124d,b2c692e)

            - Merge fholgado/minibufexpl.vim#63 -- By doublemarked@github

        * Toggle MBE will not affect the size of the currently opened windows.
          (bb1d90e)

            - Fix fholgado/minibufexpl.vim#80

        * Fix for &winminheight setting
          (731fca2,879c1f5)

            - Merge fholgado/minibufexpl.vim#81 -- By zhaocai@github

        * Set syntax on FileType event
          (0985c01)

            - Fix techlivezheng/vim-plugin-minibufexpl#3.

        * Fix a wrongly defined highlight group
          (572346d,452265c,fd6992a,43a019a)

            - Fix techlivezheng/vim-plugin-minibufexpl#4

        * Add the ability to manually control the start of MBE
          (f921a35)

            - Fix techlivezheng/vim-plugin-minibufexpl#5

        * Make MBE window temporarily modifiable during the resizing
          (08c9c4a,88e3924)

            - Fix techlivezheng/vim-plugin-minibufexpl#6
            - Merge techlviezheng/vim-plugin-minibufexpl#8 -- By whitelynx@github

        * Open MBE manually will ignore 'g:miniBufExplBuffersNeeded'.
          (8ac7f45)

        * Annoying "No Lines in Buffer" messages have been suppressed.
          (0126c7fc,f6f654d)

        * If a MBE buffer already exists, it will be used instead of creating
          a new one.
          (3153ffd)

        * MBE window is local to tab page. Open or close MBE window in one
          tab page would not interfere another.
          (b8deb38,7247d52)

        * Fix a bug that color highlighting gets lost in MBE window.
          (e5e6d15,362f2d8)

        * Auto updating will run inside any window if a buffers gets deleted.
          (5d6e445)

        * Open buffer in MBE will try to use the previous window first, then
          the next normal window.
          (4f9c957)

        * If the eligible buffers requirement has been meet, closing MBE will
          disable auto updating.
          (876b771)

6.4.4
    - Set up proper documentation. Thanks to Claytron :)
    - Fix colorcolumn feature detection. Thanks to sandaya :)
    - MBE now ignores buffers with empty name. Thanks to Folke
      for the pull :)
    - Fix incompatibility with tildes in filenames. Thanks to
      future for the pull :)

6.4.3
    - Fix MBE losing highlighting when a buffer is closed.
      Thanks to Markus Koller for the pull request!
    - Disable spellcheck on MBE buffer.
    - Don't use colorcolumn setting on MBE buffer. Thanks to
      grassofhust for the pull requests for this and the
      previous issue!

6.4.2
    - Moving current build out of beta. Getting ready for a
      re-write!

6.4.1b5
    - Allow users to turn off Buffer number display on explorer
      tabs courtesy of jmatraszek.
    - Allow users to turn off duplicate buffer name checking
      to speed up MBE buffer switching. We are working on
      optimizing this feature so that it is usable even with
      many buffers open.
    - Set Shellslash fix for Windows users so that duplicate
      buffer name checking works properly.
    - Re-enable syntax highlighting after cycling the buffer
      courtesy of Sontek.
    - Fix erratic :q behavior when MBE is the last buffer
      courtesy of Moopet.

6.4.1b4
    - Finally figured out how to turn off parentheses
      matching for the MBE buffer, which solves a couple of
      annoying graphical glitches. Thanks to Thomas Egreger
      for the patch!
    - Added a temporary fix for the issues with MBE and
      FuzzyFinder thanks to toupeira.

6.4.1b2
    - Fixed Dupe File Name checking function to prevent some
      errors and actually work properly!

6.4.1b1
    - Added handler function to only update MBE on changes.

6.4.0
    - Added Emacs-like 'uniquify' feature where MBE will
      show a parent directory when there are 2 buffers with
      the same filename. Example: There are 2 buffers, one
      is /ProjectA/Application/CSS/style.css and
      /ProjectB/Applications/CSS/style.css. Originally, MBE
      would just display 'style.css' for both buffers. Now,
      MBE crawls up the directory tree to find the first
      differentiating parent directory for both buffers, so
      MBE will show 'ProjectA/style.css' and
      'ProjectB/style.css' for each buffer name.
    - Now setting winfixheight and winfixwidth for the MBE
      buffer so that it does not get resized automatically
      by window resizing commands such as 'CTRL W='.

6.3.7
    - MBE now uses it's own status line format to reduce the
      amount of visual clutter. This can be customized.

6.3.6
    - MBE now updates current buffer's status on buffer save
      and when a buffer is modified. Patched by Federico
      Holgado (fholgado at gmail dot com).

6.3.5
    - Added highlighting for currently active buffer.
      Patched by Federico Holgado (fholgado at gmail dot
      com).

6.3.4
    - Now returns to augroup NONE after setting augroup
      commands. Big thanks to Maciej Laszcz for the bug
      report!

6.3.3
    - Added additional keybindings. In addition to <TAB> and
      <S-TAB>, l and h can now be used. In addition to <CR>,
      and e can now be used.
    - You can open the selected buffer in a new split window
      by pressing s while in the minibufexplorer window.
    - You can open the selected buffer in a new vertically
      split window while pressing v while in the
      minibufexplorer window. Patched by Oliver Uvman.

6.3.2
    - For some reason there was still a call to StopExplorer
      with 2 params. Very old bug. I know I fixed before,
      any way many thanks to Jason Mills for reporting this!

6.3.1
    - Include folds in source so that it's easier to
      navigate.
    - Added g:miniBufExplForceSyntaxEnable setting for folks
      that want a :syntax enable to be called when we enter
      buffers. This can resolve issues caused by a Vim bug
      where buffers show up without highlighting when another
      buffer has been closed, quit, wiped or deleted.

6.3.0
    - Added an option to allow single click (rather than
      the default double click) to select buffers in the
      MBE window. This feature was requested by AW Law
      and was inspired by taglist.vim. Note that you will
      need the latest version of taglist.vim if you want to
      use MBE and taglist both with singleclick turned on.
      Also thanks to AW Law for pointing out that you can
      make an Explorer not be listed in a standard :ls.
    - Added the ability to have your tabs show up in a
      vertical window rather than the standard horizontal
      one. Just let g:miniBufExplVSplit = <width> in your
      .vimrc and your will get this functionality.
    - If you use the vertical explorer and you want it to
      autosize then let g:miniBufExplMaxSize = <max width>
      in your .vimrc. You may use the MinSize letting in
      addition to the MaxLetting if you don't want a super
      thin window.
    - g:miniBufExplMaxHeight was renamed g:miniBufExplMaxSize
      g:miniBufExplMinHeight was renamed g:miniBufExplMinSize
      the old settings are backwards compatible if you don't
      use the new settings, but they are depreciated.

6.2.8
    - Add an option to stop MBE from targeting non-modifiable
      buffers when switching buffers. Thanks to AW Law for
      the inspiration for this. This may not work if a user
      has lots of explorer/help windows open.

6.2.7
    - Very minor bug fix for people who want to set
      loaded_minibufexplorer in their .vimrc in order to
      stop MBE from loading. 99.99% of users do not need
      this update.

6.2.6
    - Moved history to end of source file
    - Updated highlighting documentation
    - Created global commands MBEbn and MBEbp that can be
      used in mappings if folks want to cycle buffers while
      skipping non-eligible buffers.

6.2.5
    - Added <Leader>mbt key mapping which will toggle
      the MBE window. I map this to F3 in my .vimrc
      with "map <F3> :TMiniBufExplorer<CR>" which
      means I can easily close the MBE window when I'm
      not using it and get it back when I want it.
    - Changed default debug mode to 3 (write to global
      g:miniBufExplDebugOutput)
    - Made a pass through the documentation to clarify
      serveral issues and provide more complete docs
      for mappings and commands.

6.2.4
    - Because of the autocommand switch (see 6.2.0) it
      was possible to remove the restriction on the
      :set hidden option. It is now possible to use
      this option with MBE.

6.2.3
    - Added miniBufExplTabWrap option. It is turned
      off by default. When turned on spaces are added
      between tabs and gq} is issued to perform line
      formatting. This won't work very well if filenames
      contain spaces. It would be pretty easy to write
      my own formatter, but I'm too lazy, so if someone
      really needs that feature I'll add it :)

6.2.2
    - Changed the way the g:miniBufExplorerMoreThanOne
      global is handled. You can set this to the number
      of eligible buffers you want to be loaded before
      the MBE window is loaded. Setting it to 0 causes
      the MBE window to be opened even if there are no
      buffers. Setting it to 4 causes the window to stay
      closed until the 4th eligible buffer is loaded.
    - Added a MinHeight option. This is nice if you want
      the MBE window to always take the same amount of
      space. For example set MaxSize and MinSize to 2
      and set MoreThanOne to 0 and you will always have
      a 2 row (plus the ruler :) MBE window.
      NOTE: in 6.3.0 we started using MinSize instead of
      Minheight. This will still work if MinSize is not
      specified, but it is depreciated. Use MinSize instead.
    - I now setlocal foldcolumn=0 and nonumber in the MBE
      window. This is for those of you that like to have
      these options turned on locally. I'm assuming no one
      out there wants foldcolumns and line numbers in the
      MBE window? :)
    - Fixed a bug where an empty MBE window was taking half
      of the screen (partly why the MinHeight option was
      added.)

6.2.1
    - If MBE is the only window (because of :bd for example)
      and there are still eligible buffers then one of them
      will be displayed.
    - The <Leader>mbe mapping now highlights the buffer from
      the current window.
    - The delete ('d') binding in the MBE window now restors
      the cursor position, which can help if you want to
      delete several buffers in a row that are not at the
      beginning of the buffer list.
    - Added a new key binding ('p') in the MBE window to
      switch to the previous window (last edit window)

6.2.0
    - Major overhaul of autocommand and list updating code,
      we now have much better handling of :bd (which is the
      most requested feature.) As well as resolving other
      issues where the buffer list would not be updated
      automatically. The old version tried to trap specific
      events, this one just updates frequently, but it keeps
      track and only changes the screen if there has been
      a change.
    - Added g:miniBufExplMaxHeight variable so you can keep
      the -MiniBufExplorer- window small when you have lots
      of buffers (or buffers with long names :)
      NOTE: in 6.3.0 we started using MaxSize instead of
      MaxHeight. This will still work if MaxSize is not
      specified, but it is depreciated. Use MaxSize instead.
    - Improvement to internal syntax highlighting code
      I renamed the syntax group names. Anyone who has
      figured out how to use them already shouldn't have
      any trouble with the new Nameing :)
    - Added debug mode 3 which writes to a global variable
      this is fast and doesn't mess with the buffer/window
      lists.

6.1.0
    - <Leader>mbc was failing because I was calling one of
      my own functions with the wrong number of args. :(
      Thanks to Gerry Patterson for finding this!
      This code is very stable (although it has some
      idiocyncracies.)

6.0.9
    - Double clicking tabs was overwriting the clipboard
      register on MS Windows.  Thanks to Shoeb Bhinderwala
      for reporting this issue.

6.0.8
    - Apparently some Vim builds are having a hard time with
      line continuation in scripts so the few that were here
      have been removed.
    - Generalized FindExplorer and FindCreateExplorer so
      that they can be used for the debug window. Renaming
      to FindWindow and FindCreateWindow.
    - Updated debugging code so that debug output is put into
      a buffer which can then be written to disk or emailed
      to me when someone is having a major issue. Can also
      write directly to a file (VERY SLOWLY) on UNIX or Win32
      (not 95 or 98 at the moment) or use Vim's echo function
      to display the output to the screen.
    - Several people have had issues when the hidden option
      is turned on. So I have put in several checks to make
      sure folks know this if they try to use MBE with this
      option set.

6.0.7
    - Handling BufDelete autocmd so that the UI updates
      properly when using :bd (rather than going through
      the MBE UI.)
    - The AutoUpdate code will now close the MBE window when
      there is a single eligible buffer available.
      This has the useful side effect of stopping the MBE
      window from blocking the Vim session open when you close
      the last buffer.
    - Added functions, commands and maps to close & update
      the MBE window (<leader>mbc and <leader>mbu.)
    - Made MBE open/close state be sticky if set through
      StartExplorer(1) or StopExplorer(1), which are
      called from the standard mappings. So if you close
      the mbe window with \mbc it won't be automatically
      opened again unless you do a \mbe (or restart Vim).
    - Removed spaces between "tabs" (even more mini :)
    - Simplified MBE tab processing

6.0.6
    - Fixed register overwrite bug found by Sébastien Pierre

6.0.5
    - Fixed an issue with window sizing when we run out of
      buffers.
    - Fixed some weird commenting bugs.
    - Added more optional fancy window/buffer navigation:
    - You can turn on the capability to use control and the
      arrow keys to move between windows.
    - You can turn on the ability to use <C-TAB> and
      <C-S-TAB> to open the next and previous (respectively)
      buffer in the current window.
    - You can turn on the ability to use <C-TAB> and
      <C-S-TAB> to switch windows (forward and backwards
      respectively.)

6.0.4
    - Added optional fancy window navigation:
    - Holding down control and pressing a Vim direction
      [hjkl] will switch windows in the indicated direction.

6.0.3
    - Changed buffer name to -MiniBufExplorer- to resolve
      Issue in filename pattern matching on Windows.

6.0.2
    - 2 Changes requested by Suresh Govindachar:
    - Added SplitToEdge option and set it on by default
    - Added tab and shift-tab mappings in [MBE] window

6.0.1
    - Added MoreThanOne option and set it on by default
      MiniBufExplorer will not automatically open until
      more than one eligible buffers are opened. This
      reduces clutter when you are only working on a
      single file.
      NOTE: See change log for 6.2.2 for more details about
      this feature

6.0.0
    - Initial Release on November 20, 2001

==============================================================================
vim: ft=help tw=78 cc=79 et ts=8 sw=4 sts=4
