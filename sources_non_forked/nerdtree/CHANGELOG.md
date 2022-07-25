# NERDTree Change Log
<!-- Introduce a new MAJOR or MINOR version with a 4-hash header.
     PATCH versions are listed from newest to oldest under their respective MAJOR.MINOR
     version in an unordered list.  The format is:
        - **.PATCH**: Pull Request Title (PR Author) [PR Number](Link to PR)
-->
#### 6.10
- **.16**: Fix documentation errors. (lifecrisis) [#1269](https://github.com/preservim/nerdtree/pull/1269)
- **.15**: Ensure backward compatible testing of types. (lifecrisis) [#1266](https://github.com/preservim/nerdtree/pull/1266)
- **.14**: Replace trim() with a version-compatible alternative. (PhilRunninger) [#1265](https://github.com/preservim/nerdtree/pull/1265)
- **.13**: Change highlighting of bookmarks in the tree. (PhilRunninger) [#1261](https://github.com/preservim/nerdtree/pull/1261)
- **.12**: Answer the question about accessing files over scp or ftp. (PhilRunninger) [#1259](https://github.com/preservim/nerdtree/pull/1259)
- **.11**: Trim filenames created via the fs_menu (elanorigby) [#1243](https://github.com/preservim/nerdtree/pull/1243)
- **.10**: Improve F.A.Q. Answers and Issue Templates (PhilRunninger) [#1249](https://github.com/preservim/nerdtree/pull/1249)
- **.9**: `go` on a bookmark directory will NERDTreeFind it. (PhilRunninger) [#1236](https://github.com/preservim/nerdtree/pull/1236)
- **.8**: Put `Callback` function variables in local scope. (PhilRunninger) [#1230](https://github.com/preservim/nerdtree/pull/1230)
- **.7**: Fix mouse-clicking a file to open it. (PhilRunninger) [#1225](https://github.com/preservim/nerdtree/pull/1225)
- **.6**: Restore the default behavior of the `<CR>` key. (PhilRunninger) [#1221](https://github.com/preservim/nerdtree/pull/1221)
- **.5**: Fix `{'keepopen':0}` in NERDTreeCustomOpenArgs (PhilRunninger) [#1217](https://github.com/preservim/nerdtree/pull/1217)
- **.4**: Removed directory separator from sort key (Daniel E) [#1219](https://github.com/preservim/nerdtree/pull/1219)
- **.3**: Add new FAQ and answer: How to prevent buffers replacing NERDTree. (PhilRunninger) [#1215](https://github.com/preservim/nerdtree/pull/1215)
- **.2**: New menu command: Run a system command in this directory. (PhilRunninger) [#1214](https://github.com/preservim/nerdtree/pull/1214)
- **.1**: Escape quotation marks so they can be used in key mappings. (PhilRunninger) [#1213](https://github.com/preservim/nerdtree/pull/1213)
- **.0**: Enable full path specifications for NERDTreeIgnore (PhilRunninger) [#1207](https://github.com/preservim/nerdtree/pull/1207)
#### 6.9
- **.12**: Respect NERDTreeCustomOpenArgs when opening bookmark (przepompownia) [#1200](https://github.com/preservim/nerdtree/pull/1200)
- **.11**: Revamp the README. (buncis, PhilRunninger) [#1192](https://github.com/preservim/nerdtree/pull/1192), [#1193](https://github.com/preservim/nerdtree/pull/1193)
- **.10**: Open a mirrored NERDTree with correct width (PhilRunninger) [#1177](https://github.com/preservim/nerdtree/pull/1177)
- **.9**: Updated Readme, removed typo (H3RSKO) [#1167](https://github.com/preservim/nerdtree/pull/1167)
- **.8**: Refactor sort comparison functions, removing redundancy (PhilRunninger) [#1166](https://github.com/preservim/nerdtree/pull/1166)
- **.7**: Fix argument of `exists()` function calls checking for autocommands. (PhilRunninger) [#1165](https://github.com/preservim/nerdtree/pull/1165)
- **.6**: Don't use silent when raising User events (PhilRunninger) [#1164](https://github.com/preservim/nerdtree/pull/1164)
- **.5**: Fix highlight for file node.  (pirey) [#1157](https://github.com/preservim/nerdtree/pull/1157)
- **.4**: Make sure symbolic links' flags are highlighted correctly.  (PhilRunninger) [#1156](https://github.com/preservim/nerdtree/pull/1156)
- **.3**: Fix new NERDTrees' width when previous one was in the only window. (PhilRunninger) [#1153](https://github.com/preservim/nerdtree/pull/1153)
- **.2**: Fix the scope of several key mappings (lifecrisis, PhilRunninger) [#1151](https://github.com/preservim/nerdtree/pull/1151)
- **.1**: Respect user's `&shellslash` setting in CopyNode and RemoveNode functions (PhilRunninger) [#1150](https://github.com/preservim/nerdtree/pull/1150)
- **.0**: Enable opening bookmarks in split windows. (PhilRunninger) [#1144](https://github.com/preservim/nerdtree/pull/1144)
#### 6.8
- **.0**: Allow concealed characters to show another character. (PhilRunninger) [#1138](https://github.com/preservim/nerdtree/pull/1138)
#### 6.7
- **.15**: Add curly braces to the list of characters to be escaped. (PhilRunninger) [#1128](https://github.com/preservim/nerdtree/pull/1128)
- **.14**: Use backward-compatible `nerdtree#and()` in one place that was missed. (PhilRunninger) [#1134](https://github.com/preservim/nerdtree/pull/1134)
- **.13**: `cmd.exe /c start "" <filename>` for windows default viewer support. (J. Altayó) [#1130](https://github.com/preservim/nerdtree/pull/1130)
- **.12**: Fixed a bug that caused the file-tree construction to slow down significantly. (Eugenij-W) [#1126](https://github.com/preservim/nerdtree/pull/1126)
- **.11**: Fix exception in NERDTreeFind (on windows OS and If the file is located in the root directory of the disk) (Eugenij-W) [#1122](https://github.com/preservim/nerdtree/pull/1122)
- **.10**: Do not consider the tree root to be "cascadable". (lifecrisis) [#1120](https://github.com/preservim/nerdtree/pull/1120)
- **.9**: Force `:NERDTreeFocus` to allow events to be fired when switching windows. (PhilRunninger) [#1118](https://github.com/preservim/nerdtree/pull/1118)
- **.8**: Fix example code for the `NERDTreeAddKeyMap()` function. (PhilRunninger) [#1116](https://github.com/preservim/nerdtree/pull/1116)
- **.7**: Put `'%'` argument in `bufname()` for backwards compatibility. (PhilRunninger) [#1105](https://github.com/preservim/nerdtree/pull/1105)
- **.6**: If a file's already open in the window, don't edit it again. (PhilRunninger) [#1103](https://github.com/preservim/nerdtree/pull/1103)
- **.5**: Prevent unneeded tree creation in `:NERDTreeToggle[VCS] <path>` (PhilRunninger) [#1101](https://github.com/preservim/nerdtree/pull/1101)
- **.4**: Add missing calls to the `shellescape()` function (lifecrisis) [#1099](https://github.com/preservim/nerdtree/pull/1099)
- **.3**: Fix vsplit to not open empty buffers when opening previously closed file (AwkwardKore) [#1098](https://github.com/preservim/nerdtree/pull/1098)
- **.2**: Fix infinity loop (on winvim) in FindParentVCSRoot (Eugenij-W) [#1095](https://github.com/preservim/nerdtree/pull/1095)
- **.1**: File Move: Escape existing directory name when looking for open files. (PhilRunninger) [#1094](https://github.com/preservim/nerdtree/pull/1094)
- **.0**: Open the parent directory when revealing a non-existent file with :NERDTreeFind (bouk) [#1090](https://github.com/preservim/nerdtree/pull/1090)
#### 6.6
- **.1**: [add] How to install using dein.vim (kazukazuinaina) [#1087](https://github.com/preservim/nerdtree/pull/1087)
- **.0**: Add the ability to turn off directory arrows (PhilRunninger) [#1085](https://github.com/preservim/nerdtree/pull/1085)
#### 6.5
- **.0**: `NERDTreeToggle <start-directory>` always sets NERDTree root. (PhilRunninger) [#1083](https://github.com/preservim/nerdtree/pull/1083)
#### 6.4
- **.6**: NERDTreeFind shows expected message if file doesn't exist e.g. with vim-startify (andys8). [#1081](https://github.com/preservim/nerdtree/pull/1081)
- **.5**: Ensure events are (or aren't) being ignored correctly. (PhilRunninger) [#1080](https://github.com/preservim/nerdtree/pull/1080)
- **.4**: Prevent overwriting existing files/dirs on node move. (PhilRunninger) [#1079](https://github.com/preservim/nerdtree/pull/1079)
- **.3**: Fix regex that finds keyword for minimal menu. (PhilRunninger) [#1075](https://github.com/preservim/nerdtree/pull/1075)
- **.2**: Lint vimscript, fix errors and warnings, add CI job to review PRs (Caleb Maclennan) [#1071](https://github.com/preservim/nerdtree/pull/1071)
- **.1**: Ensure backward compatibility. v:t_func is not available before Vim 8.0 (Phil Runninger)
- **.0**: Allow use of function references as callbacks (HiPhish) [#1067](https://github.com/preservim/nerdtree/pull/1067)
#### 6.3
- **.0**: Add new command that behaves like NERDTreeToggle but defaults to the root of a VCS repository. (willfindlay) [#1060](https://github.com/preservim/nerdtree/pull/1060)
#### 6.2
- **.1**: Menu option, 'copy path to clipboard' is aware of VIM clipboard option (jhzn) [#1056](https://github.com/preservim/nerdtree/pull/1056)
- **.0**: Support tab-specific CWDs (PhilRunninger) [#1032](https://github.com/preservim/nerdtree/pull/1032)
#### 6.1
- **.4**: Add VIM built-in package management to read me file. (pesarkhobeee) [#1049](https://github.com/preservim/nerdtree/pull/1049)
- **.3**: Save/Set screen state also on WinLeave and WinEnter. (PhilRunninger) [#1048](https://github.com/preservim/nerdtree/pull/1048)
- **.2**: Wrap saveScreenState's statements in a try-catch block. (PhilRunninger) [#1047](https://github.com/preservim/nerdtree/pull/1047)
- **.1**: Catch errors when trying to read CHANGELOG.md. (PhilRunninger) [#1045](https://github.com/preservim/nerdtree/pull/1045)
- **.0**: If file path doesn't exist, :NERDTreeFind its parent directory instead. (PhilRunninger) [#1043](https://github.com/preservim/nerdtree/pull/1043)
#### 6.0
- **.1**: Reintroduce necessary variable mistakenly removed. (PhilRunninger) [#1040](https://github.com/preservim/nerdtree/pull/1040)
- **.0**: Make the behavior of window splits consistent (dragonxlwang, PhilRunninger) [#1035](https://github.com/preservim/nerdtree/pull/1035)
#### 5.3
- **.3**: Fix (p)ath not displaying in the minimal menu (tuzz) [#1038](https://github.com/preservim/nerdtree/pull/1038)
- **.2**: Enable events when closing NerdTree window. (PhilRunninger) [#1037](https://github.com/preservim/nerdtree/pull/1037)
- **.1**: Fix the `e` key mapping to use netrw if desired (PhilRunninger) [#1031](https://github.com/preservim/nerdtree/pull/1031)
- **.0**: Add file extension and size to sorting capabilities (PhilRunninger) [#1029](https://github.com/preservim/nerdtree/pull/1029)
#### 5.2
- **.9**: Suppress events for intermediate window/tab/buffer changes (PhilRunninger) [#1026](https://github.com/preservim/nerdtree/pull/1026)
- **.8**: Revert [#1019](https://github.com/preservim/nerdtree/pull/1019) to fix nvim artifacts and flickering. (PhilRunninger) [#1021](https://github.com/preservim/nerdtree/pull/1021)
- **.7**: Use :mode only in neovim. MacVim still needs to use :redraw! (PhilRunninger) [#1019](https://github.com/preservim/nerdtree/pull/1019)
- **.6**: In CHANGELOG.md and PR template, make reference to PR a true HTML link. (PhilRunninger) [#1017](https://github.com/preservim/nerdtree/pull/1017)
- **.5**: Use `:mode` instead of `:redraw!` when updating menu. (PhilRunninger) [#1016](https://github.com/preservim/nerdtree/pull/1016)
- **.4**: When searching for root line num, stop at end of file. (PhilRunninger) [#1015](https://github.com/preservim/nerdtree/pull/1015)
- **.3**: Fix `<CR>` key map on the bookmark (lkebin) [#1014](https://github.com/preservim/nerdtree/pull/1014)
- **.2**: Make Enter work on the `.. ( up a dir )` line (PhilRunninger) [#1013](https://github.com/preservim/nerdtree/pull/1013)
- **.1**: Fix nerdtree#version() on Windows. (PhilRunninger)
- **.0**: Expand functionality of `<CR>` mapping. (PhilRunninger) [#1011](https://github.com/preservim/nerdtree/pull/1011)
#### 5.1
- **.3**: Remove @mentions from PR template and change log. They weren't working. (PhilRunninger) [#1009](https://github.com/preservim/nerdtree/pull/1009)
- **.2**: Fix NERDTree opening with the wrong size. (PhilRunninger) [#1008](https://github.com/preservim/nerdtree/pull/1008)
- **.1**: Update Changelog and create PR Template (PhilRunninger) [#1007](https://github.com/preservim/nerdtree/pull/1007)
- **.0**: Too many changes for one patch...
    - Refresh a dir_node if the file wasn't found in it, and look once more. (PhilRunninger) [#1005](https://github.com/preservim/nerdtree/pull/1005)
    - Add a "copy path to clipboard" menu option (PhilRunninger) [#1002](https://github.com/preservim/nerdtree/pull/1002)
    - Enable root refresh on "vim ." a different way than [#999](https://github.com/preservim/nerdtree/pull/999). (PhilRunninger) [#1001](https://github.com/preservim/nerdtree/pull/1001)
    - Fix refreshroot (PhilRunninger) [#999](https://github.com/preservim/nerdtree/pull/999)
    - Change version check to look for 703 not 730 (vhalis) [#994](https://github.com/preservim/nerdtree/pull/994)
    - Change minimum vim (PhilRunninger) [#991](https://github.com/preservim/nerdtree/pull/991)
    - Allow multi-character DirArrows (PhilRunninger) [#985](https://github.com/preservim/nerdtree/pull/985)
    - Remove redraw! while still clearing last message empty string. (PhilRunninger) [#979](https://github.com/preservim/nerdtree/pull/979)
    - fix `_initChildren` function value set to numChildrenCached error (terryding77) [#969](https://github.com/preservim/nerdtree/pull/969)
    - On Windows, do a case-insensitive comparison of paths. (PhilRunninger) [#967](https://github.com/preservim/nerdtree/pull/967)
    - Remove the **Please wait... DONE** messages. (PhilRunninger) [#966](https://github.com/preservim/nerdtree/pull/966)
    - Smarter delimiter default (PhilRunninger) [#963](https://github.com/preservim/nerdtree/pull/963)
    - Update directory .vimdc readme example (spencerdcarlson) [#961](https://github.com/preservim/nerdtree/pull/961)
    - Preview bookmarks (PhilRunninger) [#956](https://github.com/preservim/nerdtree/pull/956)
    - Add new value to NERDTreeQuitOnOpen to close bookmark table (PhilRunninger) [#955](https://github.com/preservim/nerdtree/pull/955)
    - Add an :EditBookmarks command to edit the bookmarks file (PhilRunninger) [#954](https://github.com/preservim/nerdtree/pull/954)
    - Before copying, turn off &shellslash. Restore after copy is finished. (PhilRunninger) [#952](https://github.com/preservim/nerdtree/pull/952)
    - Set a maximum window size when zooming. (PhilRunninger) [#950](https://github.com/preservim/nerdtree/pull/950)
    - Confirm the wipeout of a unsaved buffer whose file has been renamed. (PhilRunninger) [#949](https://github.com/preservim/nerdtree/pull/949)
    - Escape a backslash so it can be used in a key mapping. (PhilRunninger) [#948](https://github.com/preservim/nerdtree/pull/948)
    - Add a NERDTreeMinimalMenu feature (tuzz) [#938](https://github.com/preservim/nerdtree/pull/938)
    - fixed root path error for windows (zcodes) [#935](https://github.com/preservim/nerdtree/pull/935)
    - Restore getDirChildren for use in nerdtree-project-plugin. (PhilRunninger) [#929](https://github.com/preservim/nerdtree/pull/929)
    - Document NERDTreeNodeDelimiter [#912](https://github.com/preservim/nerdtree/pull/912) (PhilRunninger) [#926](https://github.com/preservim/nerdtree/pull/926)
    - Allow modification of menu keybindings (Leandros) [#923](https://github.com/preservim/nerdtree/pull/923)
    - Add two more disqualifications for isCascadable(). (PhilRunninger) [#914](https://github.com/preservim/nerdtree/pull/914)
    - Allow highlighting more than one flag. (kristijanhusak) [#908](https://github.com/preservim/nerdtree/pull/908)
    - Support sorting files and directories by modification time. (PhilRunninger) [#901](https://github.com/preservim/nerdtree/pull/901)
    - Parse . and .. from path string with trailing slash. (PhilRunninger) [#899](https://github.com/preservim/nerdtree/pull/899)
    - Force sort to recalculate the cached sortKey. (PhilRunninger) [#898](https://github.com/preservim/nerdtree/pull/898)
    - Add NERDTreeRefreshRoot command (wgfm) [#897](https://github.com/preservim/nerdtree/pull/897)
    - Call Resolve on the file's path when calling :NERDTreeFind. (PhilRunninger) [#896](https://github.com/preservim/nerdtree/pull/896)
    - Catch all errors, not just NERDTree errors. (PhilRunninger) [#894](https://github.com/preservim/nerdtree/pull/894)
    - Fix typo in help file (lvoisin) [#892](https://github.com/preservim/nerdtree/pull/892)
    - Make NERDTreeCreator set the `'nolist'` option (lifecrisis) [#889](https://github.com/preservim/nerdtree/pull/889)
    - Refresh buffers after `m`, `m` operation on a folder (PhilRunninger) [#888](https://github.com/preservim/nerdtree/pull/888)
    - Use a better arg for FINDSTR when using the m,l command in Windows. (PhilRunninger) [#887](https://github.com/preservim/nerdtree/pull/887)
    - Fix the <C-J>/<C-K> motions, which currently fail with cascades (lifecrisis) [#886](https://github.com/preservim/nerdtree/pull/886)
    - Function "s:UI.getLineNum()" doesn't always work on cascades. (lifecrisis) [#882](https://github.com/preservim/nerdtree/pull/882)
    - NERDTreeCWD: reset CWD if changed by NERDTreeFocus (PhilRunninger) [#878](https://github.com/preservim/nerdtree/pull/878)
    - Use <count>tabnext instead of <count>gt to allow users to remap gt. (PhilRunninger) [#877](https://github.com/preservim/nerdtree/pull/877)
    - Do a case sensitive comparison of new/existing buffers. (PhilRunninger) [#875](https://github.com/preservim/nerdtree/pull/875)
    - Fix opening sub-directories that have commas in their name. (PhilRunninger) [#873](https://github.com/preservim/nerdtree/pull/873)
    - Add new command to open NERDTree in the root of a VCS repository. (PhilRunninger) [#872](https://github.com/preservim/nerdtree/pull/872)
    - Make sure the path to the bookmarks file exists before writing it. (PhilRunninger) [#871](https://github.com/preservim/nerdtree/pull/871)
    - Unzoom NERDTree when opening a file (PhilRunninger) [#870](https://github.com/preservim/nerdtree/pull/870)
    - Support unusual characters in file and directory names (PhilRunninger) [#868](https://github.com/preservim/nerdtree/pull/868)
    - Reword renamed-buffer prompt to be more clear (aflock) [#867](https://github.com/preservim/nerdtree/pull/867)
    - Default to placing cursor on root when closing bookmark table (lifecrisis) [#866](https://github.com/preservim/nerdtree/pull/866)
    - Fix issues with sorting of nodes (PhilRunninger) [#856](https://github.com/preservim/nerdtree/pull/856)
    - Better OSX detection (bubba-h57) [#853](https://github.com/preservim/nerdtree/pull/853)
    - Bugfix - ensure keymaps dictionary exists before using it (mnussbaum) [#852](https://github.com/preservim/nerdtree/pull/852)
    - Decrease startup-time by avoiding linear-time iteration over key mappings (mnussbaum) [#851](https://github.com/preservim/nerdtree/pull/851)
    - Add code to sort mappings in quickhelp (lifecrisis) [#849](https://github.com/preservim/nerdtree/pull/849)
    - Use ":clearjumps" in new NERDTree windows (lifecrisis) [#844](https://github.com/preservim/nerdtree/pull/844)
    - Like m-c did before, create parent directories if needed on m-m. (PhilRunninger) [#840](https://github.com/preservim/nerdtree/pull/840)
    - BUGFIX: Repair a problem with the `'u'` mapping. (lifecrisis) [#838](https://github.com/preservim/nerdtree/pull/838)
    - Make the NERDTree buffer writable when rendering it. (PhilRunninger) [#837](https://github.com/preservim/nerdtree/pull/837)
    - Code cleanup: Remove unsupported bookmark table mappings (lifecrisis) [#835](https://github.com/preservim/nerdtree/pull/835)
    - Replace strcharpart() with substitute() for backward compatibility (bravestarr) [#834](https://github.com/preservim/nerdtree/pull/834)
    - Fixed error `unknown function strcharpart` for older versions of Vim (hav4ik) [#833](https://github.com/preservim/nerdtree/pull/833)
    - Clear output when NERDTree menu is aborted (lifecrisis) [#832](https://github.com/preservim/nerdtree/pull/832)
    - Display a path with multi-byte characters correctly when it is truncated (bravestarr) [#830](https://github.com/preservim/nerdtree/pull/830)
    - Support revealing file and executing file with xdg-open for Linux (ngnmhieu) [#824](https://github.com/preservim/nerdtree/pull/824)
    - If node isn't open, count children on disk before deleting. (PhilRunninger) [#822](https://github.com/preservim/nerdtree/pull/822)
    - Add new variable g:NERDTreeRemoveFileCmd (kutsan) [#816](https://github.com/preservim/nerdtree/pull/816)
    - Use a better check for existence of the NERDTree buffer. (PhilRunninger) [#814](https://github.com/preservim/nerdtree/pull/814)
    - Fix focussing previous buffer when closing NERDTree (mrubli) [#801](https://github.com/preservim/nerdtree/pull/801)
    - Update the docs for "NERDTreeStatusline" (lifecrisis) [#796](https://github.com/preservim/nerdtree/pull/796)
    - BUGFIX: Unstable behavior in the "getPath()" method (lifecrisis) [#795](https://github.com/preservim/nerdtree/pull/795)
    - Revert the bugfix from pull request [#785](https://github.com/preservim/nerdtree/pull/785) (lifecrisis) [#794](https://github.com/preservim/nerdtree/pull/794)
    - BUGFIX: Allow ":NERDTreeFind" to discover hidden files (lifecrisis) [#786](https://github.com/preservim/nerdtree/pull/786)
    - BUGFIX: Allow ":NERDTreeFind" to reveal new files (lifecrisis) [#785](https://github.com/preservim/nerdtree/pull/785)
    - Add modelines (lifecrisis) [#782](https://github.com/preservim/nerdtree/pull/782)
    - Change the type of completion used by NERDTreeFind (lifecrisis) [#781](https://github.com/preservim/nerdtree/pull/781)
    - change NERDTreeFind with args (zhenyangze) [#778](https://github.com/preservim/nerdtree/pull/778)
    - Style Choice: Using confirm() when deleting a bookmark (lifecrisis) [#777](https://github.com/preservim/nerdtree/pull/777)
    - remove useless substitute when `file =~# "/$"` (skyblueee) [#773](https://github.com/preservim/nerdtree/pull/773)
    - remove useless removeLeadingSpaces in _stripMarkup (skyblueee) [#772](https://github.com/preservim/nerdtree/pull/772)
    - Make the "o" mapping consistent with "x" (lifecrisis) [#769](https://github.com/preservim/nerdtree/pull/769)
    - Fix a problem with the "x" handler (lifecrisis) [#768](https://github.com/preservim/nerdtree/pull/768)
    - Clean up the handler for the "x" mapping (lifecrisis) [#767](https://github.com/preservim/nerdtree/pull/767)
    - Revert change to tab opening method (lifecrisis) [#766](https://github.com/preservim/nerdtree/pull/766)
    - BUGFIX: Add back support for "b:NERDTreeRoot" (lifecrisis) [#765](https://github.com/preservim/nerdtree/pull/765)
    - Fix broken "t" and "T" mappings, tabs now open at end (lifecrisis) [#759](https://github.com/preservim/nerdtree/pull/759)
    - Update doc with already existing mapping variables (asnr) [#699](https://github.com/preservim/nerdtree/pull/699)
    - Fix the broken g:NERDTreeBookmarksSort setting (lifecrisis) [#696](https://github.com/preservim/nerdtree/pull/696)
    - Correct NERDTreeIgnore pattern in doc (cntoplolicon) [#648](https://github.com/preservim/nerdtree/pull/648)
    - Remove empty segments when splitting path (sooth-sayer) [#574](https://github.com/preservim/nerdtree/pull/574)
    - Suppress autocmds less agressively (wincent) [#578](https://github.com/preservim/nerdtree/pull/578) [#691](https://github.com/preservim/nerdtree/pull/691)
    - Add an Issues template to ask for more info initially.
    - Fix markdown headers in readme (josephfrazier) [#676](https://github.com/preservim/nerdtree/pull/676)
    - Don't touch `@o` and `@h` registers when rendering
    - Fix bug with files and directories with dollar signs (alegen) [#649](https://github.com/preservim/nerdtree/pull/649)
    - Reuse/reopen existing window trees where possible [#244](https://github.com/preservim/nerdtree/pull/244)
    - Remove NERDTree.previousBuf()
    - Change color of arrow (Leeiio) [#630](https://github.com/preservim/nerdtree/pull/630)
    - Improved a tip in README.markdown (ggicci) [#628](https://github.com/preservim/nerdtree/pull/628)
    - Shorten delete confimration of empty directory to `y` (mikeperri) [#530](https://github.com/preservim/nerdtree/pull/530)
    - Fix API call to open directory tree in window (devm33) [#533](https://github.com/preservim/nerdtree/pull/533)
    - Change default arrows on non-Windows platforms (gwilk) [#546](https://github.com/preservim/nerdtree/pull/546)
    - Update to README - combine cd and git clone (zwhitchcox) [#584](https://github.com/preservim/nerdtree/pull/584)
    - Update to README - Tip: start NERDTree when vim starts (therealplato) [#593](https://github.com/preservim/nerdtree/pull/593)
    - Escape filename when moving an open buffer (zacharyvoase) [#595](https://github.com/preservim/nerdtree/pull/595)
    - Fixed incorrect :helptags command in README (curran) [#619](https://github.com/preservim/nerdtree/pull/619)
    - Fixed incomplete escaping of folder arrows (adityanatraj) [#548](https://github.com/preservim/nerdtree/pull/548)
    - Added NERDTreeCascadeSingleChildDir option (juanibiapina) [#558](https://github.com/preservim/nerdtree/pull/558)
    - Replace strchars() with backward compatible workaround.
    - Add support for copy command in Windows (SkylerLipthay) [#231](https://github.com/preservim/nerdtree/pull/231)
    - Fixed typo in README.markdown - :Helptags -> :helptags
    - Rename "primary" and "secondary" trees to "tab" and "window" trees.
    - Move a bunch of buffer level variables into the NERDTree and UI classes.
    - Display cascading dirs on one line to save vertical/horizontal space (matt-gardner: brainstorming/testing)
    - Remove the old style UI - Remove `NERDTreeDirArrows` option.
    - On windows default to + and ~ for expand/collapse directory symbols.
    - Lots more refactoring. Move a bunch of b: level vars into b:NERDTree and friends.

#### 5.0.0
- Refactor the code significantly:
    * Break the classes out into their own files.
    * Make the majority of the code OO - previously large parts were effectively a tangle of "global" methods.
- Add an API to assign flags to nodes. This allows VCS plugins like https://github.com/Xuyuanp/nerdtree-git-plugin to exist. Thanks to **Xuyuanp** for helping design/test/build said API.
- add `scope` argument to the key map API see :help NERDTreeAddKeyMap()
- add magic [[dir]] and [[file]] flags to NERDTreeIgnore
- add support for custom path filters. See :help NERDTreeAddPathFilter()
- add path listener API. See :help NERDTreePathListenerAPI.
- expand the fs menu functionality to list file properties (PhilRunninger, apbarrero, JESii)
- make bookmarks work with `~` home shortcuts (hiberabyss)
- show OSX specific fsmenu options in regular vim on mac (evindor)
- make dir arrow icons configurable (PickRelated)
- optimise node sorting performance when opening large dirs (vtsang)
- make the root note render prettier by truncating it at a path slash (gcmt)
- remove NERDChristmasTree option - its always christmas now
- add "cascade" open and closing for dirs containing only another single dir. See :help NERDTreeCascadeOpenSingleChildDir (pendulm)
- Many other fixes, doc updates and contributions from: **actionshrimp**, **agrussellknives**, **alvan**, **AndrewRadev**, **cperl82** (*many small fixes*), **devmanhinton**, **egalpin**, **franksort**, **gastropoda**, **handcraftedbits**, **kelaban**, **lucascaton**, **mixvin**, **pendulm**, **SchDen**, **shanesmith**, **staeff**, **stephenprater**, **toiffel**, **Twinside**, **WoLpH**, **xiaodili**, **zhangoose**

#### 4.2.0
- Add NERDTreeDirArrows option to make the UI use pretty arrow chars instead of the old +~| chars to define the tree structure (sickill)
- shift the syntax highlighting out into its own syntax file (gnap)
- add some mac specific options to the filesystem menu - for macvim only (andersonfreitas)
- Add NERDTreeMinimalUI option to remove some non functional parts of the nerdtree ui (camthompson)
- tweak the behaviour of :NERDTreeFind - see :help :NERDTreeFind for the new behaviour (benjamingeiger)
- if no name is given to :Bookmark, make it default to the name of the target file/dir (minyoung)
- use `file` completion when doing copying, create, and move operations (EvanDotPro)
- lots of misc bug fixes from: **AndrewRadev**, **Bogdanov**, **camthompson**, **kml**, **mathias**, **paddyoloughlin**, **scottstvnsn**, **sdewald**, **Vitaly**, **wycats**, me RAWR!

#### 4.1.0
- features:
    - NERDTreeFind to reveal the node for the current buffer in the tree, see `|NERDTreeFind|`. This effectively merges the FindInNERDTree plugin (by **Doug McInnes**) into the script.
    - make NERDTreeQuitOnOpen apply to the t/T keymaps too. Thanks to **Stefan Ritter** and **Rémi Prévost**.
    - truncate the root node if wider than the tree window. Thanks to **Victor Gonzalez**.

- bugfixes:
    - really fix window state restoring
    - fix some win32 path escaping issues. Thanks to **Stephan Baumeister**, **Ricky**, **jfilip1024**, and **Chris Chambers**.

#### 4.0.0
- add a new programmable menu system (see `:help NERDTreeMenu`).
- add new APIs to add menus/menu-items to the menu system as well as custom key mappings to the NERD tree buffer (see `:help NERDTreeAPI`).
- removed the old API functions
- added a mapping to maximize/restore the size of nerd tree window, thanks to Guillaume Duranceau for the patch. See :help NERDTree-A for details.
- fix a bug where secondary nerd trees (netrw hijacked trees) and NERDTreeQuitOnOpen didnt play nicely, thanks to **Curtis Harvey**.
- fix a bug where the script ignored directories whose name ended in a dot, thanks to **Aggelos Orfanakos** for the patch.
- fix a bug when using the x mapping on the tree root, thanks to **Bryan Venteicher** for the patch.
- fix a bug where the cursor position/window size of the nerd tree buffer wasnt being stored on closing the window, thanks to **Richard Hart**.
- fix a bug where NERDTreeMirror would mirror the wrong tree

#### 3.1.1
- fix a bug where a non-listed no-name buffer was getting created every time the tree windows was created, thanks to **Derek Wyatt** and **owen1**
- make `<CR>` behave the same as the `o` mapping
- some helptag fixes in the doc, thanks **strull**.
- fix a bug when using `:set nohidden` and opening a file where the previous buf was modified. Thanks **iElectric**.
- other minor fixes

#### 3.1.0
- New features:
    - add mappings to open files in a vsplit, see `:help NERDTree-s` and `:help NERDTree-gs`
    - make the statusline for the nerd tree window default to something hopefully more useful. See `:help 'NERDTreeStatusline'`
- Bugfixes:
    - make the hijack netrw functionality work when vim is started with `vim <some dir>` (thanks to **Alf Mikula** for the patch).
    - fix a bug where the CWD wasnt being changed for some operations even when NERDTreeChDirMode==2 (thanks to **Lucas S. Buchala**)
    - add -bar to all the nerd tree :commands so they can chain with other :commands (thanks to **tpope**)
    - fix bugs when ignorecase was set (thanks to **nach**)
    - fix a bug with the relative path code (thanks to **nach**)
    - fix a bug where doing a `:cd` would cause `:NERDTreeToggle` to fail (thanks **nach**)


#### 3.0.1
- Bugfixes:
    - fix bugs with :NERDTreeToggle and :NERDTreeMirror when `'hidden'` was not set
    - fix a bug where `:NERDTree <path>` would fail if `<path>` was relative and didnt start with a `./` or `../`  Thanks to **James Kanze**.
    - make the `q` mapping work with secondary (`:e <dir>`  style) trees, thanks to **jamessan**
    - fix a bunch of small bugs with secondary trees
- More insane refactoring.

#### 3.0.0
- hijack netrw so that doing an `:edit <directory>`  will put a NERD tree in the window rather than a netrw browser. See :help 'NERDTreeHijackNetrw'
- allow sharing of trees across tabs, see `:help :NERDTreeMirror`
- remove "top" and "bottom" as valid settings for NERDTreeWinPos
- change the `'<tab>'` mapping to `'i'`
- change the `'H'` mapping to `'I'`
- lots of refactoring
