if exists("g:loaded_nerdtree_autoload")
    finish
endif
let g:loaded_nerdtree_autoload = 1

function! nerdtree#version()
    return '4.2.0'
endfunction

" SECTION: General Functions {{{1
"============================================================

"FUNCTION: nerdtree#checkForBrowse(dir) {{{2
"inits a secondary nerd tree in the current buffer if appropriate
function! nerdtree#checkForBrowse(dir)
    if a:dir != '' && isdirectory(a:dir)
        call g:NERDTreeCreator.CreateSecondary(a:dir)
    endif
endfunction

" FUNCTION: nerdtree#completeBookmarks(A,L,P) {{{2
" completion function for the bookmark commands
function! nerdtree#completeBookmarks(A,L,P)
    return filter(g:NERDTreeBookmark.BookmarkNames(), 'v:val =~# "^' . a:A . '"')
endfunction

"FUNCTION: nerdtree#compareBookmarks(dir) {{{2
function! nerdtree#compareBookmarks(first, second)
    return a:first.compareTo(a:second)
endfunction

"FUNCTION: nerdtree#compareNodes(dir) {{{2
function! nerdtree#compareNodes(n1, n2)
    return a:n1.path.compareTo(a:n2.path)
endfunction

" FUNCTION: nerdtree#deprecated(func, [msg]) {{{2
" Issue a deprecation warning for a:func. If a second arg is given, use this
" as the deprecation message
function! nerdtree#deprecated(func, ...)
    let msg = a:0 ? a:func . ' ' . a:1 : a:func . ' is deprecated'

    if !exists('s:deprecationWarnings')
        let s:deprecationWarnings = {}
    endif
    if !has_key(s:deprecationWarnings, a:func)
        let s:deprecationWarnings[a:func] = 1
        echomsg msg
    endif
endfunction

" FUNCTION: nerdtree#exec(cmd) {{{2
" same as :exec cmd  but eventignore=all is set for the duration
function! nerdtree#exec(cmd)
    let old_ei = &ei
    set ei=all
    exec a:cmd
    let &ei = old_ei
endfunction

" FUNCTION: nerdtree#has_opt(options, name) {{{2
function! nerdtree#has_opt(options, name)
    return has_key(a:options, a:name) && a:options[a:name] == 1
endfunction

" FUNCTION: nerdtree#loadClassFiles() {{{2
function! nerdtree#loadClassFiles()
    runtime lib/nerdtree/path.vim
    runtime lib/nerdtree/menu_controller.vim
    runtime lib/nerdtree/menu_item.vim
    runtime lib/nerdtree/key_map.vim
    runtime lib/nerdtree/bookmark.vim
    runtime lib/nerdtree/tree_file_node.vim
    runtime lib/nerdtree/tree_dir_node.vim
    runtime lib/nerdtree/opener.vim
    runtime lib/nerdtree/creator.vim
    runtime lib/nerdtree/flag_set.vim
    runtime lib/nerdtree/nerdtree.vim
    runtime lib/nerdtree/ui.vim
    runtime lib/nerdtree/event.vim
    runtime lib/nerdtree/notifier.vim
endfunction

" FUNCTION: nerdtree#postSourceActions() {{{2
function! nerdtree#postSourceActions()
    call g:NERDTreeBookmark.CacheBookmarks(0)
    call nerdtree#ui_glue#createDefaultBindings()

    "load all nerdtree plugins
    runtime! nerdtree_plugin/**/*.vim
endfunction

"FUNCTION: nerdtree#runningWindows(dir) {{{2
function! nerdtree#runningWindows()
    return has("win16") || has("win32") || has("win64")
endfunction

"FUNCTION: nerdtree#treeMarkupReg(dir) {{{2
function! nerdtree#treeMarkupReg()
    if g:NERDTreeDirArrows
        return '^\([▾▸] \| \+[▾▸] \| \+\)'
    endif

    return '^[ `|]*[\-+~]'
endfunction

"FUNCTION: nerdtree#treeUpDirLine(dir) {{{2
function! nerdtree#treeUpDirLine()
    return '.. (up a dir)'
endfunction

"FUNCTION: nerdtree#treeWid(dir) {{{2
function! nerdtree#treeWid()
    return 2
endfunction

" SECTION: View Functions {{{1
"============================================================

"FUNCTION: nerdtree#closeTree() {{{2
"Closes the primary NERD tree window for this tab
function! nerdtree#closeTree()
    if !nerdtree#isTreeOpen()
        throw "NERDTree.NoTreeFoundError: no NERDTree is open"
    endif

    if winnr("$") != 1
        if winnr() == nerdtree#getTreeWinNum()
            call nerdtree#exec("wincmd p")
            let bufnr = bufnr("")
            call nerdtree#exec("wincmd p")
        else
            let bufnr = bufnr("")
        endif

        call nerdtree#exec(nerdtree#getTreeWinNum() . " wincmd w")
        close
        call nerdtree#exec(bufwinnr(bufnr) . " wincmd w")
    else
        close
    endif
endfunction

"FUNCTION: nerdtree#closeTreeIfOpen() {{{2
"Closes the NERD tree window if it is open
function! nerdtree#closeTreeIfOpen()
   if nerdtree#isTreeOpen()
      call nerdtree#closeTree()
   endif
endfunction

"FUNCTION: nerdtree#closeTreeIfQuitOnOpen() {{{2
"Closes the NERD tree window if the close on open option is set
function! nerdtree#closeTreeIfQuitOnOpen()
    if g:NERDTreeQuitOnOpen && nerdtree#isTreeOpen()
        call nerdtree#closeTree()
    endif
endfunction

"FUNCTION: nerdtree#dumpHelp  {{{2
"prints out the quick help
function! nerdtree#dumpHelp()
    let old_h = @h
    if b:treeShowHelp ==# 1
        let @h=   "\" NERD tree (" . nerdtree#version() . ") quickhelp~\n"
        let @h=@h."\" ============================\n"
        let @h=@h."\" File node mappings~\n"
        let @h=@h."\" ". (g:NERDTreeMouseMode ==# 3 ? "single" : "double") ."-click,\n"
        let @h=@h."\" <CR>,\n"
        if b:NERDTreeType ==# "primary"
            let @h=@h."\" ". g:NERDTreeMapActivateNode .": open in prev window\n"
        else
            let @h=@h."\" ". g:NERDTreeMapActivateNode .": open in current window\n"
        endif
        if b:NERDTreeType ==# "primary"
            let @h=@h."\" ". g:NERDTreeMapPreview .": preview\n"
        endif
        let @h=@h."\" ". g:NERDTreeMapOpenInTab.": open in new tab\n"
        let @h=@h."\" ". g:NERDTreeMapOpenInTabSilent .": open in new tab silently\n"
        let @h=@h."\" middle-click,\n"
        let @h=@h."\" ". g:NERDTreeMapOpenSplit .": open split\n"
        let @h=@h."\" ". g:NERDTreeMapPreviewSplit .": preview split\n"
        let @h=@h."\" ". g:NERDTreeMapOpenVSplit .": open vsplit\n"
        let @h=@h."\" ". g:NERDTreeMapPreviewVSplit .": preview vsplit\n"

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Directory node mappings~\n"
        let @h=@h."\" ". (g:NERDTreeMouseMode ==# 1 ? "double" : "single") ."-click,\n"
        let @h=@h."\" ". g:NERDTreeMapActivateNode .": open & close node\n"
        let @h=@h."\" ". g:NERDTreeMapOpenRecursively .": recursively open node\n"
        let @h=@h."\" ". g:NERDTreeMapCloseDir .": close parent of node\n"
        let @h=@h."\" ". g:NERDTreeMapCloseChildren .": close all child nodes of\n"
        let @h=@h."\"    current node recursively\n"
        let @h=@h."\" middle-click,\n"
        let @h=@h."\" ". g:NERDTreeMapOpenExpl.": explore selected dir\n"

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Bookmark table mappings~\n"
        let @h=@h."\" double-click,\n"
        let @h=@h."\" ". g:NERDTreeMapActivateNode .": open bookmark\n"
        let @h=@h."\" ". g:NERDTreeMapOpenInTab.": open in new tab\n"
        let @h=@h."\" ". g:NERDTreeMapOpenInTabSilent .": open in new tab silently\n"
        let @h=@h."\" ". g:NERDTreeMapDeleteBookmark .": delete bookmark\n"

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Tree navigation mappings~\n"
        let @h=@h."\" ". g:NERDTreeMapJumpRoot .": go to root\n"
        let @h=@h."\" ". g:NERDTreeMapJumpParent .": go to parent\n"
        let @h=@h."\" ". g:NERDTreeMapJumpFirstChild  .": go to first child\n"
        let @h=@h."\" ". g:NERDTreeMapJumpLastChild   .": go to last child\n"
        let @h=@h."\" ". g:NERDTreeMapJumpNextSibling .": go to next sibling\n"
        let @h=@h."\" ". g:NERDTreeMapJumpPrevSibling .": go to prev sibling\n"

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Filesystem mappings~\n"
        let @h=@h."\" ". g:NERDTreeMapChangeRoot .": change tree root to the\n"
        let @h=@h."\"    selected dir\n"
        let @h=@h."\" ". g:NERDTreeMapUpdir .": move tree root up a dir\n"
        let @h=@h."\" ". g:NERDTreeMapUpdirKeepOpen .": move tree root up a dir\n"
        let @h=@h."\"    but leave old root open\n"
        let @h=@h."\" ". g:NERDTreeMapRefresh .": refresh cursor dir\n"
        let @h=@h."\" ". g:NERDTreeMapRefreshRoot .": refresh current root\n"
        let @h=@h."\" ". g:NERDTreeMapMenu .": Show menu\n"
        let @h=@h."\" ". g:NERDTreeMapChdir .":change the CWD to the\n"
        let @h=@h."\"    selected dir\n"
        let @h=@h."\" ". g:NERDTreeMapCWD .":change tree root to CWD\n"

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Tree filtering mappings~\n"
        let @h=@h."\" ". g:NERDTreeMapToggleHidden .": hidden files (" . (b:NERDTreeShowHidden ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleFilters .": file filters (" . (b:NERDTreeIgnoreEnabled ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleFiles .": files (" . (b:NERDTreeShowFiles ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleBookmarks .": bookmarks (" . (b:NERDTreeShowBookmarks ? "on" : "off") . ")\n"

        "add quickhelp entries for each custom key map
        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Custom mappings~\n"
        for i in g:NERDTreeKeyMap.All()
            if !empty(i.quickhelpText)
                let @h=@h."\" ". i.key .": ". i.quickhelpText ."\n"
            endif
        endfor

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Other mappings~\n"
        let @h=@h."\" ". g:NERDTreeMapQuit .": Close the NERDTree window\n"
        let @h=@h."\" ". g:NERDTreeMapToggleZoom .": Zoom (maximize-minimize)\n"
        let @h=@h."\"    the NERDTree window\n"
        let @h=@h."\" ". g:NERDTreeMapHelp .": toggle help\n"
        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Bookmark commands~\n"
        let @h=@h."\" :Bookmark [<name>]\n"
        let @h=@h."\" :BookmarkToRoot <name>\n"
        let @h=@h."\" :RevealBookmark <name>\n"
        let @h=@h."\" :OpenBookmark <name>\n"
        let @h=@h."\" :ClearBookmarks [<names>]\n"
        let @h=@h."\" :ClearAllBookmarks\n"
        silent! put h
    elseif g:NERDTreeMinimalUI == 0
        let @h="\" Press ". g:NERDTreeMapHelp ." for help\n"
        silent! put h
    endif

    let @h = old_h
endfunction

"FUNCTION: nerdtree#echo  {{{2
"A wrapper for :echo. Appends 'NERDTree:' on the front of all messages
"
"Args:
"msg: the message to echo
function! nerdtree#echo(msg)
    redraw
    echomsg "NERDTree: " . a:msg
endfunction

"FUNCTION: nerdtree#echoError {{{2
"Wrapper for nerdtree#echo, sets the message type to errormsg for this message
"Args:
"msg: the message to echo
function! nerdtree#echoError(msg)
    echohl errormsg
    call nerdtree#echo(a:msg)
    echohl normal
endfunction

"FUNCTION: nerdtree#echoWarning {{{2
"Wrapper for nerdtree#echo, sets the message type to warningmsg for this message
"Args:
"msg: the message to echo
function! nerdtree#echoWarning(msg)
    echohl warningmsg
    call nerdtree#echo(a:msg)
    echohl normal
endfunction

"FUNCTION: nerdtree#getTreeWinNum() {{{2
"gets the nerd tree window number for this tab
function! nerdtree#getTreeWinNum()
    if exists("t:NERDTreeBufName")
        return bufwinnr(t:NERDTreeBufName)
    else
        return -1
    endif
endfunction

"FUNCTION: nerdtree#isTreeOpen() {{{2
function! nerdtree#isTreeOpen()
    return nerdtree#getTreeWinNum() != -1
endfunction

"FUNCTION: nerdtree#putCursorOnBookmarkTable(){{{2
"Places the cursor at the top of the bookmarks table
function! nerdtree#putCursorOnBookmarkTable()
    if !b:NERDTreeShowBookmarks
        throw "NERDTree.IllegalOperationError: cant find bookmark table, bookmarks arent active"
    endif

    if g:NERDTreeMinimalUI
        return cursor(1, 2)
    endif

    let rootNodeLine = b:NERDTree.ui.getRootLineNum()

    let line = 1
    while getline(line) !~# '^>-\+Bookmarks-\+$'
        let line = line + 1
        if line >= rootNodeLine
            throw "NERDTree.BookmarkTableNotFoundError: didnt find the bookmarks table"
        endif
    endwhile
    call cursor(line, 2)
endfunction

"FUNCTION: nerdtree#putCursorInTreeWin(){{{2
"Places the cursor in the nerd tree window
function! nerdtree#putCursorInTreeWin()
    if !nerdtree#isTreeOpen()
        throw "NERDTree.InvalidOperationError: cant put cursor in NERD tree window, no window exists"
    endif

    call nerdtree#exec(nerdtree#getTreeWinNum() . "wincmd w")
endfunction

"FUNCTION: nerdtree#renderBookmarks {{{2
function! nerdtree#renderBookmarks()

    if g:NERDTreeMinimalUI == 0
        call setline(line(".")+1, ">----------Bookmarks----------")
        call cursor(line(".")+1, col("."))
    endif

    for i in g:NERDTreeBookmark.Bookmarks()
        call setline(line(".")+1, i.str())
        call cursor(line(".")+1, col("."))
    endfor

    call setline(line(".")+1, '')
    call cursor(line(".")+1, col("."))
endfunction

"FUNCTION: nerdtree#renderView {{{2
function! nerdtree#renderView()
    call b:NERDTree.render()
endfunction
"
"FUNCTION: nerdtree#stripMarkupFromLine(line, removeLeadingSpaces){{{2
"returns the given line with all the tree parts stripped off
"
"Args:
"line: the subject line
"removeLeadingSpaces: 1 if leading spaces are to be removed (leading spaces =
"any spaces before the actual text of the node)
function! nerdtree#stripMarkupFromLine(line, removeLeadingSpaces)
    let line = a:line
    "remove the tree parts and the leading space
    let line = substitute (line, nerdtree#treeMarkupReg(),"","")

    "strip off any read only flag
    let line = substitute (line, ' \[RO\]', "","")

    "strip off any bookmark flags
    let line = substitute (line, ' {[^}]*}', "","")

    "strip off any executable flags
    let line = substitute (line, '*\ze\($\| \)', "","")

    "strip off any generic flags
    let line = substitute (line, '\[[^]]*\]', "","")

    let wasdir = 0
    if line =~# '/$'
        let wasdir = 1
    endif
    let line = substitute (line,' -> .*',"","") " remove link to
    if wasdir ==# 1
        let line = substitute (line, '/\?$', '/', "")
    endif

    if a:removeLeadingSpaces
        let line = substitute (line, '^ *', '', '')
    endif

    return line
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
