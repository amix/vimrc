if exists("g:loaded_nerdtree_autoload")
    finish
endif
let g:loaded_nerdtree_autoload = 1

function! nerdtree#version()
    return '4.2.0'
endfunction

" SECTION: General Functions {{{1
"============================================================
"FUNCTION: nerdtree#bufInWindows(bnum){{{2
"[[STOLEN FROM VTREEEXPLORER.VIM]]
"Determine the number of windows open to this buffer number.
"Care of Yegappan Lakshman.  Thanks!
"
"Args:
"bnum: the subject buffers buffer number
function! nerdtree#bufInWindows(bnum)
    let cnt = 0
    let winnum = 1
    while 1
        let bufnum = winbufnr(winnum)
        if bufnum < 0
            break
        endif
        if bufnum ==# a:bnum
            let cnt = cnt + 1
        endif
        let winnum = winnum + 1
    endwhile

    return cnt
endfunction

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

" FUNCTION: nerdtree#createDefaultBindings() {{{2
function! nerdtree#createDefaultBindings()
    let s = '<SNR>' . s:SID() . '_'

    call NERDTreeAddKeyMap({ 'key': '<MiddleRelease>', 'scope': "all", 'callback': s."handleMiddleMouse" })
    call NERDTreeAddKeyMap({ 'key': '<LeftRelease>', 'scope': "all", 'callback': s."handleLeftClick" })
    call NERDTreeAddKeyMap({ 'key': '<2-LeftMouse>', 'scope': "DirNode", 'callback': s."activateDirNode" })
    call NERDTreeAddKeyMap({ 'key': '<2-LeftMouse>', 'scope': "FileNode", 'callback': s."activateFileNode" })
    call NERDTreeAddKeyMap({ 'key': '<2-LeftMouse>', 'scope': "Bookmark", 'callback': s."activateBookmark" })
    call NERDTreeAddKeyMap({ 'key': '<2-LeftMouse>', 'scope': "all", 'callback': s."activateAll" })


    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapActivateNode, 'scope': "DirNode", 'callback': s."activateDirNode" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapActivateNode, 'scope': "FileNode", 'callback': s."activateFileNode" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapActivateNode, 'scope': "Bookmark", 'callback': s."activateBookmark" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapActivateNode, 'scope': "all", 'callback': s."activateAll" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenSplit, 'scope': "Node", 'callback': s."openHSplit" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenVSplit, 'scope': "Node", 'callback': s."openVSplit" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenSplit, 'scope': "Bookmark", 'callback': s."openHSplit" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenVSplit, 'scope': "Bookmark", 'callback': s."openVSplit" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreview, 'scope': "Node", 'callback': s."previewNodeCurrent" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreviewVSplit, 'scope': "Node", 'callback': s."previewNodeVSplit" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreviewSplit, 'scope': "Node", 'callback': s."previewNodeHSplit" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreview, 'scope': "Bookmark", 'callback': s."previewNodeCurrent" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreviewVSplit, 'scope': "Bookmark", 'callback': s."previewNodeVSplit" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapPreviewSplit, 'scope': "Bookmark", 'callback': s."previewNodeHSplit" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenRecursively, 'scope': "DirNode", 'callback': s."openNodeRecursively" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapUpdir, 'scope': "all", 'callback': s."upDirCurrentRootClosed" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapUpdirKeepOpen, 'scope': "all", 'callback': s."upDirCurrentRootOpen" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapChangeRoot, 'scope': "Node", 'callback': s."chRoot" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapChdir, 'scope': "Node", 'callback': s."chCwd" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapQuit, 'scope': "all", 'callback': s."closeTreeWindow" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapCWD, 'scope': "all", 'callback': "nerdtree#chRootCwd" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapRefreshRoot, 'scope': "all", 'callback': s."refreshRoot" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapRefresh, 'scope': "Node", 'callback': s."refreshCurrent" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapHelp, 'scope': "all", 'callback': s."displayHelp" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapToggleZoom, 'scope': "all", 'callback': s."toggleZoom" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapToggleHidden, 'scope': "all", 'callback': s."toggleShowHidden" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapToggleFilters, 'scope': "all", 'callback': s."toggleIgnoreFilter" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapToggleFiles, 'scope': "all", 'callback': s."toggleShowFiles" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapToggleBookmarks, 'scope': "all", 'callback': s."toggleShowBookmarks" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapCloseDir, 'scope': "Node", 'callback': s."closeCurrentDir" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapCloseChildren, 'scope': "DirNode", 'callback': s."closeChildren" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapMenu, 'scope': "Node", 'callback': s."showMenu" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpParent, 'scope': "Node", 'callback': s."jumpToParent" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpFirstChild, 'scope': "Node", 'callback': s."jumpToFirstChild" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpLastChild, 'scope': "Node", 'callback': s."jumpToLastChild" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpRoot, 'scope': "all", 'callback': s."jumpToRoot" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpNextSibling, 'scope': "Node", 'callback': s."jumpToNextSibling" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapJumpPrevSibling, 'scope': "Node", 'callback': s."jumpToPrevSibling" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenInTab, 'scope': "Node", 'callback': s."openInNewTab" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenInTabSilent, 'scope': "Node", 'callback': s."openInNewTabSilent" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenInTab, 'scope': "Bookmark", 'callback': s."openInNewTab" })
    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenInTabSilent, 'scope': "Bookmark", 'callback': s."openInNewTabSilent" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapOpenExpl, 'scope': "DirNode", 'callback': s."openExplorer" })

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapDeleteBookmark, 'scope': "Bookmark", 'callback': s."deleteBookmark" })
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

"FUNCTION: nerdtree#escChars(dir) {{{2
function! nerdtree#escChars()
    if nerdtree#runningWindows()
        return " `\|\"#%&,?()\*^<>"
    endif

    return " \\`\|\"#%&,?()\*^<>[]"
endfunction

" FUNCTION: nerdtree#exec(cmd) {{{2
" same as :exec cmd  but eventignore=all is set for the duration
function! nerdtree#exec(cmd)
    let old_ei = &ei
    set ei=all
    exec a:cmd
    let &ei = old_ei
endfunction

" FUNCTION: nerdtree#findAndRevealPath() {{{2
function! nerdtree#findAndRevealPath()
    try
        let p = g:NERDTreePath.New(expand("%:p"))
    catch /^NERDTree.InvalidArgumentsError/
        call nerdtree#echo("no file for the current buffer")
        return
    endtry

    if p.isUnixHiddenPath()
        let showhidden=g:NERDTreeShowHidden
        let g:NERDTreeShowHidden = 1
    endif

    if !nerdtree#treeExistsForTab()
        try
            let cwd = g:NERDTreePath.New(getcwd())
        catch /^NERDTree.InvalidArgumentsError/
            call nerdtree#echo("current directory does not exist.")
            let cwd = p.getParent()
        endtry

        if p.isUnder(cwd)
            call g:NERDTreeCreator.CreatePrimary(cwd.str())
        else
            call g:NERDTreeCreator.CreatePrimary(p.getParent().str())
        endif
    else
        if !p.isUnder(g:NERDTreeFileNode.GetRootForTab().path)
            if !nerdtree#isTreeOpen()
                call g:NERDTreeCreator.TogglePrimary('')
            else
                call nerdtree#putCursorInTreeWin()
            endif
            let b:NERDTreeShowHidden = g:NERDTreeShowHidden
            call nerdtree#chRoot(g:NERDTreeDirNode.New(p.getParent()))
        else
            if !nerdtree#isTreeOpen()
                call g:NERDTreeCreator.TogglePrimary("")
            endif
        endif
    endif
    call nerdtree#putCursorInTreeWin()
    call b:NERDTreeRoot.reveal(p)

    if p.isUnixHiddenFile()
        let g:NERDTreeShowHidden = showhidden
    endif
endfunction

" FUNCTION: nerdtree#has_opt(options, name) {{{2
function! nerdtree#has_opt(options, name)
    return has_key(a:options, a:name) && a:options[a:name] == 1
endfunction

" FUNCTION: nerdtree#invokeKeyMap(key) {{{2
"this is needed since I cant figure out how to invoke dict functions from a
"key map
function! nerdtree#invokeKeyMap(key)
    call g:NERDTreeKeyMap.Invoke(a:key)
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
endfunction

" FUNCTION: nerdtree#postSourceActions() {{{2
function! nerdtree#postSourceActions()
    call g:NERDTreeBookmark.CacheBookmarks(0)
    call nerdtree#createDefaultBindings()

    "load all nerdtree plugins
    runtime! nerdtree_plugin/**/*.vim
endfunction

"FUNCTION: nerdtree#runningWindows(dir) {{{2
function! nerdtree#runningWindows()
    return has("win16") || has("win32") || has("win64")
endfunction

" Function: s:SID()   {{{2
function s:SID()
    if !exists("s:sid")
        let s:sid = matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
    endif
    return s:sid
endfun

" FUNCTION: nerdtree#tabpagevar(tabnr, var) {{{2
function! nerdtree#tabpagevar(tabnr, var)
    let currentTab = tabpagenr()
    let old_ei = &ei
    set ei=all

    exec "tabnext " . a:tabnr
    let v = -1
    if exists('t:' . a:var)
        exec 'let v = t:' . a:var
    endif
    exec "tabnext " . currentTab

    let &ei = old_ei

    return v
endfunction

" Function: nerdtree#treeExistsForBuffer()   {{{2
" Returns 1 if a nerd tree root exists in the current buffer
function! nerdtree#treeExistsForBuf()
    return exists("b:NERDTreeRoot")
endfunction

" Function: nerdtree#treeExistsForTab()   {{{2
" Returns 1 if a nerd tree root exists in the current tab
function! nerdtree#treeExistsForTab()
    return exists("t:NERDTreeBufName")
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

"FUNCTION: nerdtree#upDir(keepState) {{{2
"moves the tree up a level
"
"Args:
"keepState: 1 if the current root should be left open when the tree is
"re-rendered
function! nerdtree#upDir(keepState)
    let cwd = b:NERDTreeRoot.path.str({'format': 'UI'})
    if cwd ==# "/" || cwd =~# '^[^/]..$'
        call nerdtree#echo("already at top dir")
    else
        if !a:keepState
            call b:NERDTreeRoot.close()
        endif

        let oldRoot = b:NERDTreeRoot

        if empty(b:NERDTreeRoot.parent)
            let path = b:NERDTreeRoot.path.getParent()
            let newRoot = g:NERDTreeDirNode.New(path)
            call newRoot.open()
            call newRoot.transplantChild(b:NERDTreeRoot)
            let b:NERDTreeRoot = newRoot
        else
            let b:NERDTreeRoot = b:NERDTreeRoot.parent
        endif

        if g:NERDTreeChDirMode ==# 2
            call b:NERDTreeRoot.path.changeToDir()
        endif

        call nerdtree#renderView()
        call oldRoot.putCursorHere(0, 0)
    endif
endfunction

" Function: nerdtree#unique(list)   {{{2
" returns a:list without duplicates
function! nerdtree#unique(list)
  let uniqlist = []
  for elem in a:list
    if index(uniqlist, elem) ==# -1
      let uniqlist += [elem]
    endif
  endfor
  return uniqlist
endfunction

" SECTION: View Functions {{{1
"============================================================
"
"FUNCTION: nerdtree#centerView() {{{2
"centers the nerd tree window around the cursor (provided the nerd tree
"options permit)
function! nerdtree#centerView()
    if g:NERDTreeAutoCenter
        let current_line = winline()
        let lines_to_top = current_line
        let lines_to_bottom = winheight(nerdtree#getTreeWinNum()) - current_line
        if lines_to_top < g:NERDTreeAutoCenterThreshold || lines_to_bottom < g:NERDTreeAutoCenterThreshold
            normal! zz
        endif
    endif
endfunction

" FUNCTION: nerdtree#chRoot(node) {{{2
" changes the current root to the selected one
function! nerdtree#chRoot(node)
    call s:chRoot(a:node)
endfunction
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
        let @h=@h."\" :Bookmark <name>\n"
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

"FUNCTION: nerdtree#firstUsableWindow(){{{2
"find the window number of the first normal window
function! nerdtree#firstUsableWindow()
    let i = 1
    while i <= winnr("$")
        let bnum = winbufnr(i)
        if bnum != -1 && getbufvar(bnum, '&buftype') ==# ''
                    \ && !getwinvar(i, '&previewwindow')
                    \ && (!getbufvar(bnum, '&modified') || &hidden)
            return i
        endif

        let i += 1
    endwhile
    return -1
endfunction

"FUNCTION: nerdtree#getPath(ln) {{{2
"Gets the full path to the node that is rendered on the given line number
"
"Args:
"ln: the line number to get the path for
"
"Return:
"A path if a node was selected, {} if nothing is selected.
"If the 'up a dir' line was selected then the path to the parent of the
"current root is returned
function! nerdtree#getPath(ln)
    let line = getline(a:ln)

    let rootLine = g:NERDTreeFileNode.GetRootLineNum()

    "check to see if we have the root node
    if a:ln == rootLine
        return b:NERDTreeRoot.path
    endif

    if !g:NERDTreeDirArrows
        " in case called from outside the tree
        if line !~# '^ *[|`▸▾ ]' || line =~# '^$'
            return {}
        endif
    endif

    if line ==# nerdtree#treeUpDirLine()
        return b:NERDTreeRoot.path.getParent()
    endif

    let indent = nerdtree#indentLevelFor(line)

    "remove the tree parts and the leading space
    let curFile = nerdtree#stripMarkupFromLine(line, 0)

    let wasdir = 0
    if curFile =~# '/$'
        let wasdir = 1
        let curFile = substitute(curFile, '/\?$', '/', "")
    endif

    let dir = ""
    let lnum = a:ln
    while lnum > 0
        let lnum = lnum - 1
        let curLine = getline(lnum)
        let curLineStripped = nerdtree#stripMarkupFromLine(curLine, 1)

        "have we reached the top of the tree?
        if lnum == rootLine
            let dir = b:NERDTreeRoot.path.str({'format': 'UI'}) . dir
            break
        endif
        if curLineStripped =~# '/$'
            let lpindent = nerdtree#indentLevelFor(curLine)
            if lpindent < indent
                let indent = indent - 1

                let dir = substitute (curLineStripped,'^\\', "", "") . dir
                continue
            endif
        endif
    endwhile
    let curFile = b:NERDTreeRoot.path.drive . dir . curFile
    let toReturn = g:NERDTreePath.New(curFile)
    return toReturn
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

"FUNCTION: nerdtree#indentLevelFor(line) {{{2
function! nerdtree#indentLevelFor(line)
    let level = match(a:line, '[^ \-+~▸▾`|]') / nerdtree#treeWid()
    " check if line includes arrows
    if match(a:line, '[▸▾]') > -1
        " decrement level as arrow uses 3 ascii chars
        let level = level - 1
    endif
    return level
endfunction

"FUNCTION: nerdtree#isTreeOpen() {{{2
function! nerdtree#isTreeOpen()
    return nerdtree#getTreeWinNum() != -1
endfunction

"FUNCTION: nerdtree#isWindowUsable(winnumber) {{{2
"Returns 0 if opening a file from the tree in the given window requires it to
"be split, 1 otherwise
"
"Args:
"winnumber: the number of the window in question
function! nerdtree#isWindowUsable(winnumber)
    "gotta split if theres only one window (i.e. the NERD tree)
    if winnr("$") ==# 1
        return 0
    endif

    let oldwinnr = winnr()
    call nerdtree#exec(a:winnumber . "wincmd p")
    let specialWindow = getbufvar("%", '&buftype') != '' || getwinvar('%', '&previewwindow')
    let modified = &modified
    call nerdtree#exec(oldwinnr . "wincmd p")

    "if its a special window e.g. quickfix or another explorer plugin then we
    "have to split
    if specialWindow
        return 0
    endif

    if &hidden
        return 1
    endif

    return !modified || nerdtree#bufInWindows(winbufnr(a:winnumber)) >= 2
endfunction

" FUNCTION: nerdtree#jumpToChild(direction) {{{2
" Args:
" direction: 0 if going to first child, 1 if going to last
function! nerdtree#jumpToChild(currentNode, direction)
    if a:currentNode.isRoot()
        return nerdtree#echo("cannot jump to " . (a:direction ? "last" : "first") .  " child")
    end
    let dirNode = a:currentNode.parent
    let childNodes = dirNode.getVisibleChildren()

    let targetNode = childNodes[0]
    if a:direction
        let targetNode = childNodes[len(childNodes) - 1]
    endif

    if targetNode.equals(a:currentNode)
        let siblingDir = a:currentNode.parent.findOpenDirSiblingWithVisibleChildren(a:direction)
        if siblingDir != {}
            let indx = a:direction ? siblingDir.getVisibleChildCount()-1 : 0
            let targetNode = siblingDir.getChildByIndex(indx, 1)
        endif
    endif

    call targetNode.putCursorHere(1, 0)

    call nerdtree#centerView()
endfunction

" FUNCTION: nerdtree#jumpToSibling(currentNode, forward) {{{2
" moves the cursor to the sibling of the current node in the given direction
"
" Args:
" forward: 1 if the cursor should move to the next sibling, 0 if it should
" move back to the previous sibling
function! nerdtree#jumpToSibling(currentNode, forward)
    let sibling = a:currentNode.findSibling(a:forward)

    if !empty(sibling)
        call sibling.putCursorHere(1, 0)
        call nerdtree#centerView()
    endif
endfunction

"FUNCTION: nerdtree#promptToDelBuffer(bufnum, msg){{{2
"prints out the given msg and, if the user responds by pushing 'y' then the
"buffer with the given bufnum is deleted
"
"Args:
"bufnum: the buffer that may be deleted
"msg: a message that will be echoed to the user asking them if they wish to
"     del the buffer
function! nerdtree#promptToDelBuffer(bufnum, msg)
    echo a:msg
    if nr2char(getchar()) ==# 'y'
        exec "silent bdelete! " . a:bufnum
    endif
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

    let rootNodeLine = g:NERDTreeFileNode.GetRootLineNum()

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
"The entry function for rendering the tree
function! nerdtree#renderView()
    setlocal modifiable

    "remember the top line of the buffer and the current line so we can
    "restore the view exactly how it was
    let curLine = line(".")
    let curCol = col(".")
    let topLine = line("w0")

    "delete all lines in the buffer (being careful not to clobber a register)
    silent 1,$delete _

    call nerdtree#dumpHelp()

    "delete the blank line before the help and add one after it
    if g:NERDTreeMinimalUI == 0
        call setline(line(".")+1, "")
        call cursor(line(".")+1, col("."))
    endif

    if b:NERDTreeShowBookmarks
        call nerdtree#renderBookmarks()
    endif

    "add the 'up a dir' line
    if !g:NERDTreeMinimalUI
        call setline(line(".")+1, nerdtree#treeUpDirLine())
        call cursor(line(".")+1, col("."))
    endif

    "draw the header line
    let header = b:NERDTreeRoot.path.str({'format': 'UI', 'truncateTo': winwidth(0)})
    call setline(line(".")+1, header)
    call cursor(line(".")+1, col("."))

    "draw the tree
    let old_o = @o
    let @o = b:NERDTreeRoot.renderToString()
    silent put o
    let @o = old_o

    "delete the blank line at the top of the buffer
    silent 1,1delete _

    "restore the view
    let old_scrolloff=&scrolloff
    let &scrolloff=0
    call cursor(topLine, 1)
    normal! zt
    call cursor(curLine, curCol)
    let &scrolloff = old_scrolloff

    setlocal nomodifiable
endfunction

"FUNCTION: nerdtree#renderViewSavingPosition {{{2
"Renders the tree and ensures the cursor stays on the current node or the
"current nodes parent if it is no longer available upon re-rendering
function! nerdtree#renderViewSavingPosition()
    let currentNode = g:NERDTreeFileNode.GetSelected()

    "go up the tree till we find a node that will be visible or till we run
    "out of nodes
    while currentNode != {} && !currentNode.isVisible() && !currentNode.isRoot()
        let currentNode = currentNode.parent
    endwhile

    call nerdtree#renderView()

    if currentNode != {}
        call currentNode.putCursorHere(0, 0)
    endif
endfunction
"
"FUNCTION: nerdtree#restoreScreenState() {{{2
"
"Sets the screen state back to what it was when nerdtree#saveScreenState was last
"called.
"
"Assumes the cursor is in the NERDTree window
function! nerdtree#restoreScreenState()
    if !exists("b:NERDTreeOldTopLine") || !exists("b:NERDTreeOldPos") || !exists("b:NERDTreeOldWindowSize")
        return
    endif
    exec("silent vertical resize ".b:NERDTreeOldWindowSize)

    let old_scrolloff=&scrolloff
    let &scrolloff=0
    call cursor(b:NERDTreeOldTopLine, 0)
    normal! zt
    call setpos(".", b:NERDTreeOldPos)
    let &scrolloff=old_scrolloff
endfunction

"FUNCTION: nerdtree#saveScreenState() {{{2
"Saves the current cursor position in the current buffer and the window
"scroll position
function! nerdtree#saveScreenState()
    let win = winnr()
    try
        call nerdtree#putCursorInTreeWin()
        let b:NERDTreeOldPos = getpos(".")
        let b:NERDTreeOldTopLine = line("w0")
        let b:NERDTreeOldWindowSize = winwidth("")
        call nerdtree#exec(win . "wincmd w")
    catch /^NERDTree.InvalidOperationError/
    endtry
endfunction

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

"SECTION: Interface bindings {{{1
"============================================================

"FUNCTION: s:activateAll() {{{2
"handle the user activating the updir line
function! s:activateAll()
    if getline(".") ==# nerdtree#treeUpDirLine()
        return nerdtree#upDir(0)
    endif
endfunction
"FUNCTION: s:activateDirNode() {{{2
"handle the user activating a tree node
function! s:activateDirNode(node)
    call a:node.activate({'reuse': 1})
endfunction

"FUNCTION: s:activateFileNode() {{{2
"handle the user activating a tree node
function! s:activateFileNode(node)
    call a:node.activate({'reuse': 1, 'where': 'p'})
endfunction

"FUNCTION: s:activateBookmark() {{{2
"handle the user activating a bookmark
function! s:activateBookmark(bm)
    call a:bm.activate(!a:bm.path.isDirectory ? {'where': 'p'} : {})
endfunction

" FUNCTION: nerdtree#bookmarkNode(name) {{{2
" Associate the current node with the given name
function! nerdtree#bookmarkNode(...)
    let currentNode = g:NERDTreeFileNode.GetSelected()
    if currentNode != {}
        let name = a:1
        if empty(name)
            let name = currentNode.path.getLastPathComponent(0)
        endif
        try
            call currentNode.bookmark(name)
            call nerdtree#renderView()
        catch /^NERDTree.IllegalBookmarkNameError/
            call nerdtree#echo("bookmark names must not contain spaces")
        endtry
    else
        call nerdtree#echo("select a node first")
    endif
endfunction

" FUNCTION: s:chCwd(node) {{{2
function! s:chCwd(node)
    try
        call a:node.path.changeToDir()
    catch /^NERDTree.PathChangeError/
        call nerdtree#echoWarning("could not change cwd")
    endtry
endfunction

" FUNCTION: s:chRoot(node) {{{2
" changes the current root to the selected one
function! s:chRoot(node)
    call a:node.makeRoot()
    call nerdtree#renderView()
    call b:NERDTreeRoot.putCursorHere(0, 0)
endfunction

" FUNCTION: s:chRootCwd() {{{2
" changes the current root to CWD
function! nerdtree#chRootCwd()
    try
        let cwd = g:NERDTreePath.New(getcwd())
    catch /^NERDTree.InvalidArgumentsError/
        call nerdtree#echo("current directory does not exist.")
        return
    endtry
    if cwd.str() == g:NERDTreeFileNode.GetRootForTab().path.str()
       return
    endif
    call nerdtree#chRoot(g:NERDTreeDirNode.New(cwd))
endfunction

" FUNCTION: nerdtree#clearBookmarks(bookmarks) {{{2
function! nerdtree#clearBookmarks(bookmarks)
    if a:bookmarks ==# ''
        let currentNode = g:NERDTreeFileNode.GetSelected()
        if currentNode != {}
            call currentNode.clearBookmarks()
        endif
    else
        for name in split(a:bookmarks, ' ')
            let bookmark = g:NERDTreeBookmark.BookmarkFor(name)
            call bookmark.delete()
        endfor
    endif
    call nerdtree#renderView()
endfunction

" FUNCTION: s:closeChildren(node) {{{2
" closes all childnodes of the current node
function! s:closeChildren(node)
    call a:node.closeChildren()
    call nerdtree#renderView()
    call a:node.putCursorHere(0, 0)
endfunction

" FUNCTION: s:closeCurrentDir(node) {{{2
" closes the parent dir of the current node
function! s:closeCurrentDir(node)
    let parent = a:node.parent
    if parent ==# {} || parent.isRoot()
        call nerdtree#echo("cannot close tree root")
    else
        call a:node.parent.close()
        call nerdtree#renderView()
        call a:node.parent.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: s:closeTreeWindow() {{{2
" close the tree window
function! s:closeTreeWindow()
    if b:NERDTreeType ==# "secondary" && b:NERDTreePreviousBuf != -1
        exec "buffer " . b:NERDTreePreviousBuf
    else
        if winnr("$") > 1
            call nerdtree#closeTree()
        else
            call nerdtree#echo("Cannot close last window")
        endif
    endif
endfunction

" FUNCTION: s:deleteBookmark(bm) {{{2
" if the cursor is on a bookmark, prompt to delete
function! s:deleteBookmark(bm)
    echo  "Are you sure you wish to delete the bookmark:\n\"" . a:bm.name . "\" (yN):"

    if  nr2char(getchar()) ==# 'y'
        try
            call a:bm.delete()
            call nerdtree#renderView()
            redraw
        catch /^NERDTree/
            call nerdtree#echoWarning("Could not remove bookmark")
        endtry
    else
        call nerdtree#echo("delete aborted" )
    endif

endfunction

" FUNCTION: s:displayHelp() {{{2
" toggles the help display
function! s:displayHelp()
    let b:treeShowHelp = b:treeShowHelp ? 0 : 1
    call nerdtree#renderView()
    call nerdtree#centerView()
endfunction

"FUNCTION: s:handleLeftClick() {{{2
"Checks if the click should open the current node
function! s:handleLeftClick()
    let currentNode = g:NERDTreeFileNode.GetSelected()
    if currentNode != {}

        "the dir arrows are multibyte chars, and vim's string functions only
        "deal with single bytes - so split the line up with the hack below and
        "take the line substring manually
        let line = split(getline(line(".")), '\zs')
        let startToCur = ""
        for i in range(0,len(line)-1)
            let startToCur .= line[i]
        endfor

        if currentNode.path.isDirectory
            if startToCur =~# nerdtree#treeMarkupReg() && startToCur =~# '[+~▾▸] \?$'
                call currentNode.activate()
                return
            endif
        endif

        if (g:NERDTreeMouseMode ==# 2 && currentNode.path.isDirectory) || g:NERDTreeMouseMode ==# 3
            let char = strpart(startToCur, strlen(startToCur)-1, 1)
            if char !~# nerdtree#treeMarkupReg()
                if currentNode.path.isDirectory
                    call currentNode.activate()
                else
                    call currentNode.activate({'reuse': 1, 'where': 'p'})
                endif
                return
            endif
        endif
    endif
endfunction

" FUNCTION: s:handleMiddleMouse() {{{2
function! s:handleMiddleMouse()
    let curNode = g:NERDTreeFileNode.GetSelected()
    if curNode ==# {}
        call nerdtree#echo("Put the cursor on a node first" )
        return
    endif

    if curNode.path.isDirectory
        call nerdtree#openExplorer(curNode)
    else
        call curNode.open({'where': 'h'})
    endif
endfunction

" FUNCTION: s:jumpToFirstChild() {{{2
" wrapper for the jump to child method
function! s:jumpToFirstChild(node)
    call nerdtree#jumpToChild(a:node, 0)
endfunction

" FUNCTION: s:jumpToLastChild() {{{2
" wrapper for the jump to child method
function! s:jumpToLastChild(node)
    call nerdtree#jumpToChild(a:node, 1)
endfunction

" FUNCTION: s:jumpToParent(node) {{{2
" moves the cursor to the parent of the current node
function! s:jumpToParent(node)
    if !empty(a:node.parent)
        call a:node.parent.putCursorHere(1, 0)
        call nerdtree#centerView()
    else
        call nerdtree#echo("cannot jump to parent")
    endif
endfunction

" FUNCTION: s:jumpToRoot() {{{2
" moves the cursor to the root node
function! s:jumpToRoot()
    call b:NERDTreeRoot.putCursorHere(1, 0)
    call nerdtree#centerView()
endfunction

" FUNCTION: s:jumpToNextSibling(node) {{{2
function! s:jumpToNextSibling(node)
    call nerdtree#jumpToSibling(a:node, 1)
endfunction

" FUNCTION: s:jumpToPrevSibling(node) {{{2
function! s:jumpToPrevSibling(node)
    call nerdtree#jumpToSibling(a:node, 0)
endfunction

" FUNCTION: nerdtree#openBookmark(name) {{{2
" put the cursor on the given bookmark and, if its a file, open it
function! nerdtree#openBookmark(name)
    try
        let targetNode = g:NERDTreeBookmark.GetNodeForName(a:name, 0)
        call targetNode.putCursorHere(0, 1)
        redraw!
    catch /^NERDTree.BookmarkedNodeNotFoundError/
        call nerdtree#echo("note - target node is not cached")
        let bookmark = g:NERDTreeBookmark.BookmarkFor(a:name)
        let targetNode = g:NERDTreeFileNode.New(bookmark.path)
    endtry
    if targetNode.path.isDirectory
        call targetNode.openExplorer()
    else
        call targetNode.open({'where': 'p'})
    endif
endfunction

" FUNCTION: s:openHSplit(target) {{{2
function! s:openHSplit(target)
    call a:target.activate({'where': 'h'})
endfunction

" FUNCTION: s:openVSplit(target) {{{2
function! s:openVSplit(target)
    call a:target.activate({'where': 'v'})
endfunction

" FUNCTION: s:openExplorer(node) {{{2
function! s:openExplorer(node)
    call a:node.openExplorer()
endfunction

" FUNCTION: s:openInNewTab(target) {{{2
function! s:openInNewTab(target)
    call a:target.activate({'where': 't'})
endfunction

" FUNCTION: s:openInNewTabSilent(target) {{{2
function! s:openInNewTabSilent(target)
    call a:target.activate({'where': 't', 'stay': 1})
endfunction

" FUNCTION: s:openNodeRecursively(node) {{{2
function! s:openNodeRecursively(node)
    call nerdtree#echo("Recursively opening node. Please wait...")
    call a:node.openRecursively()
    call nerdtree#renderView()
    redraw
    call nerdtree#echo("Recursively opening node. Please wait... DONE")
endfunction

"FUNCTION: s:previewNodeCurrent(node) {{{2
function! s:previewNodeCurrent(node)
    call a:node.open({'stay': 1, 'where': 'p', 'keepopen': 1})
endfunction

"FUNCTION: s:previewNodeHSplit(node) {{{2
function! s:previewNodeHSplit(node)
    call a:node.open({'stay': 1, 'where': 'h', 'keepopen': 1})
endfunction

"FUNCTION: s:previewNodeVSplit(node) {{{2
function! s:previewNodeVSplit(node)
    call a:node.open({'stay': 1, 'where': 'v', 'keepopen': 1})
endfunction

" FUNCTION: nerdtree#revealBookmark(name) {{{2
" put the cursor on the node associate with the given name
function! nerdtree#revealBookmark(name)
    try
        let targetNode = g:NERDTreeBookmark.GetNodeForName(a:name, 0)
        call targetNode.putCursorHere(0, 1)
    catch /^NERDTree.BookmarkNotFoundError/
        call nerdtree#echo("Bookmark isnt cached under the current root")
    endtry
endfunction

" FUNCTION: s:refreshRoot() {{{2
" Reloads the current root. All nodes below this will be lost and the root dir
" will be reloaded.
function! s:refreshRoot()
    call nerdtree#echo("Refreshing the root node. This could take a while...")
    call b:NERDTreeRoot.refresh()
    call nerdtree#renderView()
    redraw
    call nerdtree#echo("Refreshing the root node. This could take a while... DONE")
endfunction

" FUNCTION: s:refreshCurrent(node) {{{2
" refreshes the root for the current node
function! s:refreshCurrent(node)
    let node = a:node
    if !node.path.isDirectory
        let node = node.parent
    endif

    call nerdtree#echo("Refreshing node. This could take a while...")
    call node.refresh()
    call nerdtree#renderView()
    redraw
    call nerdtree#echo("Refreshing node. This could take a while... DONE")
endfunction

" FUNCTION: s:showMenu(node) {{{2
function! s:showMenu(node)
    let mc = g:NERDTreeMenuController.New(g:NERDTreeMenuItem.AllEnabled())
    call mc.showMenu()
endfunction

" FUNCTION: s:toggleIgnoreFilter() {{{2
" toggles the use of the NERDTreeIgnore option
function! s:toggleIgnoreFilter()
    let b:NERDTreeIgnoreEnabled = !b:NERDTreeIgnoreEnabled
    call nerdtree#renderViewSavingPosition()
    call nerdtree#centerView()
endfunction

" FUNCTION: s:toggleShowBookmarks() {{{2
" toggles the display of bookmarks
function! s:toggleShowBookmarks()
    let b:NERDTreeShowBookmarks = !b:NERDTreeShowBookmarks
    if b:NERDTreeShowBookmarks
        call nerdtree#renderView()
        call nerdtree#putCursorOnBookmarkTable()
    else
        call nerdtree#renderViewSavingPosition()
    endif
    call nerdtree#centerView()
endfunction

" FUNCTION: s:toggleShowFiles() {{{2
" toggles the display of hidden files
function! s:toggleShowFiles()
    let b:NERDTreeShowFiles = !b:NERDTreeShowFiles
    call nerdtree#renderViewSavingPosition()
    call nerdtree#centerView()
endfunction

" FUNCTION: s:toggleShowHidden() {{{2
" toggles the display of hidden files
function! s:toggleShowHidden()
    let b:NERDTreeShowHidden = !b:NERDTreeShowHidden
    call nerdtree#renderViewSavingPosition()
    call nerdtree#centerView()
endfunction

" FUNCTION: s:toggleZoom() {{{2
" zoom (maximize/minimize) the NERDTree window
function! s:toggleZoom()
    if exists("b:NERDTreeZoomed") && b:NERDTreeZoomed
        let size = exists("b:NERDTreeOldWindowSize") ? b:NERDTreeOldWindowSize : g:NERDTreeWinSize
        exec "silent vertical resize ". size
        let b:NERDTreeZoomed = 0
    else
        exec "vertical resize"
        let b:NERDTreeZoomed = 1
    endif
endfunction

" FUNCTION: s:upDirCurrentRootOpen() {{{2
function! s:upDirCurrentRootOpen()
    call nerdtree#upDir(1)
endfunction

" FUNCTION: s:upDirCurrentRootClosed() {{{2
function! s:upDirCurrentRootClosed()
    call nerdtree#upDir(0)
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
