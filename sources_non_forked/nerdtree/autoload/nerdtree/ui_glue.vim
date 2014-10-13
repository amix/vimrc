if exists("g:loaded_nerdtree_ui_glue_autoload")
    finish
endif
let g:loaded_nerdtree_ui_glue_autoload = 1

" FUNCTION: nerdtree#ui_glue#createDefaultBindings() {{{1
function! nerdtree#ui_glue#createDefaultBindings()
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

    call NERDTreeAddKeyMap({ 'key': g:NERDTreeMapCWD, 'scope': "all", 'callback': "nerdtree#ui_glue#chRootCwd" })

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


"SECTION: Interface bindings {{{1
"============================================================

"FUNCTION: s:activateAll() {{{1
"handle the user activating the updir line
function! s:activateAll()
    if getline(".") ==# nerdtree#treeUpDirLine()
        return nerdtree#ui_glue#upDir(0)
    endif
endfunction

"FUNCTION: s:activateDirNode() {{{1
"handle the user activating a tree node
function! s:activateDirNode(node)
    call a:node.activate({'reuse': 1})
endfunction

"FUNCTION: s:activateFileNode() {{{1
"handle the user activating a tree node
function! s:activateFileNode(node)
    call a:node.activate({'reuse': 1, 'where': 'p'})
endfunction

"FUNCTION: s:activateBookmark() {{{1
"handle the user activating a bookmark
function! s:activateBookmark(bm)
    call a:bm.activate(!a:bm.path.isDirectory ? {'where': 'p'} : {})
endfunction

" FUNCTION: nerdtree#ui_glue#bookmarkNode(name) {{{1
" Associate the current node with the given name
function! nerdtree#ui_glue#bookmarkNode(...)
    let currentNode = g:NERDTreeFileNode.GetSelected()
    if currentNode != {}
        let name = a:1
        if empty(name)
            let name = currentNode.path.getLastPathComponent(0)
        endif
        try
            call currentNode.bookmark(name)
            call b:NERDTree.render()
        catch /^NERDTree.IllegalBookmarkNameError/
            call nerdtree#echo("bookmark names must not contain spaces")
        endtry
    else
        call nerdtree#echo("select a node first")
    endif
endfunction

" FUNCTION: s:chCwd(node) {{{1
function! s:chCwd(node)
    try
        call a:node.path.changeToDir()
    catch /^NERDTree.PathChangeError/
        call nerdtree#echoWarning("could not change cwd")
    endtry
endfunction

" FUNCTION: s:chRoot(node) {{{1
" changes the current root to the selected one
function! s:chRoot(node)
    call a:node.makeRoot()
    call b:NERDTree.render()
    call b:NERDTreeRoot.putCursorHere(0, 0)
endfunction

" FUNCTION: s:nerdtree#ui_glue#chRootCwd() {{{1
" changes the current root to CWD
function! nerdtree#ui_glue#chRootCwd()
    try
        let cwd = g:NERDTreePath.New(getcwd())
    catch /^NERDTree.InvalidArgumentsError/
        call nerdtree#echo("current directory does not exist.")
        return
    endtry
    if cwd.str() == g:NERDTreeFileNode.GetRootForTab().path.str()
       return
    endif
    call s:chRoot(g:NERDTreeDirNode.New(cwd))
endfunction

" FUNCTION: nnerdtree#ui_glue#clearBookmarks(bookmarks) {{{1
function! nerdtree#ui_glue#clearBookmarks(bookmarks)
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
    call b:NERDTree.render()
endfunction

" FUNCTION: s:closeChildren(node) {{{1
" closes all childnodes of the current node
function! s:closeChildren(node)
    call a:node.closeChildren()
    call b:NERDTree.render()
    call a:node.putCursorHere(0, 0)
endfunction

" FUNCTION: s:closeCurrentDir(node) {{{1
" closes the parent dir of the current node
function! s:closeCurrentDir(node)
    let parent = a:node.parent
    if parent ==# {} || parent.isRoot()
        call nerdtree#echo("cannot close tree root")
    else
        while g:NERDTreeCascadeOpenSingleChildDir && !parent.parent.isRoot()
            if parent.parent.getVisibleChildCount() == 1
                call parent.close()
                let parent = parent.parent
            else
                break
            endif
        endwhile
        call parent.close()
        call b:NERDTree.render()
        call parent.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: s:closeTreeWindow() {{{1
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

" FUNCTION: s:deleteBookmark(bm) {{{1
" if the cursor is on a bookmark, prompt to delete
function! s:deleteBookmark(bm)
    echo  "Are you sure you wish to delete the bookmark:\n\"" . a:bm.name . "\" (yN):"

    if  nr2char(getchar()) ==# 'y'
        try
            call a:bm.delete()
            call b:NERDTree.render()
            redraw
        catch /^NERDTree/
            call nerdtree#echoWarning("Could not remove bookmark")
        endtry
    else
        call nerdtree#echo("delete aborted" )
    endif

endfunction

" FUNCTION: s:displayHelp() {{{1
" toggles the help display
function! s:displayHelp()
    let b:treeShowHelp = b:treeShowHelp ? 0 : 1
    call b:NERDTree.render()
    call b:NERDTree.ui.centerView()
endfunction

" FUNCTION: s:findAndRevealPath() {{{1
function! s:findAndRevealPath()
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

    if !g:NERDTree.ExistsForTab()
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
            call s:chRoot(g:NERDTreeDirNode.New(p.getParent()))
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

"FUNCTION: s:handleLeftClick() {{{1
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

" FUNCTION: s:handleMiddleMouse() {{{1
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

" FUNCTION: s:jumpToChild(direction) {{{2
" Args:
" direction: 0 if going to first child, 1 if going to last
function! s:jumpToChild(currentNode, direction)
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

    call b:NERDTree.ui.centerView()
endfunction


" FUNCTION: nerdtree#ui_glue#invokeKeyMap(key) {{{1
"this is needed since I cant figure out how to invoke dict functions from a
"key map
function! nerdtree#ui_glue#invokeKeyMap(key)
    call g:NERDTreeKeyMap.Invoke(a:key)
endfunction

" FUNCTION: s:jumpToFirstChild() {{{1
" wrapper for the jump to child method
function! s:jumpToFirstChild(node)
    call s:jumpToChild(a:node, 0)
endfunction

" FUNCTION: s:jumpToLastChild() {{{1
" wrapper for the jump to child method
function! s:jumpToLastChild(node)
    call s:jumpToChild(a:node, 1)
endfunction

" FUNCTION: s:jumpToParent(node) {{{1
" moves the cursor to the parent of the current node
function! s:jumpToParent(node)
    if !empty(a:node.parent)
        call a:node.parent.putCursorHere(1, 0)
        call b:NERDTree.ui.centerView()
    else
        call nerdtree#echo("cannot jump to parent")
    endif
endfunction

" FUNCTION: s:jumpToRoot() {{{1
" moves the cursor to the root node
function! s:jumpToRoot()
    call b:NERDTreeRoot.putCursorHere(1, 0)
    call b:NERDTree.ui.centerView()
endfunction

" FUNCTION: s:jumpToNextSibling(node) {{{1
function! s:jumpToNextSibling(node)
    call s:jumpToSibling(a:node, 1)
endfunction

" FUNCTION: s:jumpToPrevSibling(node) {{{1
function! s:jumpToPrevSibling(node)
    call s:jumpToSibling(a:node, 0)
endfunction

" FUNCTION: s:jumpToSibling(currentNode, forward) {{{2
" moves the cursor to the sibling of the current node in the given direction
"
" Args:
" forward: 1 if the cursor should move to the next sibling, 0 if it should
" move back to the previous sibling
function! s:jumpToSibling(currentNode, forward)
    let sibling = a:currentNode.findSibling(a:forward)

    if !empty(sibling)
        call sibling.putCursorHere(1, 0)
        call b:NERDTree.ui.centerView()
    endif
endfunction

" FUNCTION: nerdtree#ui_glue#openBookmark(name) {{{1
" put the cursor on the given bookmark and, if its a file, open it
function! nerdtree#ui_glue#openBookmark(name)
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

" FUNCTION: s:openHSplit(target) {{{1
function! s:openHSplit(target)
    call a:target.activate({'where': 'h'})
endfunction

" FUNCTION: s:openVSplit(target) {{{1
function! s:openVSplit(target)
    call a:target.activate({'where': 'v'})
endfunction

" FUNCTION: s:openExplorer(node) {{{1
function! s:openExplorer(node)
    call a:node.openExplorer()
endfunction

" FUNCTION: s:openInNewTab(target) {{{1
function! s:openInNewTab(target)
    call a:target.activate({'where': 't'})
endfunction

" FUNCTION: s:openInNewTabSilent(target) {{{1
function! s:openInNewTabSilent(target)
    call a:target.activate({'where': 't', 'stay': 1})
endfunction

" FUNCTION: s:openNodeRecursively(node) {{{1
function! s:openNodeRecursively(node)
    call nerdtree#echo("Recursively opening node. Please wait...")
    call a:node.openRecursively()
    call b:NERDTree.render()
    redraw
    call nerdtree#echo("Recursively opening node. Please wait... DONE")
endfunction

"FUNCTION: s:previewNodeCurrent(node) {{{1
function! s:previewNodeCurrent(node)
    call a:node.open({'stay': 1, 'where': 'p', 'keepopen': 1})
endfunction

"FUNCTION: s:previewNodeHSplit(node) {{{1
function! s:previewNodeHSplit(node)
    call a:node.open({'stay': 1, 'where': 'h', 'keepopen': 1})
endfunction

"FUNCTION: s:previewNodeVSplit(node) {{{1
function! s:previewNodeVSplit(node)
    call a:node.open({'stay': 1, 'where': 'v', 'keepopen': 1})
endfunction

" FUNCTION: nerdtree#ui_glue#revealBookmark(name) {{{1
" put the cursor on the node associate with the given name
function! nerdtree#ui_glue#revealBookmark(name)
    try
        let targetNode = g:NERDTreeBookmark.GetNodeForName(a:name, 0)
        call targetNode.putCursorHere(0, 1)
    catch /^NERDTree.BookmarkNotFoundError/
        call nerdtree#echo("Bookmark isnt cached under the current root")
    endtry
endfunction

" FUNCTION: s:refreshRoot() {{{1
" Reloads the current root. All nodes below this will be lost and the root dir
" will be reloaded.
function! s:refreshRoot()
    call nerdtree#echo("Refreshing the root node. This could take a while...")
    call b:NERDTreeRoot.refresh()
    call b:NERDTree.render()
    redraw
    call nerdtree#echo("Refreshing the root node. This could take a while... DONE")
endfunction

" FUNCTION: s:refreshCurrent(node) {{{1
" refreshes the root for the current node
function! s:refreshCurrent(node)
    let node = a:node
    if !node.path.isDirectory
        let node = node.parent
    endif

    call nerdtree#echo("Refreshing node. This could take a while...")
    call node.refresh()
    call b:NERDTree.render()
    redraw
    call nerdtree#echo("Refreshing node. This could take a while... DONE")
endfunction

" FUNCTION: nerdtree#ui_glue#setupCommands() {{{1
function! nerdtree#ui_glue#setupCommands()
    command! -n=? -complete=dir -bar NERDTree :call g:NERDTreeCreator.CreatePrimary('<args>')
    command! -n=? -complete=dir -bar NERDTreeToggle :call g:NERDTreeCreator.TogglePrimary('<args>')
    command! -n=0 -bar NERDTreeClose :call nerdtree#closeTreeIfOpen()
    command! -n=1 -complete=customlist,nerdtree#completeBookmarks -bar NERDTreeFromBookmark call g:NERDTreeCreator.CreatePrimary('<args>')
    command! -n=0 -bar NERDTreeMirror call g:NERDTreeCreator.CreateMirror()
    command! -n=0 -bar NERDTreeFind call s:findAndRevealPath()
    command! -n=0 -bar NERDTreeFocus call NERDTreeFocus()
    command! -n=0 -bar NERDTreeCWD call NERDTreeCWD()
endfunction

" Function: s:SID()   {{{1
function s:SID()
    if !exists("s:sid")
        let s:sid = matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
    endif
    return s:sid
endfun

" FUNCTION: s:showMenu(node) {{{1
function! s:showMenu(node)
    let mc = g:NERDTreeMenuController.New(g:NERDTreeMenuItem.AllEnabled())
    call mc.showMenu()
endfunction

" FUNCTION: s:toggleIgnoreFilter() {{{1
function! s:toggleIgnoreFilter()
    call b:NERDTree.ui.toggleIgnoreFilter()
endfunction

" FUNCTION: s:toggleShowBookmarks() {{{1
function! s:toggleShowBookmarks()
    call b:NERDTree.ui.toggleShowBookmarks()
endfunction

" FUNCTION: s:toggleShowFiles() {{{1
function! s:toggleShowFiles()
    call b:NERDTree.ui.toggleShowFiles()
endfunction

" FUNCTION: s:toggleShowHidden() {{{1
" toggles the display of hidden files
function! s:toggleShowHidden()
    call b:NERDTree.ui.toggleShowHidden()
endfunction

" FUNCTION: s:toggleZoom() {{{1
function! s:toggleZoom()
    call b:NERDTree.ui.toggleZoom()
endfunction

"FUNCTION: nerdtree#ui_glue#upDir(keepState) {{{1
"moves the tree up a level
"
"Args:
"keepState: 1 if the current root should be left open when the tree is
"re-rendered
function! nerdtree#ui_glue#upDir(keepState)
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

        call b:NERDTree.render()
        call oldRoot.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: s:upDirCurrentRootOpen() {{{1
function! s:upDirCurrentRootOpen()
    call nerdtree#ui_glue#upDir(1)
endfunction

" FUNCTION: s:upDirCurrentRootClosed() {{{1
function! s:upDirCurrentRootClosed()
    call nerdtree#ui_glue#upDir(0)
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
