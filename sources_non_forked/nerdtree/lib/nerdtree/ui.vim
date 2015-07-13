"CLASS: UI
"============================================================
let s:UI = {}
let g:NERDTreeUI = s:UI

"FUNCTION: s:UI.centerView() {{{2
"centers the nerd tree window around the cursor (provided the nerd tree
"options permit)
function! s:UI.centerView()
    if g:NERDTreeAutoCenter
        let current_line = winline()
        let lines_to_top = current_line
        let lines_to_bottom = winheight(g:NERDTree.GetWinNum()) - current_line
        if lines_to_top < g:NERDTreeAutoCenterThreshold || lines_to_bottom < g:NERDTreeAutoCenterThreshold
            normal! zz
        endif
    endif
endfunction

"FUNCTION: s:UI._dumpHelp  {{{1
"prints out the quick help
function! s:UI._dumpHelp()
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


"FUNCTION: s:UI.new(nerdtree) {{{1
function! s:UI.New(nerdtree)
    let newObj = copy(self)
    let newObj.nerdtree = a:nerdtree
    return newObj
endfunction

"FUNCTION: s:UI.getPath(ln) {{{1
"Gets the full path to the node that is rendered on the given line number
"
"Args:
"ln: the line number to get the path for
"
"Return:
"A path if a node was selected, {} if nothing is selected.
"If the 'up a dir' line was selected then the path to the parent of the
"current root is returned
function! s:UI.getPath(ln)
    let line = getline(a:ln)

    let rootLine = self.getRootLineNum()

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

    if line ==# s:UI.UpDirLine()
        return b:NERDTreeRoot.path.getParent()
    endif

    let indent = self._indentLevelFor(line)

    "remove the tree parts and the leading space
    let curFile = self._stripMarkup(line, 0)

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
        let curLineStripped = self._stripMarkup(curLine, 1)

        "have we reached the top of the tree?
        if lnum == rootLine
            let dir = b:NERDTreeRoot.path.str({'format': 'UI'}) . dir
            break
        endif
        if curLineStripped =~# '/$'
            let lpindent = self._indentLevelFor(curLine)
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

"FUNCTION: s:UI.getLineNum(file_node){{{1
"returns the line number this node is rendered on, or -1 if it isnt rendered
function! s:UI.getLineNum(file_node)
    "if the node is the root then return the root line no.
    if a:file_node.isRoot()
        return b:NERDTree.ui.getRootLineNum()
    endif

    let totalLines = line("$")

    "the path components we have matched so far
    let pathcomponents = [substitute(b:NERDTreeRoot.path.str({'format': 'UI'}), '/ *$', '', '')]
    "the index of the component we are searching for
    let curPathComponent = 1

    let fullpath = a:file_node.path.str({'format': 'UI'})

    let lnum = b:NERDTree.ui.getRootLineNum()
    while lnum > 0
        let lnum = lnum + 1
        "have we reached the bottom of the tree?
        if lnum ==# totalLines+1
            return -1
        endif

        let curLine = getline(lnum)

        let indent = self._indentLevelFor(curLine)
        if indent ==# curPathComponent
            let curLine = self._stripMarkup(curLine, 1)

            let curPath =  join(pathcomponents, '/') . '/' . curLine
            if stridx(fullpath, curPath, 0) ==# 0
                if fullpath ==# curPath || strpart(fullpath, len(curPath)-1,1) ==# '/'
                    let curLine = substitute(curLine, '/ *$', '', '')
                    call add(pathcomponents, curLine)
                    let curPathComponent = curPathComponent + 1

                    if fullpath ==# curPath
                        return lnum
                    endif
                endif
            endif
        endif
    endwhile
    return -1
endfunction

"FUNCTION: s:UI.getRootLineNum(){{{1
"gets the line number of the root node
function! s:UI.getRootLineNum()
    let rootLine = 1
    while getline(rootLine) !~# '^\(/\|<\)'
        let rootLine = rootLine + 1
    endwhile
    return rootLine
endfunction

"FUNCTION: s:UI._indentLevelFor(line) {{{1
function! s:UI._indentLevelFor(line)
    let level = match(a:line, '[^ \-+~▸▾`|]') / s:UI.IndentWid()
    " check if line includes arrows
    if match(a:line, '[▸▾]') > -1
        " decrement level as arrow uses 3 ascii chars
        let level = level - 1
    endif
    return level
endfunction

"FUNCTION: s:UI.IndentWid() {{{1
function! s:UI.IndentWid()
    return 2
endfunction

"FUNCTION: s:UI.MarkupReg() {{{1
function! s:UI.MarkupReg()
    if g:NERDTreeDirArrows
        return '^\([▾▸] \| \+[▾▸] \| \+\)'
    endif

    return '^[ `|]*[\-+~]'
endfunction

"FUNCTION: s:UI._renderBookmarks {{{1
function! s:UI._renderBookmarks()

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

"FUNCTION: s:UI.restoreScreenState() {{{1
"
"Sets the screen state back to what it was when nerdtree#saveScreenState was last
"called.
"
"Assumes the cursor is in the NERDTree window
function! s:UI.restoreScreenState()
    if !has_key(self, '_screenState')
        return
    endif
    exec("silent vertical resize " . self._screenState['oldWindowSize'])

    let old_scrolloff=&scrolloff
    let &scrolloff=0
    call cursor(self._screenState['oldTopLine'], 0)
    normal! zt
    call setpos(".", self._screenState['oldPos'])
    let &scrolloff=old_scrolloff
endfunction

"FUNCTION: s:UI.saveScreenState() {{{1
"Saves the current cursor position in the current buffer and the window
"scroll position
function! s:UI.saveScreenState()
    let win = winnr()
    call g:NERDTree.CursorToTreeWin()
    let self._screenState = {}
    let self._screenState['oldPos'] = getpos(".")
    let self._screenState['oldTopLine'] = line("w0")
    let self._screenState['oldWindowSize']= winwidth("")
    call nerdtree#exec(win . "wincmd w")
endfunction

"FUNCTION: s:UI._stripMarkup(line, removeLeadingSpaces){{{1
"returns the given line with all the tree parts stripped off
"
"Args:
"line: the subject line
"removeLeadingSpaces: 1 if leading spaces are to be removed (leading spaces =
"any spaces before the actual text of the node)
function! s:UI._stripMarkup(line, removeLeadingSpaces)
    let line = a:line
    "remove the tree parts and the leading space
    let line = substitute (line, g:NERDTreeUI.MarkupReg(),"","")

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

"FUNCTION: s:UI.render() {{{1
function! s:UI.render()
    setlocal modifiable

    "remember the top line of the buffer and the current line so we can
    "restore the view exactly how it was
    let curLine = line(".")
    let curCol = col(".")
    let topLine = line("w0")

    "delete all lines in the buffer (being careful not to clobber a register)
    silent 1,$delete _

    call self._dumpHelp()

    "delete the blank line before the help and add one after it
    if g:NERDTreeMinimalUI == 0
        call setline(line(".")+1, "")
        call cursor(line(".")+1, col("."))
    endif

    if b:NERDTreeShowBookmarks
        call self._renderBookmarks()
    endif

    "add the 'up a dir' line
    if !g:NERDTreeMinimalUI
        call setline(line(".")+1, s:UI.UpDirLine())
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


"FUNCTION: UI.renderViewSavingPosition {{{1
"Renders the tree and ensures the cursor stays on the current node or the
"current nodes parent if it is no longer available upon re-rendering
function! s:UI.renderViewSavingPosition()
    let currentNode = g:NERDTreeFileNode.GetSelected()

    "go up the tree till we find a node that will be visible or till we run
    "out of nodes
    while currentNode != {} && !currentNode.isVisible() && !currentNode.isRoot()
        let currentNode = currentNode.parent
    endwhile

    call b:NERDTree.render()

    if currentNode != {}
        call currentNode.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: s:UI.toggleIgnoreFilter() {{{1
" toggles the use of the NERDTreeIgnore option
function! s:UI.toggleIgnoreFilter()
    let b:NERDTreeIgnoreEnabled = !b:NERDTreeIgnoreEnabled
    call b:NERDTree.ui.renderViewSavingPosition()
    call b:NERDTree.ui.centerView()
endfunction

" FUNCTION: s:UI.toggleShowBookmarks() {{{1
" toggles the display of bookmarks
function! s:UI.toggleShowBookmarks()
    let b:NERDTreeShowBookmarks = !b:NERDTreeShowBookmarks
    if b:NERDTreeShowBookmarks
        call b:NERDTree.render()
        call g:NERDTree.CursorToBookmarkTable()
    else
        call b:NERDTree.ui.renderViewSavingPosition()
    endif
    call b:NERDTree.ui.centerView()
endfunction

" FUNCTION: s:UI.toggleShowFiles() {{{1
" toggles the display of hidden files
function! s:UI.toggleShowFiles()
    let b:NERDTreeShowFiles = !b:NERDTreeShowFiles
    call b:NERDTree.ui.renderViewSavingPosition()
    call b:NERDTree.ui.centerView()
endfunction

" FUNCTION: s:UI.toggleShowHidden() {{{1
" toggles the display of hidden files
function! s:UI.toggleShowHidden()
    let b:NERDTreeShowHidden = !b:NERDTreeShowHidden
    call b:NERDTree.ui.renderViewSavingPosition()
    call self.centerView()
endfunction

" FUNCTION: s:UI.toggleZoom() {{{1
" zoom (maximize/minimize) the NERDTree window
function! s:UI.toggleZoom()
    if exists("b:NERDTreeZoomed") && b:NERDTreeZoomed
        let size = exists("b:NERDTreeOldWindowSize") ? b:NERDTreeOldWindowSize : g:NERDTreeWinSize
        exec "silent vertical resize ". size
        let b:NERDTreeZoomed = 0
    else
        exec "vertical resize"
        let b:NERDTreeZoomed = 1
    endif
endfunction

"FUNCTION: s:UI.UpDirLine() {{{1
function! s:UI.UpDirLine()
    return '.. (up a dir)'
endfunction
