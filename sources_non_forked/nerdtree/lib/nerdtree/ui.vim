"CLASS: UI
"============================================================
let s:UI = {}
let g:NERDTreeUI = s:UI


function! s:UI.lolcats()
    echomsg "lolcats"
endfunction

"FUNCTION: s:UI.centerView() {{{2
"centers the nerd tree window around the cursor (provided the nerd tree
"options permit)
function! s:UI.centerView()
    if g:NERDTreeAutoCenter
        let current_line = winline()
        let lines_to_top = current_line
        let lines_to_bottom = winheight(nerdtree#getTreeWinNum()) - current_line
        if lines_to_top < g:NERDTreeAutoCenterThreshold || lines_to_bottom < g:NERDTreeAutoCenterThreshold
            normal! zz
        endif
    endif
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

    if line ==# nerdtree#treeUpDirLine()
        return b:NERDTreeRoot.path.getParent()
    endif

    let indent = self._indentLevelFor(line)

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
            let curLine = nerdtree#stripMarkupFromLine(curLine, 1)

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

"FUNCTION: s:UI._indentLevelFor(line) {{{2
function! s:UI._indentLevelFor(line)
    let level = match(a:line, '[^ \-+~▸▾`|]') / nerdtree#treeWid()
    " check if line includes arrows
    if match(a:line, '[▸▾]') > -1
        " decrement level as arrow uses 3 ascii chars
        let level = level - 1
    endif
    return level
endfunction


"FUNCTION: s:UI.restoreScreenState() {{{2
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

"FUNCTION: s:UI.saveScreenState() {{{2
"Saves the current cursor position in the current buffer and the window
"scroll position
function! s:UI.saveScreenState()
    let win = winnr()
    try
        call nerdtree#putCursorInTreeWin()
        let self._screenState = {}
        let self._screenState['oldPos'] = getpos(".")
        let self._screenState['oldTopLine'] = line("w0")
        let self._screenState['oldWindowSize']= winwidth("")
        call nerdtree#exec(win . "wincmd w")
    catch /^NERDTree.InvalidOperationError/
    endtry
endfunction

"FUNCTION: s:UI.render() {{{2
function! s:UI.render()
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
        call nerdtree#putCursorOnBookmarkTable()
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
