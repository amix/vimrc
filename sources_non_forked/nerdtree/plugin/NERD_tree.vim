" ============================================================================
" File:        NERD_tree.vim
" Description: vim global plugin that provides a nice tree explorer
" Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
" Last Change: 28 December, 2011
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================
let s:NERD_tree_version = '4.2.0'

" SECTION: Script init stuff {{{1
"============================================================
if exists("loaded_nerd_tree")
    finish
endif
if v:version < 700
    echoerr "NERDTree: this plugin requires vim >= 7. DOWNLOAD IT! You'll thank me later!"
    finish
endif
let loaded_nerd_tree = 1

"for line continuation - i.e dont want C in &cpo
let s:old_cpo = &cpo
set cpo&vim

let s:running_windows = has("win16") || has("win32") || has("win64")

"Function: s:initVariable() function {{{2
"This function is used to initialise a given variable to a given value. The
"variable is only initialised if it does not exist prior
"
"Args:
"var: the name of the var to be initialised
"value: the value to initialise var to
"
"Returns:
"1 if the var is set, 0 otherwise
function! s:initVariable(var, value)
    if !exists(a:var)
        exec 'let ' . a:var . ' = ' . "'" . substitute(a:value, "'", "''", "g") . "'"
        return 1
    endif
    return 0
endfunction

"SECTION: Init variable calls and other random constants {{{2
call s:initVariable("g:NERDChristmasTree", 1)
call s:initVariable("g:NERDTreeAutoCenter", 1)
call s:initVariable("g:NERDTreeAutoCenterThreshold", 3)
call s:initVariable("g:NERDTreeCaseSensitiveSort", 0)
call s:initVariable("g:NERDTreeChDirMode", 0)
call s:initVariable("g:NERDTreeMinimalUI", 0)
if !exists("g:NERDTreeIgnore")
    let g:NERDTreeIgnore = ['\~$']
endif
call s:initVariable("g:NERDTreeBookmarksFile", expand('$HOME') . '/.NERDTreeBookmarks')
call s:initVariable("g:NERDTreeHighlightCursorline", 1)
call s:initVariable("g:NERDTreeHijackNetrw", 1)
call s:initVariable("g:NERDTreeMouseMode", 1)
call s:initVariable("g:NERDTreeNotificationThreshold", 100)
call s:initVariable("g:NERDTreeQuitOnOpen", 0)
call s:initVariable("g:NERDTreeShowBookmarks", 0)
call s:initVariable("g:NERDTreeShowFiles", 1)
call s:initVariable("g:NERDTreeShowHidden", 0)
call s:initVariable("g:NERDTreeShowLineNumbers", 0)
call s:initVariable("g:NERDTreeSortDirs", 1)
call s:initVariable("g:NERDTreeDirArrows", !s:running_windows)
call s:initVariable("g:NERDTreeCasadeOpenSingleChildDir", 1)

if !exists("g:NERDTreeSortOrder")
    let g:NERDTreeSortOrder = ['\/$', '*', '\.swp$',  '\.bak$', '\~$']
else
    "if there isnt a * in the sort sequence then add one
    if count(g:NERDTreeSortOrder, '*') < 1
        call add(g:NERDTreeSortOrder, '*')
    endif
endif

"we need to use this number many times for sorting... so we calculate it only
"once here
let s:NERDTreeSortStarIndex = index(g:NERDTreeSortOrder, '*')

if !exists('g:NERDTreeStatusline')

    "the exists() crap here is a hack to stop vim spazzing out when
    "loading a session that was created with an open nerd tree. It spazzes
    "because it doesnt store b:NERDTreeRoot (its a b: var, and its a hash)
    let g:NERDTreeStatusline = "%{exists('b:NERDTreeRoot')?b:NERDTreeRoot.path.str():''}"

endif
call s:initVariable("g:NERDTreeWinPos", "left")
call s:initVariable("g:NERDTreeWinSize", 31)

"init the shell commands that will be used to copy nodes, and remove dir trees
"
"Note: the space after the command is important
if s:running_windows
    call s:initVariable("g:NERDTreeRemoveDirCmd", 'rmdir /s /q ')
else
    call s:initVariable("g:NERDTreeRemoveDirCmd", 'rm -rf ')
    call s:initVariable("g:NERDTreeCopyCmd", 'cp -r ')
endif


"SECTION: Init variable calls for key mappings {{{2
call s:initVariable("g:NERDTreeMapActivateNode", "o")
call s:initVariable("g:NERDTreeMapChangeRoot", "C")
call s:initVariable("g:NERDTreeMapChdir", "cd")
call s:initVariable("g:NERDTreeMapCloseChildren", "X")
call s:initVariable("g:NERDTreeMapCloseDir", "x")
call s:initVariable("g:NERDTreeMapDeleteBookmark", "D")
call s:initVariable("g:NERDTreeMapMenu", "m")
call s:initVariable("g:NERDTreeMapHelp", "?")
call s:initVariable("g:NERDTreeMapJumpFirstChild", "K")
call s:initVariable("g:NERDTreeMapJumpLastChild", "J")
call s:initVariable("g:NERDTreeMapJumpNextSibling", "<C-j>")
call s:initVariable("g:NERDTreeMapJumpParent", "p")
call s:initVariable("g:NERDTreeMapJumpPrevSibling", "<C-k>")
call s:initVariable("g:NERDTreeMapJumpRoot", "P")
call s:initVariable("g:NERDTreeMapOpenExpl", "e")
call s:initVariable("g:NERDTreeMapOpenInTab", "t")
call s:initVariable("g:NERDTreeMapOpenInTabSilent", "T")
call s:initVariable("g:NERDTreeMapOpenRecursively", "O")
call s:initVariable("g:NERDTreeMapOpenSplit", "i")
call s:initVariable("g:NERDTreeMapOpenVSplit", "s")
call s:initVariable("g:NERDTreeMapPreview", "g" . NERDTreeMapActivateNode)
call s:initVariable("g:NERDTreeMapPreviewSplit", "g" . NERDTreeMapOpenSplit)
call s:initVariable("g:NERDTreeMapPreviewVSplit", "g" . NERDTreeMapOpenVSplit)
call s:initVariable("g:NERDTreeMapQuit", "q")
call s:initVariable("g:NERDTreeMapRefresh", "r")
call s:initVariable("g:NERDTreeMapRefreshRoot", "R")
call s:initVariable("g:NERDTreeMapToggleBookmarks", "B")
call s:initVariable("g:NERDTreeMapToggleFiles", "F")
call s:initVariable("g:NERDTreeMapToggleFilters", "f")
call s:initVariable("g:NERDTreeMapToggleHidden", "I")
call s:initVariable("g:NERDTreeMapToggleZoom", "A")
call s:initVariable("g:NERDTreeMapUpdir", "u")
call s:initVariable("g:NERDTreeMapUpdirKeepOpen", "U")

"SECTION: Script level variable declaration{{{2
if s:running_windows
    let s:escape_chars =  " `\|\"#%&,?()\*^<>"
else
    let s:escape_chars =  " \\`\|\"#%&,?()\*^<>[]"
endif
let s:NERDTreeBufName = 'NERD_tree_'

let s:tree_wid = 2

if g:NERDTreeDirArrows
    let s:tree_markup_reg = '^\([▾▸] \| \+[▾▸] \| \+\)'
else
    let s:tree_markup_reg = '^[ `|]*[\-+~]'
endif
let s:tree_up_dir_line = '.. (up a dir)'

"the number to add to the nerd tree buffer name to make the buf name unique
let s:next_buffer_number = 1

" SECTION: Commands {{{1
"============================================================
"init the command that users start the nerd tree with
command! -n=? -complete=dir -bar NERDTree :call s:initNerdTree('<args>')
command! -n=? -complete=dir -bar NERDTreeToggle :call s:toggle('<args>')
command! -n=0 -bar NERDTreeClose :call s:closeTreeIfOpen()
command! -n=1 -complete=customlist,s:completeBookmarks -bar NERDTreeFromBookmark call s:initNerdTree('<args>')
command! -n=0 -bar NERDTreeMirror call s:initNerdTreeMirror()
command! -n=0 -bar NERDTreeFind call s:findAndRevealPath()
command! -n=0 -bar NERDTreeFocus call NERDTreeFocus()
" SECTION: Auto commands {{{1
"============================================================
augroup NERDTree
    "Save the cursor position whenever we close the nerd tree
    exec "autocmd BufWinLeave ". s:NERDTreeBufName ."* call <SID>saveScreenState()"

    "disallow insert mode in the NERDTree
    exec "autocmd BufEnter ". s:NERDTreeBufName ."* stopinsert"
augroup END

if g:NERDTreeHijackNetrw
    augroup NERDTreeHijackNetrw
        autocmd VimEnter * silent! autocmd! FileExplorer
        au BufEnter,VimEnter * call s:checkForBrowse(expand("<amatch>"))
    augroup END
endif

"SECTION: Classes {{{1
"============================================================
"CLASS: Bookmark {{{2
"============================================================
let s:Bookmark = {}
" FUNCTION: Bookmark.activate() {{{3
function! s:Bookmark.activate(...)
    call self.open(a:0 ? a:1 : {})
endfunction
" FUNCTION: Bookmark.AddBookmark(name, path) {{{3
" Class method to add a new bookmark to the list, if a previous bookmark exists
" with the same name, just update the path for that bookmark
function! s:Bookmark.AddBookmark(name, path)
    for i in s:Bookmark.Bookmarks()
        if i.name ==# a:name
            let i.path = a:path
            return
        endif
    endfor
    call add(s:Bookmark.Bookmarks(), s:Bookmark.New(a:name, a:path))
    call s:Bookmark.Sort()
endfunction
" Function: Bookmark.Bookmarks()   {{{3
" Class method to get all bookmarks. Lazily initializes the bookmarks global
" variable
function! s:Bookmark.Bookmarks()
    if !exists("g:NERDTreeBookmarks")
        let g:NERDTreeBookmarks = []
    endif
    return g:NERDTreeBookmarks
endfunction
" Function: Bookmark.BookmarkExistsFor(name)   {{{3
" class method that returns 1 if a bookmark with the given name is found, 0
" otherwise
function! s:Bookmark.BookmarkExistsFor(name)
    try
        call s:Bookmark.BookmarkFor(a:name)
        return 1
    catch /^NERDTree.BookmarkNotFoundError/
        return 0
    endtry
endfunction
" Function: Bookmark.BookmarkFor(name)   {{{3
" Class method to get the bookmark that has the given name. {} is return if no
" bookmark is found
function! s:Bookmark.BookmarkFor(name)
    for i in s:Bookmark.Bookmarks()
        if i.name ==# a:name
            return i
        endif
    endfor
    throw "NERDTree.BookmarkNotFoundError: no bookmark found for name: \"". a:name  .'"'
endfunction
" Function: Bookmark.BookmarkNames()   {{{3
" Class method to return an array of all bookmark names
function! s:Bookmark.BookmarkNames()
    let names = []
    for i in s:Bookmark.Bookmarks()
        call add(names, i.name)
    endfor
    return names
endfunction
" FUNCTION: Bookmark.CacheBookmarks(silent) {{{3
" Class method to read all bookmarks from the bookmarks file intialize
" bookmark objects for each one.
"
" Args:
" silent - dont echo an error msg if invalid bookmarks are found
function! s:Bookmark.CacheBookmarks(silent)
    if filereadable(g:NERDTreeBookmarksFile)
        let g:NERDTreeBookmarks = []
        let g:NERDTreeInvalidBookmarks = []
        let bookmarkStrings = readfile(g:NERDTreeBookmarksFile)
        let invalidBookmarksFound = 0
        for i in bookmarkStrings

            "ignore blank lines
            if i != ''

                let name = substitute(i, '^\(.\{-}\) .*$', '\1', '')
                let path = substitute(i, '^.\{-} \(.*\)$', '\1', '')

                try
                    let bookmark = s:Bookmark.New(name, s:Path.New(path))
                    call add(g:NERDTreeBookmarks, bookmark)
                catch /^NERDTree.InvalidArgumentsError/
                    call add(g:NERDTreeInvalidBookmarks, i)
                    let invalidBookmarksFound += 1
                endtry
            endif
        endfor
        if invalidBookmarksFound
            call s:Bookmark.Write()
            if !a:silent
                call s:echo(invalidBookmarksFound . " invalid bookmarks were read. See :help NERDTreeInvalidBookmarks for info.")
            endif
        endif
        call s:Bookmark.Sort()
    endif
endfunction
" FUNCTION: Bookmark.compareTo(otherbookmark) {{{3
" Compare these two bookmarks for sorting purposes
function! s:Bookmark.compareTo(otherbookmark)
    return a:otherbookmark.name < self.name
endfunction
" FUNCTION: Bookmark.ClearAll() {{{3
" Class method to delete all bookmarks.
function! s:Bookmark.ClearAll()
    for i in s:Bookmark.Bookmarks()
        call i.delete()
    endfor
    call s:Bookmark.Write()
endfunction
" FUNCTION: Bookmark.delete() {{{3
" Delete this bookmark. If the node for this bookmark is under the current
" root, then recache bookmarks for its Path object
function! s:Bookmark.delete()
    let node = {}
    try
        let node = self.getNode(1)
    catch /^NERDTree.BookmarkedNodeNotFoundError/
    endtry
    call remove(s:Bookmark.Bookmarks(), index(s:Bookmark.Bookmarks(), self))
    if !empty(node)
        call node.path.cacheDisplayString()
    endif
    call s:Bookmark.Write()
endfunction
" FUNCTION: Bookmark.getNode(searchFromAbsoluteRoot) {{{3
" Gets the treenode for this bookmark
"
" Args:
" searchFromAbsoluteRoot: specifies whether we should search from the current
" tree root, or the highest cached node
function! s:Bookmark.getNode(searchFromAbsoluteRoot)
    let searchRoot = a:searchFromAbsoluteRoot ? s:TreeDirNode.AbsoluteTreeRoot() : b:NERDTreeRoot
    let targetNode = searchRoot.findNode(self.path)
    if empty(targetNode)
        throw "NERDTree.BookmarkedNodeNotFoundError: no node was found for bookmark: " . self.name
    endif
    return targetNode
endfunction
" FUNCTION: Bookmark.GetNodeForName(name, searchFromAbsoluteRoot) {{{3
" Class method that finds the bookmark with the given name and returns the
" treenode for it.
function! s:Bookmark.GetNodeForName(name, searchFromAbsoluteRoot)
    let bookmark = s:Bookmark.BookmarkFor(a:name)
    return bookmark.getNode(a:searchFromAbsoluteRoot)
endfunction
" FUNCTION: Bookmark.GetSelected() {{{3
" returns the Bookmark the cursor is over, or {}
function! s:Bookmark.GetSelected()
    let line = getline(".")
    let name = substitute(line, '^>\(.\{-}\) .\+$', '\1', '')
    if name != line
        try
            return s:Bookmark.BookmarkFor(name)
        catch /^NERDTree.BookmarkNotFoundError/
            return {}
        endtry
    endif
    return {}
endfunction

" Function: Bookmark.InvalidBookmarks()   {{{3
" Class method to get all invalid bookmark strings read from the bookmarks
" file
function! s:Bookmark.InvalidBookmarks()
    if !exists("g:NERDTreeInvalidBookmarks")
        let g:NERDTreeInvalidBookmarks = []
    endif
    return g:NERDTreeInvalidBookmarks
endfunction
" FUNCTION: Bookmark.mustExist() {{{3
function! s:Bookmark.mustExist()
    if !self.path.exists()
        call s:Bookmark.CacheBookmarks(1)
        throw "NERDTree.BookmarkPointsToInvalidLocationError: the bookmark \"".
            \ self.name ."\" points to a non existing location: \"". self.path.str()
    endif
endfunction
" FUNCTION: Bookmark.New(name, path) {{{3
" Create a new bookmark object with the given name and path object
function! s:Bookmark.New(name, path)
    if a:name =~# ' '
        throw "NERDTree.IllegalBookmarkNameError: illegal name:" . a:name
    endif

    let newBookmark = copy(self)
    let newBookmark.name = a:name
    let newBookmark.path = a:path
    return newBookmark
endfunction
" FUNCTION: Bookmark.open([options]) {{{3
"Args:
"A dictionary containing the following keys (all optional):
"  'where': Specifies whether the node should be opened in new split/tab or in
"           the previous window. Can be either 'v' (vertical split), 'h'
"           (horizontal split), 't' (new tab) or 'p' (previous window).
"  'reuse': if a window is displaying the file then jump the cursor there
"  'keepopen': dont close the tree window
"  'stay': open the file, but keep the cursor in the tree win
"
function! s:Bookmark.open(...)
    let opts = a:0 ? a:1 : {}

    if self.path.isDirectory && !has_key(opts, 'where')
        call self.toRoot()
    else
        let opener = s:Opener.New(self.path, opts)
        call opener.open(self)
    endif
endfunction
" FUNCTION: Bookmark.openInNewTab(options) {{{3
" Create a new bookmark object with the given name and path object
function! s:Bookmark.openInNewTab(options)
    call s:deprecated('Bookmark.openInNewTab', 'is deprecated, use open() instead')
    call self.open(a:options)
endfunction
" Function: Bookmark.setPath(path)   {{{3
" makes this bookmark point to the given path
function! s:Bookmark.setPath(path)
    let self.path = a:path
endfunction
" Function: Bookmark.Sort()   {{{3
" Class method that sorts all bookmarks
function! s:Bookmark.Sort()
    let CompareFunc = function("s:compareBookmarks")
    call sort(s:Bookmark.Bookmarks(), CompareFunc)
endfunction
" Function: Bookmark.str()   {{{3
" Get the string that should be rendered in the view for this bookmark
function! s:Bookmark.str()
    let pathStrMaxLen = winwidth(s:getTreeWinNum()) - 4 - len(self.name)
    if &nu
        let pathStrMaxLen = pathStrMaxLen - &numberwidth
    endif

    let pathStr = self.path.str({'format': 'UI'})
    if len(pathStr) > pathStrMaxLen
        let pathStr = '<' . strpart(pathStr, len(pathStr) - pathStrMaxLen)
    endif
    return '>' . self.name . ' ' . pathStr
endfunction
" FUNCTION: Bookmark.toRoot() {{{3
" Make the node for this bookmark the new tree root
function! s:Bookmark.toRoot()
    if self.validate()
        try
            let targetNode = self.getNode(1)
        catch /^NERDTree.BookmarkedNodeNotFoundError/
            let targetNode = s:TreeFileNode.New(s:Bookmark.BookmarkFor(self.name).path)
        endtry
        call targetNode.makeRoot()
        call s:renderView()
        call targetNode.putCursorHere(0, 0)
    endif
endfunction
" FUNCTION: Bookmark.ToRoot(name) {{{3
" Make the node for this bookmark the new tree root
function! s:Bookmark.ToRoot(name)
    let bookmark = s:Bookmark.BookmarkFor(a:name)
    call bookmark.toRoot()
endfunction


"FUNCTION: Bookmark.validate() {{{3
function! s:Bookmark.validate()
    if self.path.exists()
        return 1
    else
        call s:Bookmark.CacheBookmarks(1)
        call s:renderView()
        call s:echo(self.name . "now points to an invalid location. See :help NERDTreeInvalidBookmarks for info.")
        return 0
    endif
endfunction

" Function: Bookmark.Write()   {{{3
" Class method to write all bookmarks to the bookmarks file
function! s:Bookmark.Write()
    let bookmarkStrings = []
    for i in s:Bookmark.Bookmarks()
        call add(bookmarkStrings, i.name . ' ' . i.path.str())
    endfor

    "add a blank line before the invalid ones
    call add(bookmarkStrings, "")

    for j in s:Bookmark.InvalidBookmarks()
        call add(bookmarkStrings, j)
    endfor
    call writefile(bookmarkStrings, g:NERDTreeBookmarksFile)
endfunction
"CLASS: KeyMap {{{2
"============================================================
let s:KeyMap = {}
"FUNCTION: KeyMap.All() {{{3
function! s:KeyMap.All()
    if !exists("s:keyMaps")
        let s:keyMaps = []
    endif
    return s:keyMaps
endfunction

"FUNCTION: KeyMap.FindFor(key, scope) {{{3
function! s:KeyMap.FindFor(key, scope)
    for i in s:KeyMap.All()
         if i.key ==# a:key && i.scope ==# a:scope
            return i
        endif
    endfor
    return {}
endfunction

"FUNCTION: KeyMap.BindAll() {{{3
function! s:KeyMap.BindAll()
    for i in s:KeyMap.All()
        call i.bind()
    endfor
endfunction

"FUNCTION: KeyMap.bind() {{{3
function! s:KeyMap.bind()
    " If the key sequence we're trying to map contains any '<>' notation, we
    " must replace each of the '<' characters with '<lt>' to ensure the string
    " is not translated into its corresponding keycode during the later part
    " of the map command below
    " :he <>
    let specialNotationRegex = '\m<\([[:alnum:]_-]\+>\)'
    if self.key =~# specialNotationRegex
        let keymapInvokeString = substitute(self.key, specialNotationRegex, '<lt>\1', 'g')
    else
        let keymapInvokeString = self.key
    endif

    let premap = self.key == "<LeftRelease>" ? " <LeftRelease>" : " "

    exec 'nnoremap <buffer> <silent> '. self.key . premap . ':call <SID>KeyMap_Invoke("'. keymapInvokeString .'")<cr>'
endfunction

"FUNCTION: KeyMap.Remove(key, scope) {{{3
function! s:KeyMap.Remove(key, scope)
    let maps = s:KeyMap.All()
    for i in range(len(maps))
         if maps[i].key ==# a:key && maps[i].scope ==# a:scope
            return remove(maps, i)
        endif
    endfor
endfunction
"FUNCTION: KeyMap.invoke() {{{3
"Call the KeyMaps callback function
function! s:KeyMap.invoke(...)
    let Callback = function(self.callback)
    if a:0
        call Callback(a:1)
    else
        call Callback()
    endif
endfunction


"FUNCTION: KeyMap.Invoke() {{{3
"Find a keymapping for a:key and the current scope invoke it.
"
"Scope is determined as follows:
"   * if the cursor is on a dir node then "DirNode"
"   * if the cursor is on a file node then "FileNode"
"   * if the cursor is on a bookmark then "Bookmark"
"
"If a keymap has the scope of "all" then it will be called if no other keymap
"is found for a:key and the scope.
function! s:KeyMap.Invoke(key)
    let node = s:TreeFileNode.GetSelected()
    if !empty(node)

        "try file node
        if !node.path.isDirectory
            let km = s:KeyMap.FindFor(a:key, "FileNode")
            if !empty(km)
                return km.invoke(node)
            endif
        endif

        "try dir node
        if node.path.isDirectory
            let km = s:KeyMap.FindFor(a:key, "DirNode")
            if !empty(km)
                return km.invoke(node)
            endif
        endif

        "try generic node
        let km = s:KeyMap.FindFor(a:key, "Node")
        if !empty(km)
            return km.invoke(node)
        endif

    endif

    "try bookmark
    let bm = s:Bookmark.GetSelected()
    if !empty(bm)
        let km = s:KeyMap.FindFor(a:key, "Bookmark")
        if !empty(km)
            return km.invoke(bm)
        endif
    endif

    "try all
    let km = s:KeyMap.FindFor(a:key, "all")
    if !empty(km)
        return km.invoke()
    endif
endfunction

"this is needed since I cant figure out how to invoke dict functions from a
"key map
function! s:KeyMap_Invoke(key)
    call s:KeyMap.Invoke(a:key)
endfunction

"FUNCTION: KeyMap.Create(options) {{{3
function! s:KeyMap.Create(options)
    let newKeyMap = copy(self)
    let opts = extend({'scope': 'all', 'quickhelpText': ''}, copy(a:options))
    let newKeyMap.key = opts['key']
    let newKeyMap.quickhelpText = opts['quickhelpText']
    let newKeyMap.callback = opts['callback']
    let newKeyMap.scope = opts['scope']

    call s:KeyMap.Add(newKeyMap)
endfunction

"FUNCTION: KeyMap.Add(keymap) {{{3
function! s:KeyMap.Add(keymap)
    call s:KeyMap.Remove(a:keymap.key, a:keymap.scope)
    call add(s:KeyMap.All(), a:keymap)
endfunction

"CLASS: MenuController {{{2
"============================================================
let s:MenuController = {}
"FUNCTION: MenuController.New(menuItems) {{{3
"create a new menu controller that operates on the given menu items
function! s:MenuController.New(menuItems)
    let newMenuController =  copy(self)
    if a:menuItems[0].isSeparator()
        let newMenuController.menuItems = a:menuItems[1:-1]
    else
        let newMenuController.menuItems = a:menuItems
    endif
    return newMenuController
endfunction

"FUNCTION: MenuController.showMenu() {{{3
"start the main loop of the menu and get the user to choose/execute a menu
"item
function! s:MenuController.showMenu()
    call self._saveOptions()

    try
        let self.selection = 0

        let done = 0
        while !done
            redraw!
            call self._echoPrompt()
            let key = nr2char(getchar())
            let done = self._handleKeypress(key)
        endwhile
    finally
        call self._restoreOptions()
    endtry

    if self.selection != -1
        let m = self._current()
        call m.execute()
    endif
endfunction

"FUNCTION: MenuController._echoPrompt() {{{3
function! s:MenuController._echoPrompt()
    echo "NERDTree Menu. Use j/k/enter and the shortcuts indicated"
    echo "=========================================================="

    for i in range(0, len(self.menuItems)-1)
        if self.selection == i
            echo "> " . self.menuItems[i].text
        else
            echo "  " . self.menuItems[i].text
        endif
    endfor
endfunction

"FUNCTION: MenuController._current(key) {{{3
"get the MenuItem that is currently selected
function! s:MenuController._current()
    return self.menuItems[self.selection]
endfunction

"FUNCTION: MenuController._handleKeypress(key) {{{3
"change the selection (if appropriate) and return 1 if the user has made
"their choice, 0 otherwise
function! s:MenuController._handleKeypress(key)
    if a:key == 'j'
        call self._cursorDown()
    elseif a:key == 'k'
        call self._cursorUp()
    elseif a:key == nr2char(27) "escape
        let self.selection = -1
        return 1
    elseif a:key == "\r" || a:key == "\n" "enter and ctrl-j
        return 1
    else
        let index = self._nextIndexFor(a:key)
        if index != -1
            let self.selection = index
            if len(self._allIndexesFor(a:key)) == 1
                return 1
            endif
        endif
    endif

    return 0
endfunction

"FUNCTION: MenuController._allIndexesFor(shortcut) {{{3
"get indexes to all menu items with the given shortcut
function! s:MenuController._allIndexesFor(shortcut)
    let toReturn = []

    for i in range(0, len(self.menuItems)-1)
        if self.menuItems[i].shortcut == a:shortcut
            call add(toReturn, i)
        endif
    endfor

    return toReturn
endfunction

"FUNCTION: MenuController._nextIndexFor(shortcut) {{{3
"get the index to the next menu item with the given shortcut, starts from the
"current cursor location and wraps around to the top again if need be
function! s:MenuController._nextIndexFor(shortcut)
    for i in range(self.selection+1, len(self.menuItems)-1)
        if self.menuItems[i].shortcut == a:shortcut
            return i
        endif
    endfor

    for i in range(0, self.selection)
        if self.menuItems[i].shortcut == a:shortcut
            return i
        endif
    endfor

    return -1
endfunction

"FUNCTION: MenuController._setCmdheight() {{{3
"sets &cmdheight to whatever is needed to display the menu
function! s:MenuController._setCmdheight()
    let &cmdheight = len(self.menuItems) + 3
endfunction

"FUNCTION: MenuController._saveOptions() {{{3
"set any vim options that are required to make the menu work (saving their old
"values)
function! s:MenuController._saveOptions()
    let self._oldLazyredraw = &lazyredraw
    let self._oldCmdheight = &cmdheight
    set nolazyredraw
    call self._setCmdheight()
endfunction

"FUNCTION: MenuController._restoreOptions() {{{3
"restore the options we saved in _saveOptions()
function! s:MenuController._restoreOptions()
    let &cmdheight = self._oldCmdheight
    let &lazyredraw = self._oldLazyredraw
endfunction

"FUNCTION: MenuController._cursorDown() {{{3
"move the cursor to the next menu item, skipping separators
function! s:MenuController._cursorDown()
    let done = 0
    while !done
        if self.selection < len(self.menuItems)-1
            let self.selection += 1
        else
            let self.selection = 0
        endif

        if !self._current().isSeparator()
            let done = 1
        endif
    endwhile
endfunction

"FUNCTION: MenuController._cursorUp() {{{3
"move the cursor to the previous menu item, skipping separators
function! s:MenuController._cursorUp()
    let done = 0
    while !done
        if self.selection > 0
            let self.selection -= 1
        else
            let self.selection = len(self.menuItems)-1
        endif

        if !self._current().isSeparator()
            let done = 1
        endif
    endwhile
endfunction

"CLASS: MenuItem {{{2
"============================================================
let s:MenuItem = {}
"FUNCTION: MenuItem.All() {{{3
"get all top level menu items
function! s:MenuItem.All()
    if !exists("s:menuItems")
        let s:menuItems = []
    endif
    return s:menuItems
endfunction

"FUNCTION: MenuItem.AllEnabled() {{{3
"get all top level menu items that are currently enabled
function! s:MenuItem.AllEnabled()
    let toReturn = []
    for i in s:MenuItem.All()
        if i.enabled()
            call add(toReturn, i)
        endif
    endfor
    return toReturn
endfunction

"FUNCTION: MenuItem.Create(options) {{{3
"make a new menu item and add it to the global list
function! s:MenuItem.Create(options)
    let newMenuItem = copy(self)

    let newMenuItem.text = a:options['text']
    let newMenuItem.shortcut = a:options['shortcut']
    let newMenuItem.children = []

    let newMenuItem.isActiveCallback = -1
    if has_key(a:options, 'isActiveCallback')
        let newMenuItem.isActiveCallback = a:options['isActiveCallback']
    endif

    let newMenuItem.callback = -1
    if has_key(a:options, 'callback')
        let newMenuItem.callback = a:options['callback']
    endif

    if has_key(a:options, 'parent')
        call add(a:options['parent'].children, newMenuItem)
    else
        call add(s:MenuItem.All(), newMenuItem)
    endif

    return newMenuItem
endfunction

"FUNCTION: MenuItem.CreateSeparator(options) {{{3
"make a new separator menu item and add it to the global list
function! s:MenuItem.CreateSeparator(options)
    let standard_options = { 'text': '--------------------',
                \ 'shortcut': -1,
                \ 'callback': -1 }
    let options = extend(a:options, standard_options, "force")

    return s:MenuItem.Create(options)
endfunction

"FUNCTION: MenuItem.CreateSubmenu(options) {{{3
"make a new submenu and add it to global list
function! s:MenuItem.CreateSubmenu(options)
    let standard_options = { 'callback': -1 }
    let options = extend(a:options, standard_options, "force")

    return s:MenuItem.Create(options)
endfunction

"FUNCTION: MenuItem.enabled() {{{3
"return 1 if this menu item should be displayed
"
"delegates off to the isActiveCallback, and defaults to 1 if no callback was
"specified
function! s:MenuItem.enabled()
    if self.isActiveCallback != -1
        return {self.isActiveCallback}()
    endif
    return 1
endfunction

"FUNCTION: MenuItem.execute() {{{3
"perform the action behind this menu item, if this menuitem has children then
"display a new menu for them, otherwise deletegate off to the menuitem's
"callback
function! s:MenuItem.execute()
    if len(self.children)
        let mc = s:MenuController.New(self.children)
        call mc.showMenu()
    else
        if self.callback != -1
            call {self.callback}()
        endif
    endif
endfunction

"FUNCTION: MenuItem.isSeparator() {{{3
"return 1 if this menuitem is a separator
function! s:MenuItem.isSeparator()
    return self.callback == -1 && self.children == []
endfunction

"FUNCTION: MenuItem.isSubmenu() {{{3
"return 1 if this menuitem is a submenu
function! s:MenuItem.isSubmenu()
    return self.callback == -1 && !empty(self.children)
endfunction

"CLASS: TreeFileNode {{{2
"This class is the parent of the TreeDirNode class and constitures the
"'Component' part of the composite design pattern between the treenode
"classes.
"============================================================
let s:TreeFileNode = {}
"FUNCTION: TreeFileNode.activate(...) {{{3
function! s:TreeFileNode.activate(...)
    call self.open(a:0 ? a:1 : {})
endfunction
"FUNCTION: TreeFileNode.bookmark(name) {{{3
"bookmark this node with a:name
function! s:TreeFileNode.bookmark(name)

    "if a bookmark exists with the same name and the node is cached then save
    "it so we can update its display string
    let oldMarkedNode = {}
    try
        let oldMarkedNode = s:Bookmark.GetNodeForName(a:name, 1)
    catch /^NERDTree.BookmarkNotFoundError/
    catch /^NERDTree.BookmarkedNodeNotFoundError/
    endtry

    call s:Bookmark.AddBookmark(a:name, self.path)
    call self.path.cacheDisplayString()
    call s:Bookmark.Write()

    if !empty(oldMarkedNode)
        call oldMarkedNode.path.cacheDisplayString()
    endif
endfunction
"FUNCTION: TreeFileNode.cacheParent() {{{3
"initializes self.parent if it isnt already
function! s:TreeFileNode.cacheParent()
    if empty(self.parent)
        let parentPath = self.path.getParent()
        if parentPath.equals(self.path)
            throw "NERDTree.CannotCacheParentError: already at root"
        endif
        let self.parent = s:TreeFileNode.New(parentPath)
    endif
endfunction
"FUNCTION: TreeFileNode.compareNodes {{{3
"This is supposed to be a class level method but i cant figure out how to
"get func refs to work from a dict..
"
"A class level method that compares two nodes
"
"Args:
"n1, n2: the 2 nodes to compare
function! s:compareNodes(n1, n2)
    return a:n1.path.compareTo(a:n2.path)
endfunction

"FUNCTION: TreeFileNode.clearBookmarks() {{{3
function! s:TreeFileNode.clearBookmarks()
    for i in s:Bookmark.Bookmarks()
        if i.path.equals(self.path)
            call i.delete()
        end
    endfor
    call self.path.cacheDisplayString()
endfunction
"FUNCTION: TreeFileNode.copy(dest) {{{3
function! s:TreeFileNode.copy(dest)
    call self.path.copy(a:dest)
    let newPath = s:Path.New(a:dest)
    let parent = b:NERDTreeRoot.findNode(newPath.getParent())
    if !empty(parent)
        call parent.refresh()
        return parent.findNode(newPath)
    else
        return {}
    endif
endfunction

"FUNCTION: TreeFileNode.delete {{{3
"Removes this node from the tree and calls the Delete method for its path obj
function! s:TreeFileNode.delete()
    call self.path.delete()
    call self.parent.removeChild(self)
endfunction

"FUNCTION: TreeFileNode.displayString() {{{3
"
"Returns a string that specifies how the node should be represented as a
"string
"
"Return:
"a string that can be used in the view to represent this node
function! s:TreeFileNode.displayString()
    return self.path.displayString()
endfunction

"FUNCTION: TreeFileNode.equals(treenode) {{{3
"
"Compares this treenode to the input treenode and returns 1 if they are the
"same node.
"
"Use this method instead of ==  because sometimes when the treenodes contain
"many children, vim seg faults when doing ==
"
"Args:
"treenode: the other treenode to compare to
function! s:TreeFileNode.equals(treenode)
    return self.path.str() ==# a:treenode.path.str()
endfunction

"FUNCTION: TreeFileNode.findNode(path) {{{3
"Returns self if this node.path.Equals the given path.
"Returns {} if not equal.
"
"Args:
"path: the path object to compare against
function! s:TreeFileNode.findNode(path)
    if a:path.equals(self.path)
        return self
    endif
    return {}
endfunction
"FUNCTION: TreeFileNode.findOpenDirSiblingWithVisibleChildren(direction) {{{3
"
"Finds the next sibling for this node in the indicated direction. This sibling
"must be a directory and may/may not have children as specified.
"
"Args:
"direction: 0 if you want to find the previous sibling, 1 for the next sibling
"
"Return:
"a treenode object or {} if no appropriate sibling could be found
function! s:TreeFileNode.findOpenDirSiblingWithVisibleChildren(direction)
    "if we have no parent then we can have no siblings
    if self.parent != {}
        let nextSibling = self.findSibling(a:direction)

        while nextSibling != {}
            if nextSibling.path.isDirectory && nextSibling.hasVisibleChildren() && nextSibling.isOpen
                return nextSibling
            endif
            let nextSibling = nextSibling.findSibling(a:direction)
        endwhile
    endif

    return {}
endfunction
"FUNCTION: TreeFileNode.findSibling(direction) {{{3
"
"Finds the next sibling for this node in the indicated direction
"
"Args:
"direction: 0 if you want to find the previous sibling, 1 for the next sibling
"
"Return:
"a treenode object or {} if no sibling could be found
function! s:TreeFileNode.findSibling(direction)
    "if we have no parent then we can have no siblings
    if self.parent != {}

        "get the index of this node in its parents children
        let siblingIndx = self.parent.getChildIndex(self.path)

        if siblingIndx != -1
            "move a long to the next potential sibling node
            let siblingIndx = a:direction ==# 1 ? siblingIndx+1 : siblingIndx-1

            "keep moving along to the next sibling till we find one that is valid
            let numSiblings = self.parent.getChildCount()
            while siblingIndx >= 0 && siblingIndx < numSiblings

                "if the next node is not an ignored node (i.e. wont show up in the
                "view) then return it
                if self.parent.children[siblingIndx].path.ignore() ==# 0
                    return self.parent.children[siblingIndx]
                endif

                "go to next node
                let siblingIndx = a:direction ==# 1 ? siblingIndx+1 : siblingIndx-1
            endwhile
        endif
    endif

    return {}
endfunction

"FUNCTION: TreeFileNode.getLineNum(){{{3
"returns the line number this node is rendered on, or -1 if it isnt rendered
function! s:TreeFileNode.getLineNum()
    "if the node is the root then return the root line no.
    if self.isRoot()
        return s:TreeFileNode.GetRootLineNum()
    endif

    let totalLines = line("$")

    "the path components we have matched so far
    let pathcomponents = [substitute(b:NERDTreeRoot.path.str({'format': 'UI'}), '/ *$', '', '')]
    "the index of the component we are searching for
    let curPathComponent = 1

    let fullpath = self.path.str({'format': 'UI'})


    let lnum = s:TreeFileNode.GetRootLineNum()
    while lnum > 0
        let lnum = lnum + 1
        "have we reached the bottom of the tree?
        if lnum ==# totalLines+1
            return -1
        endif

        let curLine = getline(lnum)

        let indent = s:indentLevelFor(curLine)
        if indent ==# curPathComponent
            let curLine = s:stripMarkupFromLine(curLine, 1)

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

"FUNCTION: TreeFileNode.GetRootForTab(){{{3
"get the root node for this tab
function! s:TreeFileNode.GetRootForTab()
    if s:treeExistsForTab()
        return getbufvar(t:NERDTreeBufName, 'NERDTreeRoot')
    end
    return {}
endfunction
"FUNCTION: TreeFileNode.GetRootLineNum(){{{3
"gets the line number of the root node
function! s:TreeFileNode.GetRootLineNum()
    let rootLine = 1
    while getline(rootLine) !~# '^\(/\|<\)'
        let rootLine = rootLine + 1
    endwhile
    return rootLine
endfunction

"FUNCTION: TreeFileNode.GetSelected() {{{3
"gets the treenode that the cursor is currently over
function! s:TreeFileNode.GetSelected()
    try
        let path = s:getPath(line("."))
        if path ==# {}
            return {}
        endif
        return b:NERDTreeRoot.findNode(path)
    catch /NERDTree/
        return {}
    endtry
endfunction
"FUNCTION: TreeFileNode.isVisible() {{{3
"returns 1 if this node should be visible according to the tree filters and
"hidden file filters (and their on/off status)
function! s:TreeFileNode.isVisible()
    return !self.path.ignore()
endfunction
"FUNCTION: TreeFileNode.isRoot() {{{3
"returns 1 if this node is b:NERDTreeRoot
function! s:TreeFileNode.isRoot()
    if !s:treeExistsForBuf()
        throw "NERDTree.NoTreeError: No tree exists for the current buffer"
    endif

    return self.equals(b:NERDTreeRoot)
endfunction

"FUNCTION: TreeFileNode.makeRoot() {{{3
"Make this node the root of the tree
function! s:TreeFileNode.makeRoot()
    if self.path.isDirectory
        let b:NERDTreeRoot = self
    else
        call self.cacheParent()
        let b:NERDTreeRoot = self.parent
    endif

    call b:NERDTreeRoot.open()

    "change dir to the dir of the new root if instructed to
    if g:NERDTreeChDirMode ==# 2
        exec "cd " . b:NERDTreeRoot.path.str({'format': 'Edit'})
    endif

    silent doautocmd User NERDTreeNewRoot
endfunction
"FUNCTION: TreeFileNode.New(path) {{{3
"Returns a new TreeNode object with the given path and parent
"
"Args:
"path: a path object representing the full filesystem path to the file/dir that the node represents
function! s:TreeFileNode.New(path)
    if a:path.isDirectory
        return s:TreeDirNode.New(a:path)
    else
        let newTreeNode = copy(self)
        let newTreeNode.path = a:path
        let newTreeNode.parent = {}
        return newTreeNode
    endif
endfunction

"FUNCTION: TreeFileNode.open() {{{3
function! s:TreeFileNode.open(...)
    let opts = a:0 ? a:1 : {}
    let opener = s:Opener.New(self.path, opts)
    call opener.open(self)
endfunction

"FUNCTION: TreeFileNode.openSplit() {{{3
"Open this node in a new window
function! s:TreeFileNode.openSplit()
    call s:deprecated('TreeFileNode.openSplit', 'is deprecated, use .open() instead.')
    call self.open({'where': 'h'})
endfunction
"FUNCTION: TreeFileNode.openVSplit() {{{3
"Open this node in a new vertical window
function! s:TreeFileNode.openVSplit()
    call s:deprecated('TreeFileNode.openVSplit', 'is deprecated, use .open() instead.')
    call self.open({'where': 'v'})
endfunction
"FUNCTION: TreeFileNode.openInNewTab(options) {{{3
function! s:TreeFileNode.openInNewTab(options)
    echomsg 'TreeFileNode.openInNewTab is deprecated'
    call self.open(extend({'where': 't'}, a:options))
endfunction
"FUNCTION: TreeFileNode.putCursorHere(isJump, recurseUpward){{{3
"Places the cursor on the line number this node is rendered on
"
"Args:
"isJump: 1 if this cursor movement should be counted as a jump by vim
"recurseUpward: try to put the cursor on the parent if the this node isnt
"visible
function! s:TreeFileNode.putCursorHere(isJump, recurseUpward)
    let ln = self.getLineNum()
    if ln != -1
        if a:isJump
            mark '
        endif
        call cursor(ln, col("."))
    else
        if a:recurseUpward
            let node = self
            while node != {} && node.getLineNum() ==# -1
                let node = node.parent
                call node.open()
            endwhile
            call s:renderView()
            call node.putCursorHere(a:isJump, 0)
        endif
    endif
endfunction

"FUNCTION: TreeFileNode.refresh() {{{3
function! s:TreeFileNode.refresh()
    call self.path.refresh()
endfunction
"FUNCTION: TreeFileNode.rename() {{{3
"Calls the rename method for this nodes path obj
function! s:TreeFileNode.rename(newName)
    let newName = substitute(a:newName, '\(\\\|\/\)$', '', '')
    call self.path.rename(newName)
    call self.parent.removeChild(self)

    let parentPath = self.path.getParent()
    let newParent = b:NERDTreeRoot.findNode(parentPath)

    if newParent != {}
        call newParent.createChild(self.path, 1)
        call newParent.refresh()
    endif
endfunction
"FUNCTION: TreeFileNode.renderToString {{{3
"returns a string representation for this tree to be rendered in the view
function! s:TreeFileNode.renderToString()
    return self._renderToString(0, 0, [], self.getChildCount() ==# 1)
endfunction


"Args:
"depth: the current depth in the tree for this call
"drawText: 1 if we should actually draw the line for this node (if 0 then the
"child nodes are rendered only)
"vertMap: a binary array that indicates whether a vertical bar should be draw
"for each depth in the tree
"isLastChild:true if this curNode is the last child of its parent
function! s:TreeFileNode._renderToString(depth, drawText, vertMap, isLastChild)
    let output = ""
    if a:drawText ==# 1

        let treeParts = ''

        "get all the leading spaces and vertical tree parts for this line
        if a:depth > 1
            for j in a:vertMap[0:-2]
                if g:NERDTreeDirArrows
                    let treeParts = treeParts . '  '
                else
                    if j ==# 1
                        let treeParts = treeParts . '| '
                    else
                        let treeParts = treeParts . '  '
                    endif
                endif
            endfor
        endif

        "get the last vertical tree part for this line which will be different
        "if this node is the last child of its parent
        if !g:NERDTreeDirArrows
            if a:isLastChild
                let treeParts = treeParts . '`'
            else
                let treeParts = treeParts . '|'
            endif
        endif

        "smack the appropriate dir/file symbol on the line before the file/dir
        "name itself
        if self.path.isDirectory
            if self.isOpen
                if g:NERDTreeDirArrows
                    let treeParts = treeParts . '▾ '
                else
                    let treeParts = treeParts . '~'
                endif
            else
                if g:NERDTreeDirArrows
                    let treeParts = treeParts . '▸ '
                else
                    let treeParts = treeParts . '+'
                endif
            endif
        else
            if g:NERDTreeDirArrows
                let treeParts = treeParts . '  '
            else
                let treeParts = treeParts . '-'
            endif
        endif
        let line = treeParts . self.displayString()

        let output = output . line . "\n"
    endif

    "if the node is an open dir, draw its children
    if self.path.isDirectory ==# 1 && self.isOpen ==# 1

        let childNodesToDraw = self.getVisibleChildren()
        if len(childNodesToDraw) > 0

            "draw all the nodes children except the last
            let lastIndx = len(childNodesToDraw)-1
            if lastIndx > 0
                for i in childNodesToDraw[0:lastIndx-1]
                    let output = output . i._renderToString(a:depth + 1, 1, add(copy(a:vertMap), 1), 0)
                endfor
            endif

            "draw the last child, indicating that it IS the last
            let output = output . childNodesToDraw[lastIndx]._renderToString(a:depth + 1, 1, add(copy(a:vertMap), 0), 1)
        endif
    endif

    return output
endfunction
"CLASS: TreeDirNode {{{2
"This class is a child of the TreeFileNode class and constitutes the
"'Composite' part of the composite design pattern between the treenode
"classes.
"============================================================
let s:TreeDirNode = copy(s:TreeFileNode)
"FUNCTION: TreeDirNode.AbsoluteTreeRoot(){{{3
"class method that returns the highest cached ancestor of the current root
function! s:TreeDirNode.AbsoluteTreeRoot()
    let currentNode = b:NERDTreeRoot
    while currentNode.parent != {}
        let currentNode = currentNode.parent
    endwhile
    return currentNode
endfunction
"FUNCTION: TreeDirNode.activate([options]) {{{3
unlet s:TreeDirNode.activate
function! s:TreeDirNode.activate(...)
    let opts = a:0 ? a:1 : {}
    call self.toggleOpen(opts)
    call s:renderView()
    call self.putCursorHere(0, 0)
endfunction
"FUNCTION: TreeDirNode.addChild(treenode, inOrder) {{{3
"Adds the given treenode to the list of children for this node
"
"Args:
"-treenode: the node to add
"-inOrder: 1 if the new node should be inserted in sorted order
function! s:TreeDirNode.addChild(treenode, inOrder)
    call add(self.children, a:treenode)
    let a:treenode.parent = self

    if a:inOrder
        call self.sortChildren()
    endif
endfunction

"FUNCTION: TreeDirNode.close() {{{3
"Closes this directory
function! s:TreeDirNode.close()
    let self.isOpen = 0
endfunction

"FUNCTION: TreeDirNode.closeChildren() {{{3
"Closes all the child dir nodes of this node
function! s:TreeDirNode.closeChildren()
    for i in self.children
        if i.path.isDirectory
            call i.close()
            call i.closeChildren()
        endif
    endfor
endfunction

"FUNCTION: TreeDirNode.createChild(path, inOrder) {{{3
"Instantiates a new child node for this node with the given path. The new
"nodes parent is set to this node.
"
"Args:
"path: a Path object that this node will represent/contain
"inOrder: 1 if the new node should be inserted in sorted order
"
"Returns:
"the newly created node
function! s:TreeDirNode.createChild(path, inOrder)
    let newTreeNode = s:TreeFileNode.New(a:path)
    call self.addChild(newTreeNode, a:inOrder)
    return newTreeNode
endfunction

"FUNCTION: TreeDirNode.findNode(path) {{{3
"Will find one of the children (recursively) that has the given path
"
"Args:
"path: a path object
unlet s:TreeDirNode.findNode
function! s:TreeDirNode.findNode(path)
    if a:path.equals(self.path)
        return self
    endif
    if stridx(a:path.str(), self.path.str(), 0) ==# -1
        return {}
    endif

    if self.path.isDirectory
        for i in self.children
            let retVal = i.findNode(a:path)
            if retVal != {}
                return retVal
            endif
        endfor
    endif
    return {}
endfunction
"FUNCTION: TreeDirNode.getChildCount() {{{3
"Returns the number of children this node has
function! s:TreeDirNode.getChildCount()
    return len(self.children)
endfunction

"FUNCTION: TreeDirNode.getChild(path) {{{3
"Returns child node of this node that has the given path or {} if no such node
"exists.
"
"This function doesnt not recurse into child dir nodes
"
"Args:
"path: a path object
function! s:TreeDirNode.getChild(path)
    if stridx(a:path.str(), self.path.str(), 0) ==# -1
        return {}
    endif

    let index = self.getChildIndex(a:path)
    if index ==# -1
        return {}
    else
        return self.children[index]
    endif

endfunction

"FUNCTION: TreeDirNode.getChildByIndex(indx, visible) {{{3
"returns the child at the given index
"Args:
"indx: the index to get the child from
"visible: 1 if only the visible children array should be used, 0 if all the
"children should be searched.
function! s:TreeDirNode.getChildByIndex(indx, visible)
    let array_to_search = a:visible? self.getVisibleChildren() : self.children
    if a:indx > len(array_to_search)
        throw "NERDTree.InvalidArgumentsError: Index is out of bounds."
    endif
    return array_to_search[a:indx]
endfunction

"FUNCTION: TreeDirNode.getChildIndex(path) {{{3
"Returns the index of the child node of this node that has the given path or
"-1 if no such node exists.
"
"This function doesnt not recurse into child dir nodes
"
"Args:
"path: a path object
function! s:TreeDirNode.getChildIndex(path)
    if stridx(a:path.str(), self.path.str(), 0) ==# -1
        return -1
    endif

    "do a binary search for the child
    let a = 0
    let z = self.getChildCount()
    while a < z
        let mid = (a+z)/2
        let diff = a:path.compareTo(self.children[mid].path)

        if diff ==# -1
            let z = mid
        elseif diff ==# 1
            let a = mid+1
        else
            return mid
        endif
    endwhile
    return -1
endfunction

"FUNCTION: TreeDirNode.GetSelected() {{{3
"Returns the current node if it is a dir node, or else returns the current
"nodes parent
unlet s:TreeDirNode.GetSelected
function! s:TreeDirNode.GetSelected()
    let currentDir = s:TreeFileNode.GetSelected()
    if currentDir != {} && !currentDir.isRoot()
        if currentDir.path.isDirectory ==# 0
            let currentDir = currentDir.parent
        endif
    endif
    return currentDir
endfunction
"FUNCTION: TreeDirNode.getVisibleChildCount() {{{3
"Returns the number of visible children this node has
function! s:TreeDirNode.getVisibleChildCount()
    return len(self.getVisibleChildren())
endfunction

"FUNCTION: TreeDirNode.getVisibleChildren() {{{3
"Returns a list of children to display for this node, in the correct order
"
"Return:
"an array of treenodes
function! s:TreeDirNode.getVisibleChildren()
    let toReturn = []
    for i in self.children
        if i.path.ignore() ==# 0
            call add(toReturn, i)
        endif
    endfor
    return toReturn
endfunction

"FUNCTION: TreeDirNode.hasVisibleChildren() {{{3
"returns 1 if this node has any childre, 0 otherwise..
function! s:TreeDirNode.hasVisibleChildren()
    return self.getVisibleChildCount() != 0
endfunction

"FUNCTION: TreeDirNode._initChildren() {{{3
"Removes all childen from this node and re-reads them
"
"Args:
"silent: 1 if the function should not echo any "please wait" messages for
"large directories
"
"Return: the number of child nodes read
function! s:TreeDirNode._initChildren(silent)
    "remove all the current child nodes
    let self.children = []

    "get an array of all the files in the nodes dir
    let dir = self.path
    let globDir = dir.str({'format': 'Glob'})
    let filesStr = globpath(globDir, '*',1) . "\n" . globpath(globDir, '.*',1)
    let files = split(filesStr, "\n")

    if !a:silent && len(files) > g:NERDTreeNotificationThreshold
        call s:echo("Please wait, caching a large dir ...")
    endif

    let invalidFilesFound = 0
    for i in files

        "filter out the .. and . directories
        "Note: we must match .. AND ../ cos sometimes the globpath returns
        "../ for path with strange chars (eg $)
        if i !~# '\/\.\.\/\?$' && i !~# '\/\.\/\?$'

            "put the next file in a new node and attach it
            try
                let path = s:Path.New(i)
                call self.createChild(path, 0)
            catch /^NERDTree.\(InvalidArguments\|InvalidFiletype\)Error/
                let invalidFilesFound += 1
            endtry
        endif
    endfor

    call self.sortChildren()

    if !a:silent && len(files) > g:NERDTreeNotificationThreshold
        call s:echo("Please wait, caching a large dir ... DONE (". self.getChildCount() ." nodes cached).")
    endif

    if invalidFilesFound
        call s:echoWarning(invalidFilesFound . " file(s) could not be loaded into the NERD tree")
    endif
    return self.getChildCount()
endfunction
"FUNCTION: TreeDirNode.New(path) {{{3
"Returns a new TreeNode object with the given path and parent
"
"Args:
"path: a path object representing the full filesystem path to the file/dir that the node represents
unlet s:TreeDirNode.New
function! s:TreeDirNode.New(path)
    if a:path.isDirectory != 1
        throw "NERDTree.InvalidArgumentsError: A TreeDirNode object must be instantiated with a directory Path object."
    endif

    let newTreeNode = copy(self)
    let newTreeNode.path = a:path

    let newTreeNode.isOpen = 0
    let newTreeNode.children = []

    let newTreeNode.parent = {}

    return newTreeNode
endfunction
"FUNCTION: TreeDirNode.open([opts]) {{{3
"Open the dir in the current tree or in a new tree elsewhere.
"
"If opening in the current tree, return the number of cached nodes.
unlet s:TreeDirNode.open
function! s:TreeDirNode.open(...)
    let opts = a:0 ? a:1 : {}

    if has_key(opts, 'where') && !empty(opts['where'])
        let opener = s:Opener.New(self.path, opts)
        call opener.open(self)
    else
        let self.isOpen = 1
        if self.children ==# []
            return self._initChildren(0)
        else
            return 0
        endif
    endif
endfunction
"FUNCTION: TreeDirNode.openAlong([opts]) {{{3
"recursive open the dir if it has only one directory child.
"
"return the level of opened directories.
function! s:TreeDirNode.openAlong(...)
    let opts = a:0 ? a:1 : {}
    let level = 0

    let node = self
    while node.path.isDirectory
        call node.open(opts)
        let level += 1
        if node.getVisibleChildCount() == 1
            let node = node.getChildByIndex(0, 1)
        else
            break
        endif
    endwhile
    return level
endfunction
" FUNCTION: TreeDirNode.openExplorer() {{{3
" opens an explorer window for this node in the previous window (could be a
" nerd tree or a netrw)
function! s:TreeDirNode.openExplorer()
    call self.open({'where': 'p'})
endfunction
"FUNCTION: TreeDirNode.openInNewTab(options) {{{3
unlet s:TreeDirNode.openInNewTab
function! s:TreeDirNode.openInNewTab(options)
    call s:deprecated('TreeDirNode.openInNewTab', 'is deprecated, use open() instead')
    call self.open({'where': 't'})
endfunction
"FUNCTION: TreeDirNode._openInNewTab() {{{3
function! s:TreeDirNode._openInNewTab()
    tabnew
    call s:initNerdTree(self.path.str())
endfunction
"FUNCTION: TreeDirNode.openRecursively() {{{3
"Opens this treenode and all of its children whose paths arent 'ignored'
"because of the file filters.
"
"This method is actually a wrapper for the OpenRecursively2 method which does
"the work.
function! s:TreeDirNode.openRecursively()
    call self._openRecursively2(1)
endfunction

"FUNCTION: TreeDirNode._openRecursively2() {{{3
"Opens this all children of this treenode recursively if either:
"   *they arent filtered by file filters
"   *a:forceOpen is 1
"
"Args:
"forceOpen: 1 if this node should be opened regardless of file filters
function! s:TreeDirNode._openRecursively2(forceOpen)
    if self.path.ignore() ==# 0 || a:forceOpen
        let self.isOpen = 1
        if self.children ==# []
            call self._initChildren(1)
        endif

        for i in self.children
            if i.path.isDirectory ==# 1
                call i._openRecursively2(0)
            endif
        endfor
    endif
endfunction

"FUNCTION: TreeDirNode.refresh() {{{3
unlet s:TreeDirNode.refresh
function! s:TreeDirNode.refresh()
    call self.path.refresh()

    "if this node was ever opened, refresh its children
    if self.isOpen || !empty(self.children)
        "go thru all the files/dirs under this node
        let newChildNodes = []
        let invalidFilesFound = 0
        let dir = self.path
        let globDir = dir.str({'format': 'Glob'})
        let filesStr = globpath(globDir, '*') . "\n" . globpath(globDir, '.*')
        let files = split(filesStr, "\n")
        for i in files
            "filter out the .. and . directories
            "Note: we must match .. AND ../ cos sometimes the globpath returns
            "../ for path with strange chars (eg $)
            if i !~# '\/\.\.\/\?$' && i !~# '\/\.\/\?$'

                try
                    "create a new path and see if it exists in this nodes children
                    let path = s:Path.New(i)
                    let newNode = self.getChild(path)
                    if newNode != {}
                        call newNode.refresh()
                        call add(newChildNodes, newNode)

                    "the node doesnt exist so create it
                    else
                        let newNode = s:TreeFileNode.New(path)
                        let newNode.parent = self
                        call add(newChildNodes, newNode)
                    endif


                catch /^NERDTree.InvalidArgumentsError/
                    let invalidFilesFound = 1
                endtry
            endif
        endfor

        "swap this nodes children out for the children we just read/refreshed
        let self.children = newChildNodes
        call self.sortChildren()

        if invalidFilesFound
            call s:echoWarning("some files could not be loaded into the NERD tree")
        endif
    endif
endfunction

"FUNCTION: TreeDirNode.reveal(path) {{{3
"reveal the given path, i.e. cache and open all treenodes needed to display it
"in the UI
function! s:TreeDirNode.reveal(path)
    if !a:path.isUnder(self.path)
        throw "NERDTree.InvalidArgumentsError: " . a:path.str() . " should be under " . self.path.str()
    endif

    call self.open()

    if self.path.equals(a:path.getParent())
        let n = self.findNode(a:path)
        call s:renderView()
        call n.putCursorHere(1,0)
        return
    endif

    let p = a:path
    while !p.getParent().equals(self.path)
        let p = p.getParent()
    endwhile

    let n = self.findNode(p)
    call n.reveal(a:path)
endfunction
"FUNCTION: TreeDirNode.removeChild(treenode) {{{3
"
"Removes the given treenode from this nodes set of children
"
"Args:
"treenode: the node to remove
"
"Throws a NERDTree.ChildNotFoundError if the given treenode is not found
function! s:TreeDirNode.removeChild(treenode)
    for i in range(0, self.getChildCount()-1)
        if self.children[i].equals(a:treenode)
            call remove(self.children, i)
            return
        endif
    endfor

    throw "NERDTree.ChildNotFoundError: child node was not found"
endfunction

"FUNCTION: TreeDirNode.sortChildren() {{{3
"
"Sorts the children of this node according to alphabetical order and the
"directory priority.
"
function! s:TreeDirNode.sortChildren()
    let CompareFunc = function("s:compareNodes")
    call sort(self.children, CompareFunc)
endfunction

"FUNCTION: TreeDirNode.toggleOpen([options]) {{{3
"Opens this directory if it is closed and vice versa
function! s:TreeDirNode.toggleOpen(...)
    let opts = a:0 ? a:1 : {}
    if self.isOpen ==# 1
        call self.close()
    else
        if g:NERDTreeCasadeOpenSingleChildDir == 0
            call self.open(opts)
        else
            call self.openAlong(opts)
        endif
    endif
endfunction
"FUNCTION: TreeDirNode.transplantChild(newNode) {{{3
"Replaces the child of this with the given node (where the child node's full
"path matches a:newNode's fullpath). The search for the matching node is
"non-recursive
"
"Arg:
"newNode: the node to graft into the tree
function! s:TreeDirNode.transplantChild(newNode)
    for i in range(0, self.getChildCount()-1)
        if self.children[i].equals(a:newNode)
            let self.children[i] = a:newNode
            let a:newNode.parent = self
            break
        endif
    endfor
endfunction
"============================================================
"CLASS: Opener {{{2
"============================================================
let s:Opener = {}

"FUNCTION: Opener._checkToCloseTree(newtab) {{{3
"Check the class options and global options (i.e. NERDTreeQuitOnOpen) to see
"if the tree should be closed now.
"
"Args:
"a:newtab - boolean. If set, only close the tree now if we are opening the
"target in a new tab. This is needed because we have to close tree before we
"leave the tab
function! s:Opener._checkToCloseTree(newtab)
    if self._keepopen
        return
    endif

    if (a:newtab && self._where == 't') || !a:newtab
        call s:closeTreeIfQuitOnOpen()
    endif
endfunction

"FUNCTION: Opener._gotoTargetWin() {{{3
function! s:Opener._gotoTargetWin()
    if b:NERDTreeType ==# "secondary"
        if self._where == 'v'
            vsplit
        elseif self._where == 'h'
            split
        elseif self._where == 't'
            tabnew
        endif
    else
        call self._checkToCloseTree(1)

        if self._where == 'v'
            call self._newVSplit()
        elseif self._where == 'h'
            call self._newSplit()
        elseif self._where == 't'
            tabnew
        elseif self._where == 'p'
            call self._previousWindow()
        endif

        call self._checkToCloseTree(0)
    endif
endfunction

"FUNCTION: Opener.New(path, opts) {{{3
"Args:
"
"a:path: The path object that is to be opened.
"
"a:opts:
"
"A dictionary containing the following keys (all optional):
"  'where': Specifies whether the node should be opened in new split/tab or in
"           the previous window. Can be either 'v' or 'h' or 't' (for open in
"           new tab)
"  'reuse': if a window is displaying the file then jump the cursor there
"  'keepopen': dont close the tree window
"  'stay': open the file, but keep the cursor in the tree win
function! s:Opener.New(path, opts)
    let newObj = copy(self)

    let newObj._path = a:path
    let newObj._stay = s:has_opt(a:opts, 'stay')
    let newObj._reuse = s:has_opt(a:opts, 'reuse')
    let newObj._keepopen = s:has_opt(a:opts, 'keepopen')
    let newObj._where = has_key(a:opts, 'where') ? a:opts['where'] : ''
    let newObj._treetype = b:NERDTreeType
    call newObj._saveCursorPos()

    return newObj
endfunction

"FUNCTION: Opener._newSplit() {{{3
function! s:Opener._newSplit()
    " Save the user's settings for splitbelow and splitright
    let savesplitbelow=&splitbelow
    let savesplitright=&splitright

    " 'there' will be set to a command to move from the split window
    " back to the explorer window
    "
    " 'back' will be set to a command to move from the explorer window
    " back to the newly split window
    "
    " 'right' and 'below' will be set to the settings needed for
    " splitbelow and splitright IF the explorer is the only window.
    "
    let there= g:NERDTreeWinPos ==# "left" ? "wincmd h" : "wincmd l"
    let back = g:NERDTreeWinPos ==# "left" ? "wincmd l" : "wincmd h"
    let right= g:NERDTreeWinPos ==# "left"
    let below=0

    " Attempt to go to adjacent window
    call s:exec(back)

    let onlyOneWin = (winnr("$") ==# 1)

    " If no adjacent window, set splitright and splitbelow appropriately
    if onlyOneWin
        let &splitright=right
        let &splitbelow=below
    else
        " found adjacent window - invert split direction
        let &splitright=!right
        let &splitbelow=!below
    endif

    let splitMode = onlyOneWin ? "vertical" : ""

    " Open the new window
    try
        exec(splitMode." sp ")
    catch /^Vim\%((\a\+)\)\=:E37/
        call s:putCursorInTreeWin()
        throw "NERDTree.FileAlreadyOpenAndModifiedError: ". self._path.str() ." is already open and modified."
    catch /^Vim\%((\a\+)\)\=:/
        "do nothing
    endtry

    "resize the tree window if no other window was open before
    if onlyOneWin
        let size = exists("b:NERDTreeOldWindowSize") ? b:NERDTreeOldWindowSize : g:NERDTreeWinSize
        call s:exec(there)
        exec("silent ". splitMode ." resize ". size)
        call s:exec('wincmd p')
    endif

    " Restore splitmode settings
    let &splitbelow=savesplitbelow
    let &splitright=savesplitright
endfunction

"FUNCTION: Opener._newVSplit() {{{3
function! s:Opener._newVSplit()
    let winwidth = winwidth(".")
    if winnr("$")==#1
        let winwidth = g:NERDTreeWinSize
    endif

    call s:exec("wincmd p")
    vnew

    "resize the nerd tree back to the original size
    call s:putCursorInTreeWin()
    exec("silent vertical resize ". winwidth)
    call s:exec('wincmd p')
endfunction

"FUNCTION: Opener.open(target) {{{3
function! s:Opener.open(target)
    if self._path.isDirectory
        call self._openDirectory(a:target)
    else
        call self._openFile()
    endif
endfunction

"FUNCTION: Opener._openFile() {{{3
function! s:Opener._openFile()
    if self._reuse && self._reuseWindow()
        return
    endif

    call self._gotoTargetWin()

    if self._treetype ==# "secondary"
        call self._path.edit()
    else
        call self._path.edit()


        if self._stay
            call self._restoreCursorPos()
        endif
    endif
endfunction

"FUNCTION: Opener._openDirectory(node) {{{3
function! s:Opener._openDirectory(node)
    if self._treetype ==# "secondary"
        call self._gotoTargetWin()
        call s:initNerdTreeInPlace(a:node.path.str())
    else
        call self._gotoTargetWin()
        if empty(self._where)
            call a:node.makeRoot()
            call s:renderView()
            call a:node.putCursorHere(0, 0)
        elseif self._where == 't'
            call s:initNerdTree(a:node.path.str())
        else
            call s:initNerdTreeInPlace(a:node.path.str())
        endif
    endif

    if self._stay
        call self._restoreCursorPos()
    endif
endfunction

"FUNCTION: Opener._previousWindow() {{{3
function! s:Opener._previousWindow()
    if !s:isWindowUsable(winnr("#")) && s:firstUsableWindow() ==# -1
        call self._newSplit()
    else
        try
            if !s:isWindowUsable(winnr("#"))
                call s:exec(s:firstUsableWindow() . "wincmd w")
            else
                call s:exec('wincmd p')
            endif
        catch /^Vim\%((\a\+)\)\=:E37/
            call s:putCursorInTreeWin()
            throw "NERDTree.FileAlreadyOpenAndModifiedError: ". self._path.str() ." is already open and modified."
        catch /^Vim\%((\a\+)\)\=:/
            echo v:exception
        endtry
    endif
endfunction

"FUNCTION: Opener._restoreCursorPos(){{{3
function! s:Opener._restoreCursorPos()
    call s:exec('normal ' . self._tabnr . 'gt')
    call s:exec(bufwinnr(self._bufnr) . 'wincmd w')
endfunction

"FUNCTION: Opener._reuseWindow(){{{3
"put the cursor in the first window we find for this file
"
"return 1 if we were successful
function! s:Opener._reuseWindow()
    "check the current tab for the window
    let winnr = bufwinnr('^' . self._path.str() . '$')
    if winnr != -1
        call s:exec(winnr . "wincmd w")
        call self._checkToCloseTree(0)
        return 1
    else
        "check other tabs
        let tabnr = self._path.tabnr()
        if tabnr
            call self._checkToCloseTree(1)
            call s:exec('normal! ' . tabnr . 'gt')
            let winnr = bufwinnr('^' . self._path.str() . '$')
            call s:exec(winnr . "wincmd w")
            return 1
        endif
    endif
    return 0
endfunction

"FUNCTION: Opener._saveCursorPos(){{{3
function! s:Opener._saveCursorPos()
    let self._bufnr = bufnr("")
    let self._tabnr = tabpagenr()
endfunction

"CLASS: Path {{{2
"============================================================
let s:Path = {}
"FUNCTION: Path.AbsolutePathFor(str) {{{3
function! s:Path.AbsolutePathFor(str)
    let prependCWD = 0
    if s:running_windows
        let prependCWD = a:str !~# '^.:\(\\\|\/\)' && a:str !~# '^\(\\\\\|\/\/\)'
    else
        let prependCWD = a:str !~# '^/'
    endif

    let toReturn = a:str
    if prependCWD
        let toReturn = getcwd() . s:Path.Slash() . a:str
    endif

    return toReturn
endfunction
"FUNCTION: Path.bookmarkNames() {{{3
function! s:Path.bookmarkNames()
    if !exists("self._bookmarkNames")
        call self.cacheDisplayString()
    endif
    return self._bookmarkNames
endfunction
"FUNCTION: Path.cacheDisplayString() {{{3
function! s:Path.cacheDisplayString()
    let self.cachedDisplayString = self.getLastPathComponent(1)

    if self.isExecutable
        let self.cachedDisplayString = self.cachedDisplayString . '*'
    endif

    let self._bookmarkNames = []
    for i in s:Bookmark.Bookmarks()
        if i.path.equals(self)
            call add(self._bookmarkNames, i.name)
        endif
    endfor
    if !empty(self._bookmarkNames)
        let self.cachedDisplayString .= ' {' . join(self._bookmarkNames) . '}'
    endif

    if self.isSymLink
        let self.cachedDisplayString .=  ' -> ' . self.symLinkDest
    endif

    if self.isReadOnly
        let self.cachedDisplayString .=  ' [RO]'
    endif
endfunction
"FUNCTION: Path.changeToDir() {{{3
function! s:Path.changeToDir()
    let dir = self.str({'format': 'Cd'})
    if self.isDirectory ==# 0
        let dir = self.getParent().str({'format': 'Cd'})
    endif

    try
        execute "cd " . dir
        call s:echo("CWD is now: " . getcwd())
    catch
        throw "NERDTree.PathChangeError: cannot change CWD to " . dir
    endtry
endfunction

"FUNCTION: Path.compareTo() {{{3
"
"Compares this Path to the given path and returns 0 if they are equal, -1 if
"this Path is "less than" the given path, or 1 if it is "greater".
"
"Args:
"path: the path object to compare this to
"
"Return:
"1, -1 or 0
function! s:Path.compareTo(path)
    let thisPath = self.getLastPathComponent(1)
    let thatPath = a:path.getLastPathComponent(1)

    "if the paths are the same then clearly we return 0
    if thisPath ==# thatPath
        return 0
    endif

    let thisSS = self.getSortOrderIndex()
    let thatSS = a:path.getSortOrderIndex()

    "compare the sort sequences, if they are different then the return
    "value is easy
    if thisSS < thatSS
        return -1
    elseif thisSS > thatSS
        return 1
    else
        "if the sort sequences are the same then compare the paths
        "alphabetically
        let pathCompare = g:NERDTreeCaseSensitiveSort ? thisPath <# thatPath : thisPath <? thatPath
        if pathCompare
            return -1
        else
            return 1
        endif
    endif
endfunction

"FUNCTION: Path.Create(fullpath) {{{3
"
"Factory method.
"
"Creates a path object with the given path. The path is also created on the
"filesystem. If the path already exists, a NERDTree.Path.Exists exception is
"thrown. If any other errors occur, a NERDTree.Path exception is thrown.
"
"Args:
"fullpath: the full filesystem path to the file/dir to create
function! s:Path.Create(fullpath)
    "bail if the a:fullpath already exists
    if isdirectory(a:fullpath) || filereadable(a:fullpath)
        throw "NERDTree.CreatePathError: Directory Exists: '" . a:fullpath . "'"
    endif

    try

        "if it ends with a slash, assume its a dir create it
        if a:fullpath =~# '\(\\\|\/\)$'
            "whack the trailing slash off the end if it exists
            let fullpath = substitute(a:fullpath, '\(\\\|\/\)$', '', '')

            call mkdir(fullpath, 'p')

        "assume its a file and create
        else
            call writefile([], a:fullpath)
        endif
    catch
        throw "NERDTree.CreatePathError: Could not create path: '" . a:fullpath . "'"
    endtry

    return s:Path.New(a:fullpath)
endfunction

"FUNCTION: Path.copy(dest) {{{3
"
"Copies the file/dir represented by this Path to the given location
"
"Args:
"dest: the location to copy this dir/file to
function! s:Path.copy(dest)
    if !s:Path.CopyingSupported()
        throw "NERDTree.CopyingNotSupportedError: Copying is not supported on this OS"
    endif

    let dest = s:Path.WinToUnixPath(a:dest)

    let cmd = g:NERDTreeCopyCmd . " " . escape(self.str(), s:escape_chars) . " " . escape(dest, s:escape_chars)
    let success = system(cmd)
    if success != 0
        throw "NERDTree.CopyError: Could not copy ''". self.str() ."'' to: '" . a:dest . "'"
    endif
endfunction

"FUNCTION: Path.CopyingSupported() {{{3
"
"returns 1 if copying is supported for this OS
function! s:Path.CopyingSupported()
    return exists('g:NERDTreeCopyCmd')
endfunction


"FUNCTION: Path.copyingWillOverwrite(dest) {{{3
"
"returns 1 if copy this path to the given location will cause files to
"overwritten
"
"Args:
"dest: the location this path will be copied to
function! s:Path.copyingWillOverwrite(dest)
    if filereadable(a:dest)
        return 1
    endif

    if isdirectory(a:dest)
        let path = s:Path.JoinPathStrings(a:dest, self.getLastPathComponent(0))
        if filereadable(path)
            return 1
        endif
    endif
endfunction

"FUNCTION: Path.delete() {{{3
"
"Deletes the file represented by this path.
"Deletion of directories is not supported
"
"Throws NERDTree.Path.Deletion exceptions
function! s:Path.delete()
    if self.isDirectory

        let cmd = g:NERDTreeRemoveDirCmd . self.str({'escape': 1})
        let success = system(cmd)

        if v:shell_error != 0
            throw "NERDTree.PathDeletionError: Could not delete directory: '" . self.str() . "'"
        endif
    else
        let success = delete(self.str())
        if success != 0
            throw "NERDTree.PathDeletionError: Could not delete file: '" . self.str() . "'"
        endif
    endif

    "delete all bookmarks for this path
    for i in self.bookmarkNames()
        let bookmark = s:Bookmark.BookmarkFor(i)
        call bookmark.delete()
    endfor
endfunction

"FUNCTION: Path.displayString() {{{3
"
"Returns a string that specifies how the path should be represented as a
"string
function! s:Path.displayString()
    if self.cachedDisplayString ==# ""
        call self.cacheDisplayString()
    endif

    return self.cachedDisplayString
endfunction
"FUNCTION: Path.edit() {{{3
function! s:Path.edit()
    exec "edit " . self.str({'format': 'Edit'})
endfunction
"FUNCTION: Path.extractDriveLetter(fullpath) {{{3
"
"If running windows, cache the drive letter for this path
function! s:Path.extractDriveLetter(fullpath)
    if s:running_windows
        if a:fullpath =~ '^\(\\\\\|\/\/\)'
            "For network shares, the 'drive' consists of the first two parts of the path, i.e. \\boxname\share
            let self.drive = substitute(a:fullpath, '^\(\(\\\\\|\/\/\)[^\\\/]*\(\\\|\/\)[^\\\/]*\).*', '\1', '')
            let self.drive = substitute(self.drive, '/', '\', "g")
        else
            let self.drive = substitute(a:fullpath, '\(^[a-zA-Z]:\).*', '\1', '')
        endif
    else
        let self.drive = ''
    endif

endfunction
"FUNCTION: Path.exists() {{{3
"return 1 if this path points to a location that is readable or is a directory
function! s:Path.exists()
    let p = self.str()
    return filereadable(p) || isdirectory(p)
endfunction
"FUNCTION: Path.getDir() {{{3
"
"Returns this path if it is a directory, else this paths parent.
"
"Return:
"a Path object
function! s:Path.getDir()
    if self.isDirectory
        return self
    else
        return self.getParent()
    endif
endfunction
"FUNCTION: Path.getParent() {{{3
"
"Returns a new path object for this paths parent
"
"Return:
"a new Path object
function! s:Path.getParent()
    if s:running_windows
        let path = self.drive . '\' . join(self.pathSegments[0:-2], '\')
    else
        let path = '/'. join(self.pathSegments[0:-2], '/')
    endif

    return s:Path.New(path)
endfunction
"FUNCTION: Path.getLastPathComponent(dirSlash) {{{3
"
"Gets the last part of this path.
"
"Args:
"dirSlash: if 1 then a trailing slash will be added to the returned value for
"directory nodes.
function! s:Path.getLastPathComponent(dirSlash)
    if empty(self.pathSegments)
        return ''
    endif
    let toReturn = self.pathSegments[-1]
    if a:dirSlash && self.isDirectory
        let toReturn = toReturn . '/'
    endif
    return toReturn
endfunction

"FUNCTION: Path.getSortOrderIndex() {{{3
"returns the index of the pattern in g:NERDTreeSortOrder that this path matches
function! s:Path.getSortOrderIndex()
    let i = 0
    while i < len(g:NERDTreeSortOrder)
        if  self.getLastPathComponent(1) =~# g:NERDTreeSortOrder[i]
            return i
        endif
        let i = i + 1
    endwhile
    return s:NERDTreeSortStarIndex
endfunction

"FUNCTION: Path.ignore() {{{3
"returns true if this path should be ignored
function! s:Path.ignore()
    "filter out the user specified paths to ignore
    if b:NERDTreeIgnoreEnabled
        for i in g:NERDTreeIgnore
            if self._ignorePatternMatches(i)
                return 1
            endif
        endfor
    endif

    "dont show hidden files unless instructed to
    if b:NERDTreeShowHidden ==# 0 && self.getLastPathComponent(0) =~# '^\.'
        return 1
    endif

    if b:NERDTreeShowFiles ==# 0 && self.isDirectory ==# 0
        return 1
    endif

    if exists("*NERDTreeCustomIgnoreFilter") && NERDTreeCustomIgnoreFilter(self)
        return 1
    endif

    return 0
endfunction

"FUNCTION: Path._ignorePatternMatches(pattern) {{{3
"returns true if this path matches the given ignore pattern
function! s:Path._ignorePatternMatches(pattern)
    let pat = a:pattern
    if strpart(pat,len(pat)-7) == '[[dir]]'
        if !self.isDirectory
            return 0
        endif
        let pat = strpart(pat,0, len(pat)-7)
    elseif strpart(pat,len(pat)-8) == '[[file]]'
        if self.isDirectory
            return 0
        endif
        let pat = strpart(pat,0, len(pat)-8)
    endif

    return self.getLastPathComponent(0) =~# pat
endfunction
"FUNCTION: Path.isUnder(path) {{{3
"return 1 if this path is somewhere under the given path in the filesystem.
"
"a:path should be a dir
function! s:Path.isUnder(path)
    if a:path.isDirectory == 0
        return 0
    endif

    let this = self.str()
    let that = a:path.str()
    return stridx(this, that . s:Path.Slash()) == 0
endfunction

"FUNCTION: Path.JoinPathStrings(...) {{{3
function! s:Path.JoinPathStrings(...)
    let components = []
    for i in a:000
        let components = extend(components, split(i, '/'))
    endfor
    return '/' . join(components, '/')
endfunction

"FUNCTION: Path.equals() {{{3
"
"Determines whether 2 path objects are "equal".
"They are equal if the paths they represent are the same
"
"Args:
"path: the other path obj to compare this with
function! s:Path.equals(path)
    return self.str() ==# a:path.str()
endfunction

"FUNCTION: Path.New() {{{3
"The Constructor for the Path object
function! s:Path.New(path)
    let newPath = copy(self)

    call newPath.readInfoFromDisk(s:Path.AbsolutePathFor(a:path))

    let newPath.cachedDisplayString = ""

    return newPath
endfunction

"FUNCTION: Path.Slash() {{{3
"return the slash to use for the current OS
function! s:Path.Slash()
    return s:running_windows ? '\' : '/'
endfunction

"FUNCTION: Path.Resolve() {{{3
"Invoke the vim resolve() function and return the result
"This is necessary because in some versions of vim resolve() removes trailing
"slashes while in other versions it doesn't.  This always removes the trailing
"slash
function! s:Path.Resolve(path)
    let tmp = resolve(a:path)
    return tmp =~# '/$' ? substitute(tmp, '/$', '', '') : tmp
endfunction

"FUNCTION: Path.readInfoFromDisk(fullpath) {{{3
"
"
"Throws NERDTree.Path.InvalidArguments exception.
function! s:Path.readInfoFromDisk(fullpath)
    call self.extractDriveLetter(a:fullpath)

    let fullpath = s:Path.WinToUnixPath(a:fullpath)

    if getftype(fullpath) ==# "fifo"
        throw "NERDTree.InvalidFiletypeError: Cant handle FIFO files: " . a:fullpath
    endif

    let self.pathSegments = split(fullpath, '/')

    let self.isReadOnly = 0
    if isdirectory(a:fullpath)
        let self.isDirectory = 1
    elseif filereadable(a:fullpath)
        let self.isDirectory = 0
        let self.isReadOnly = filewritable(a:fullpath) ==# 0
    else
        throw "NERDTree.InvalidArgumentsError: Invalid path = " . a:fullpath
    endif

    let self.isExecutable = 0
    if !self.isDirectory
        let self.isExecutable = getfperm(a:fullpath) =~# 'x'
    endif

    "grab the last part of the path (minus the trailing slash)
    let lastPathComponent = self.getLastPathComponent(0)

    "get the path to the new node with the parent dir fully resolved
    let hardPath = s:Path.Resolve(self.strTrunk()) . '/' . lastPathComponent

    "if  the last part of the path is a symlink then flag it as such
    let self.isSymLink = (s:Path.Resolve(hardPath) != hardPath)
    if self.isSymLink
        let self.symLinkDest = s:Path.Resolve(fullpath)

        "if the link is a dir then slap a / on the end of its dest
        if isdirectory(self.symLinkDest)

            "we always wanna treat MS windows shortcuts as files for
            "simplicity
            if hardPath !~# '\.lnk$'

                let self.symLinkDest = self.symLinkDest . '/'
            endif
        endif
    endif
endfunction

"FUNCTION: Path.refresh() {{{3
function! s:Path.refresh()
    call self.readInfoFromDisk(self.str())
    call self.cacheDisplayString()
endfunction

"FUNCTION: Path.rename() {{{3
"
"Renames this node on the filesystem
function! s:Path.rename(newPath)
    if a:newPath ==# ''
        throw "NERDTree.InvalidArgumentsError: Invalid newPath for renaming = ". a:newPath
    endif

    let success =  rename(self.str(), a:newPath)
    if success != 0
        throw "NERDTree.PathRenameError: Could not rename: '" . self.str() . "'" . 'to:' . a:newPath
    endif
    call self.readInfoFromDisk(a:newPath)

    for i in self.bookmarkNames()
        let b = s:Bookmark.BookmarkFor(i)
        call b.setPath(copy(self))
    endfor
    call s:Bookmark.Write()
endfunction

"FUNCTION: Path.str() {{{3
"
"Returns a string representation of this Path
"
"Takes an optional dictionary param to specify how the output should be
"formatted.
"
"The dict may have the following keys:
"  'format'
"  'escape'
"  'truncateTo'
"
"The 'format' key may have a value of:
"  'Cd' - a string to be used with the :cd command
"  'Edit' - a string to be used with :e :sp :new :tabedit etc
"  'UI' - a string used in the NERD tree UI
"
"The 'escape' key, if specified will cause the output to be escaped with
"shellescape()
"
"The 'truncateTo' key causes the resulting string to be truncated to the value
"'truncateTo' maps to. A '<' char will be prepended.
function! s:Path.str(...)
    let options = a:0 ? a:1 : {}
    let toReturn = ""

    if has_key(options, 'format')
        let format = options['format']
        if has_key(self, '_strFor' . format)
            exec 'let toReturn = self._strFor' . format . '()'
        else
            raise 'NERDTree.UnknownFormatError: unknown format "'. format .'"'
        endif
    else
        let toReturn = self._str()
    endif

    if s:has_opt(options, 'escape')
        let toReturn = shellescape(toReturn)
    endif

    if has_key(options, 'truncateTo')
        let limit = options['truncateTo']
        if len(toReturn) > limit
            let toReturn = "<" . strpart(toReturn, len(toReturn) - limit + 1)
        endif
    endif

    return toReturn
endfunction

"FUNCTION: Path._strForUI() {{{3
function! s:Path._strForUI()
    let toReturn = '/' . join(self.pathSegments, '/')
    if self.isDirectory && toReturn != '/'
        let toReturn  = toReturn . '/'
    endif
    return toReturn
endfunction

"FUNCTION: Path._strForCd() {{{3
"
" returns a string that can be used with :cd
function! s:Path._strForCd()
    return escape(self.str(), s:escape_chars)
endfunction
"FUNCTION: Path._strForEdit() {{{3
"
"Return: the string for this path that is suitable to be used with the :edit
"command
function! s:Path._strForEdit()
    let p = escape(self.str({'format': 'UI'}), s:escape_chars)
    let cwd = getcwd() . s:Path.Slash()

    "return a relative path if we can
    let isRelative = 0
    if s:running_windows
        let isRelative = stridx(tolower(p), tolower(cwd)) == 0
    else
        let isRelative = stridx(p, cwd) == 0
    endif

    if isRelative
        let p = strpart(p, strlen(cwd))

        "handle the edge case where the file begins with a + (vim interprets
        "the +foo in `:e +foo` as an option to :edit)
        if p[0] == "+"
            let p = '\' . p
        endif
    endif

    if p ==# ''
        let p = '.'
    endif

    return p
endfunction
"FUNCTION: Path._strForGlob() {{{3
function! s:Path._strForGlob()
    let lead = s:Path.Slash()

    "if we are running windows then slap a drive letter on the front
    if s:running_windows
        let lead = self.drive . '\'
    endif

    let toReturn = lead . join(self.pathSegments, s:Path.Slash())

    if !s:running_windows
        let toReturn = escape(toReturn, s:escape_chars)
    endif
    return toReturn
endfunction
"FUNCTION: Path._str() {{{3
"
"Gets the string path for this path object that is appropriate for the OS.
"EG, in windows c:\foo\bar
"    in *nix  /foo/bar
function! s:Path._str()
    let lead = s:Path.Slash()

    "if we are running windows then slap a drive letter on the front
    if s:running_windows
        let lead = self.drive . '\'
    endif

    return lead . join(self.pathSegments, s:Path.Slash())
endfunction

"FUNCTION: Path.strTrunk() {{{3
"Gets the path without the last segment on the end.
function! s:Path.strTrunk()
    return self.drive . '/' . join(self.pathSegments[0:-2], '/')
endfunction

" FUNCTION: Path.tabnr() {{{3
" return the number of the first tab that is displaying this file
"
" return 0 if no tab was found
function! s:Path.tabnr()
    let str = self.str()
    for t in range(tabpagenr('$'))
        for b in tabpagebuflist(t+1)
            if str == expand('#' . b . ':p')
                return t+1
            endif
        endfor
    endfor
    return 0
endfunction
"FUNCTION: Path.WinToUnixPath(pathstr){{{3
"Takes in a windows path and returns the unix equiv
"
"A class level method
"
"Args:
"pathstr: the windows path to convert
function! s:Path.WinToUnixPath(pathstr)
    if !s:running_windows
        return a:pathstr
    endif

    let toReturn = a:pathstr

    "remove the x:\ of the front
    let toReturn = substitute(toReturn, '^.*:\(\\\|/\)\?', '/', "")

    "remove the \\ network share from the front
    let toReturn = substitute(toReturn, '^\(\\\\\|\/\/\)[^\\\/]*\(\\\|\/\)[^\\\/]*\(\\\|\/\)\?', '/', "")

    "convert all \ chars to /
    let toReturn = substitute(toReturn, '\', '/', "g")

    return toReturn
endfunction

" SECTION: General Functions {{{1
"============================================================
"FUNCTION: s:bufInWindows(bnum){{{2
"[[STOLEN FROM VTREEEXPLORER.VIM]]
"Determine the number of windows open to this buffer number.
"Care of Yegappan Lakshman.  Thanks!
"
"Args:
"bnum: the subject buffers buffer number
function! s:bufInWindows(bnum)
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
endfunction " >>>
"FUNCTION: s:checkForBrowse(dir) {{{2
"inits a secondary nerd tree in the current buffer if appropriate
function! s:checkForBrowse(dir)
    if a:dir != '' && isdirectory(a:dir)
        call s:initNerdTreeInPlace(a:dir)
    endif
endfunction
"FUNCTION: s:compareBookmarks(first, second) {{{2
"Compares two bookmarks
function! s:compareBookmarks(first, second)
    return a:first.compareTo(a:second)
endfunction

" FUNCTION: s:completeBookmarks(A,L,P) {{{2
" completion function for the bookmark commands
function! s:completeBookmarks(A,L,P)
    return filter(s:Bookmark.BookmarkNames(), 'v:val =~# "^' . a:A . '"')
endfunction
" FUNCTION: s:createDefaultBindings() {{{2
function! s:createDefaultBindings()
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
" FUNCTION: s:deprecated(func, [msg]) {{{2
" Issue a deprecation warning for a:func. If a second arg is given, use this
" as the deprecation message
function! s:deprecated(func, ...)
    let msg = a:0 ? a:func . ' ' . a:1 : a:func . ' is deprecated'

    if !exists('s:deprecationWarnings')
        let s:deprecationWarnings = {}
    endif
    if !has_key(s:deprecationWarnings, a:func)
        let s:deprecationWarnings[a:func] = 1
        echomsg msg
    endif
endfunction
" FUNCTION: s:exec(cmd) {{{2
" same as :exec cmd  but eventignore=all is set for the duration
function! s:exec(cmd)
    let old_ei = &ei
    set ei=all
    exec a:cmd
    let &ei = old_ei
endfunction
" FUNCTION: s:findAndRevealPath() {{{2
function! s:findAndRevealPath()
    try
        let p = s:Path.New(expand("%:p"))
    catch /^NERDTree.InvalidArgumentsError/
        call s:echo("no file for the current buffer")
        return
    endtry

    if !s:treeExistsForTab()
        try
            let cwd = s:Path.New(getcwd())
        catch /^NERDTree.InvalidArgumentsError/
            call s:echo("current directory does not exist.")
            let cwd = p.getParent()
        endtry

        if p.isUnder(cwd)
            call s:initNerdTree(cwd.str())
        else
            call s:initNerdTree(p.getParent().str())
        endif
    else
        if !p.isUnder(s:TreeFileNode.GetRootForTab().path)
            call s:initNerdTree(p.getParent().str())
        else
            if !s:isTreeOpen()
                call s:toggle("")
            endif
        endif
    endif
    call s:putCursorInTreeWin()
    call b:NERDTreeRoot.reveal(p)
endfunction

" FUNCTION: s:has_opt(options, name) {{{2
function! s:has_opt(options, name)
    return has_key(a:options, a:name) && a:options[a:name] == 1
endfunction

"FUNCTION: s:initNerdTree(name) {{{2
"Initialise the nerd tree for this tab. The tree will start in either the
"given directory, or the directory associated with the given bookmark
"
"Args:
"name: the name of a bookmark or a directory
function! s:initNerdTree(name)
    let path = {}
    if s:Bookmark.BookmarkExistsFor(a:name)
        let path = s:Bookmark.BookmarkFor(a:name).path
    else
        let dir = a:name ==# '' ? getcwd() : a:name

        "hack to get an absolute path if a relative path is given
        if dir =~# '^\.'
            let dir = getcwd() . s:Path.Slash() . dir
        endif
        let dir = s:Path.Resolve(dir)

        try
            let path = s:Path.New(dir)
        catch /^NERDTree.InvalidArgumentsError/
            call s:echo("No bookmark or directory found for: " . a:name)
            return
        endtry
    endif
    if !path.isDirectory
        let path = path.getParent()
    endif

    "if instructed to, then change the vim CWD to the dir the NERDTree is
    "inited in
    if g:NERDTreeChDirMode != 0
        call path.changeToDir()
    endif

    if s:treeExistsForTab()
        if s:isTreeOpen()
            call s:closeTree()
        endif
        unlet t:NERDTreeBufName
    endif

    let newRoot = s:TreeDirNode.New(path)
    call newRoot.open()

    call s:createTreeWin()
    let b:treeShowHelp = 0
    let b:NERDTreeIgnoreEnabled = 1
    let b:NERDTreeShowFiles = g:NERDTreeShowFiles
    let b:NERDTreeShowHidden = g:NERDTreeShowHidden
    let b:NERDTreeShowBookmarks = g:NERDTreeShowBookmarks
    let b:NERDTreeRoot = newRoot
    let b:NERDTreeType = "primary"

    call s:renderView()
    call b:NERDTreeRoot.putCursorHere(0, 0)

    silent doautocmd User NERDTreeInit
endfunction

"FUNCTION: s:initNerdTreeInPlace(dir) {{{2
function! s:initNerdTreeInPlace(dir)
    try
        let path = s:Path.New(a:dir)
    catch /^NERDTree.InvalidArgumentsError/
        call s:echo("Invalid directory name:" . a:name)
        return
    endtry

    "we want the directory buffer to disappear when we do the :edit below
    setlocal bufhidden=wipe

    let previousBuf = expand("#")

    "we need a unique name for each secondary tree buffer to ensure they are
    "all independent
    exec "silent edit " . s:nextBufferName()

    let b:NERDTreePreviousBuf = bufnr(previousBuf)

    let b:NERDTreeRoot = s:TreeDirNode.New(path)
    call b:NERDTreeRoot.open()

    call s:setCommonBufOptions()
    let b:NERDTreeType = "secondary"

    call s:renderView()

    silent doautocmd User NERDTreeInit
endfunction
" FUNCTION: s:initNerdTreeMirror() {{{2
function! s:initNerdTreeMirror()

    "get the names off all the nerd tree buffers
    let treeBufNames = []
    for i in range(1, tabpagenr("$"))
        let nextName = s:tabpagevar(i, 'NERDTreeBufName')
        if nextName != -1 && (!exists("t:NERDTreeBufName") || nextName != t:NERDTreeBufName)
            call add(treeBufNames, nextName)
        endif
    endfor
    let treeBufNames = s:unique(treeBufNames)

    "map the option names (that the user will be prompted with) to the nerd
    "tree buffer names
    let options = {}
    let i = 0
    while i < len(treeBufNames)
        let bufName = treeBufNames[i]
        let treeRoot = getbufvar(bufName, "NERDTreeRoot")
        let options[i+1 . '. ' . treeRoot.path.str() . '  (buf name: ' . bufName . ')'] = bufName
        let i = i + 1
    endwhile

    "work out which tree to mirror, if there is more than 1 then ask the user
    let bufferName = ''
    if len(keys(options)) > 1
        let choices = ["Choose a tree to mirror"]
        let choices = extend(choices, sort(keys(options)))
        let choice = inputlist(choices)
        if choice < 1 || choice > len(options) || choice ==# ''
            return
        endif

        let bufferName = options[sort(keys(options))[choice-1]]
    elseif len(keys(options)) ==# 1
        let bufferName = values(options)[0]
    else
        call s:echo("No trees to mirror")
        return
    endif

    if s:treeExistsForTab() && s:isTreeOpen()
        call s:closeTree()
    endif

    let t:NERDTreeBufName = bufferName
    call s:createTreeWin()
    exec 'buffer ' .  bufferName
    if !&hidden
        call s:renderView()
    endif
endfunction
" FUNCTION: s:nextBufferName() {{{2
" returns the buffer name for the next nerd tree
function! s:nextBufferName()
    let name = s:NERDTreeBufName . s:next_buffer_number
    let s:next_buffer_number += 1
    return name
endfunction
" FUNCTION: s:postSourceActions() {{{2
function! s:postSourceActions()
    call s:Bookmark.CacheBookmarks(0)
    call s:createDefaultBindings()

    "load all nerdtree plugins
    runtime! nerdtree_plugin/**/*.vim
endfunction
" FUNCTION: s:tabpagevar(tabnr, var) {{{2
function! s:tabpagevar(tabnr, var)
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
" Function: s:treeExistsForBuffer()   {{{2
" Returns 1 if a nerd tree root exists in the current buffer
function! s:treeExistsForBuf()
    return exists("b:NERDTreeRoot")
endfunction
" Function: s:treeExistsForTab()   {{{2
" Returns 1 if a nerd tree root exists in the current tab
function! s:treeExistsForTab()
    return exists("t:NERDTreeBufName")
endfunction
" Function: s:SID()   {{{2
function s:SID()
    if !exists("s:sid")
        let s:sid = matchstr(expand('<sfile>'), '<SNR>\zs\d\+\ze_SID$')
    endif
    return s:sid
endfun
"FUNCTION: s:upDir(keepState) {{{2
"moves the tree up a level
"
"Args:
"keepState: 1 if the current root should be left open when the tree is
"re-rendered
function! s:upDir(keepState)
    let cwd = b:NERDTreeRoot.path.str({'format': 'UI'})
    if cwd ==# "/" || cwd =~# '^[^/]..$'
        call s:echo("already at top dir")
    else
        if !a:keepState
            call b:NERDTreeRoot.close()
        endif

        let oldRoot = b:NERDTreeRoot

        if empty(b:NERDTreeRoot.parent)
            let path = b:NERDTreeRoot.path.getParent()
            let newRoot = s:TreeDirNode.New(path)
            call newRoot.open()
            call newRoot.transplantChild(b:NERDTreeRoot)
            let b:NERDTreeRoot = newRoot
        else
            let b:NERDTreeRoot = b:NERDTreeRoot.parent
        endif

        if g:NERDTreeChDirMode ==# 2
            call b:NERDTreeRoot.path.changeToDir()
        endif

        call s:renderView()
        call oldRoot.putCursorHere(0, 0)
    endif
endfunction
" Function: s:unique(list)   {{{2
" returns a:list without duplicates
function! s:unique(list)
  let uniqlist = []
  for elem in a:list
    if index(uniqlist, elem) ==# -1
      let uniqlist += [elem]
    endif
  endfor
  return uniqlist
endfunction
" SECTION: Public API {{{1
"============================================================
let g:NERDTreePath = s:Path
let g:NERDTreeDirNode = s:TreeDirNode
let g:NERDTreeFileNode = s:TreeFileNode
let g:NERDTreeBookmark = s:Bookmark

function! NERDTreeAddMenuItem(options)
    call s:MenuItem.Create(a:options)
endfunction

function! NERDTreeAddMenuSeparator(...)
    let opts = a:0 ? a:1 : {}
    call s:MenuItem.CreateSeparator(opts)
endfunction

function! NERDTreeAddSubmenu(options)
    return s:MenuItem.Create(a:options)
endfunction

function! NERDTreeAddKeyMap(options)
    call s:KeyMap.Create(a:options)
endfunction

function! NERDTreeRender()
    call s:renderView()
endfunction

function! NERDTreeFocus()
    if s:isTreeOpen()
        call s:putCursorInTreeWin()
    else
        call s:toggle("")
    endif
endfunction

" SECTION: View Functions {{{1
"============================================================
"FUNCTION: s:centerView() {{{2
"centers the nerd tree window around the cursor (provided the nerd tree
"options permit)
function! s:centerView()
    if g:NERDTreeAutoCenter
        let current_line = winline()
        let lines_to_top = current_line
        let lines_to_bottom = winheight(s:getTreeWinNum()) - current_line
        if lines_to_top < g:NERDTreeAutoCenterThreshold || lines_to_bottom < g:NERDTreeAutoCenterThreshold
            normal! zz
        endif
    endif
endfunction
"FUNCTION: s:closeTree() {{{2
"Closes the primary NERD tree window for this tab
function! s:closeTree()
    if !s:isTreeOpen()
        throw "NERDTree.NoTreeFoundError: no NERDTree is open"
    endif

    if winnr("$") != 1
        if winnr() == s:getTreeWinNum()
            call s:exec("wincmd p")
            let bufnr = bufnr("")
            call s:exec("wincmd p")
        else
            let bufnr = bufnr("")
        endif

        call s:exec(s:getTreeWinNum() . " wincmd w")
        close
        call s:exec(bufwinnr(bufnr) . " wincmd w")
    else
        close
    endif
endfunction

"FUNCTION: s:closeTreeIfOpen() {{{2
"Closes the NERD tree window if it is open
function! s:closeTreeIfOpen()
   if s:isTreeOpen()
      call s:closeTree()
   endif
endfunction
"FUNCTION: s:closeTreeIfQuitOnOpen() {{{2
"Closes the NERD tree window if the close on open option is set
function! s:closeTreeIfQuitOnOpen()
    if g:NERDTreeQuitOnOpen && s:isTreeOpen()
        call s:closeTree()
    endif
endfunction
"FUNCTION: s:createTreeWin() {{{2
"Inits the NERD tree window. ie. opens it, sizes it, sets all the local
"options etc
function! s:createTreeWin()
    "create the nerd tree window
    let splitLocation = g:NERDTreeWinPos ==# "left" ? "topleft " : "botright "
    let splitSize = g:NERDTreeWinSize

    if !exists('t:NERDTreeBufName')
        let t:NERDTreeBufName = s:nextBufferName()
        silent! exec splitLocation . 'vertical ' . splitSize . ' new'
        silent! exec "edit " . t:NERDTreeBufName
    else
        silent! exec splitLocation . 'vertical ' . splitSize . ' split'
        silent! exec "buffer " . t:NERDTreeBufName
    endif

    setlocal winfixwidth
    call s:setCommonBufOptions()
endfunction

"FUNCTION: s:dumpHelp  {{{2
"prints out the quick help
function! s:dumpHelp()
    let old_h = @h
    if b:treeShowHelp ==# 1
        let @h=   "\" NERD tree (" . s:NERD_tree_version . ") quickhelp~\n"
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

        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Tree filtering mappings~\n"
        let @h=@h."\" ". g:NERDTreeMapToggleHidden .": hidden files (" . (b:NERDTreeShowHidden ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleFilters .": file filters (" . (b:NERDTreeIgnoreEnabled ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleFiles .": files (" . (b:NERDTreeShowFiles ? "on" : "off") . ")\n"
        let @h=@h."\" ". g:NERDTreeMapToggleBookmarks .": bookmarks (" . (b:NERDTreeShowBookmarks ? "on" : "off") . ")\n"

        "add quickhelp entries for each custom key map
        let @h=@h."\"\n\" ----------------------------\n"
        let @h=@h."\" Custom mappings~\n"
        for i in s:KeyMap.All()
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
"FUNCTION: s:echo  {{{2
"A wrapper for :echo. Appends 'NERDTree:' on the front of all messages
"
"Args:
"msg: the message to echo
function! s:echo(msg)
    redraw
    echomsg "NERDTree: " . a:msg
endfunction
"FUNCTION: s:echoWarning {{{2
"Wrapper for s:echo, sets the message type to warningmsg for this message
"Args:
"msg: the message to echo
function! s:echoWarning(msg)
    echohl warningmsg
    call s:echo(a:msg)
    echohl normal
endfunction
"FUNCTION: s:echoError {{{2
"Wrapper for s:echo, sets the message type to errormsg for this message
"Args:
"msg: the message to echo
function! s:echoError(msg)
    echohl errormsg
    call s:echo(a:msg)
    echohl normal
endfunction
"FUNCTION: s:firstUsableWindow(){{{2
"find the window number of the first normal window
function! s:firstUsableWindow()
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
"FUNCTION: s:getPath(ln) {{{2
"Gets the full path to the node that is rendered on the given line number
"
"Args:
"ln: the line number to get the path for
"
"Return:
"A path if a node was selected, {} if nothing is selected.
"If the 'up a dir' line was selected then the path to the parent of the
"current root is returned
function! s:getPath(ln)
    let line = getline(a:ln)

    let rootLine = s:TreeFileNode.GetRootLineNum()

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

    if line ==# s:tree_up_dir_line
        return b:NERDTreeRoot.path.getParent()
    endif

    let indent = s:indentLevelFor(line)

    "remove the tree parts and the leading space
    let curFile = s:stripMarkupFromLine(line, 0)

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
        let curLineStripped = s:stripMarkupFromLine(curLine, 1)

        "have we reached the top of the tree?
        if lnum == rootLine
            let dir = b:NERDTreeRoot.path.str({'format': 'UI'}) . dir
            break
        endif
        if curLineStripped =~# '/$'
            let lpindent = s:indentLevelFor(curLine)
            if lpindent < indent
                let indent = indent - 1

                let dir = substitute (curLineStripped,'^\\', "", "") . dir
                continue
            endif
        endif
    endwhile
    let curFile = b:NERDTreeRoot.path.drive . dir . curFile
    let toReturn = s:Path.New(curFile)
    return toReturn
endfunction

"FUNCTION: s:getTreeWinNum() {{{2
"gets the nerd tree window number for this tab
function! s:getTreeWinNum()
    if exists("t:NERDTreeBufName")
        return bufwinnr(t:NERDTreeBufName)
    else
        return -1
    endif
endfunction
"FUNCTION: s:indentLevelFor(line) {{{2
function! s:indentLevelFor(line)
    let level = match(a:line, '[^ \-+~▸▾`|]') / s:tree_wid
    " check if line includes arrows
    if match(a:line, '[▸▾]') > -1
        " decrement level as arrow uses 3 ascii chars
        let level = level - 1
    endif
    return level
endfunction
"FUNCTION: s:isTreeOpen() {{{2
function! s:isTreeOpen()
    return s:getTreeWinNum() != -1
endfunction
"FUNCTION: s:isWindowUsable(winnumber) {{{2
"Returns 0 if opening a file from the tree in the given window requires it to
"be split, 1 otherwise
"
"Args:
"winnumber: the number of the window in question
function! s:isWindowUsable(winnumber)
    "gotta split if theres only one window (i.e. the NERD tree)
    if winnr("$") ==# 1
        return 0
    endif

    let oldwinnr = winnr()
    call s:exec(a:winnumber . "wincmd p")
    let specialWindow = getbufvar("%", '&buftype') != '' || getwinvar('%', '&previewwindow')
    let modified = &modified
    call s:exec(oldwinnr . "wincmd p")

    "if its a special window e.g. quickfix or another explorer plugin then we
    "have to split
    if specialWindow
        return 0
    endif

    if &hidden
        return 1
    endif

    return !modified || s:bufInWindows(winbufnr(a:winnumber)) >= 2
endfunction

" FUNCTION: s:jumpToChild(direction) {{{2
" Args:
" direction: 0 if going to first child, 1 if going to last
function! s:jumpToChild(currentNode, direction)
    if a:currentNode.isRoot()
        return s:echo("cannot jump to " . (a:direction ? "last" : "first") .  " child")
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

    call s:centerView()
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
        call s:centerView()
    endif
endfunction

"FUNCTION: s:promptToDelBuffer(bufnum, msg){{{2
"prints out the given msg and, if the user responds by pushing 'y' then the
"buffer with the given bufnum is deleted
"
"Args:
"bufnum: the buffer that may be deleted
"msg: a message that will be echoed to the user asking them if they wish to
"     del the buffer
function! s:promptToDelBuffer(bufnum, msg)
    echo a:msg
    if nr2char(getchar()) ==# 'y'
        exec "silent bdelete! " . a:bufnum
    endif
endfunction

"FUNCTION: s:putCursorOnBookmarkTable(){{{2
"Places the cursor at the top of the bookmarks table
function! s:putCursorOnBookmarkTable()
    if !b:NERDTreeShowBookmarks
        throw "NERDTree.IllegalOperationError: cant find bookmark table, bookmarks arent active"
    endif

    if g:NERDTreeMinimalUI
        return cursor(1, 2)
    endif

    let rootNodeLine = s:TreeFileNode.GetRootLineNum()

    let line = 1
    while getline(line) !~# '^>-\+Bookmarks-\+$'
        let line = line + 1
        if line >= rootNodeLine
            throw "NERDTree.BookmarkTableNotFoundError: didnt find the bookmarks table"
        endif
    endwhile
    call cursor(line, 2)
endfunction

"FUNCTION: s:putCursorInTreeWin(){{{2
"Places the cursor in the nerd tree window
function! s:putCursorInTreeWin()
    if !s:isTreeOpen()
        throw "NERDTree.InvalidOperationError: cant put cursor in NERD tree window, no window exists"
    endif

    call s:exec(s:getTreeWinNum() . "wincmd w")
endfunction

"FUNCTION: s:renderBookmarks {{{2
function! s:renderBookmarks()

    if g:NERDTreeMinimalUI == 0
        call setline(line(".")+1, ">----------Bookmarks----------")
        call cursor(line(".")+1, col("."))
    endif

    for i in s:Bookmark.Bookmarks()
        call setline(line(".")+1, i.str())
        call cursor(line(".")+1, col("."))
    endfor

    call setline(line(".")+1, '')
    call cursor(line(".")+1, col("."))
endfunction
"FUNCTION: s:renderView {{{2
"The entry function for rendering the tree
function! s:renderView()
    setlocal modifiable

    "remember the top line of the buffer and the current line so we can
    "restore the view exactly how it was
    let curLine = line(".")
    let curCol = col(".")
    let topLine = line("w0")

    "delete all lines in the buffer (being careful not to clobber a register)
    silent 1,$delete _

    call s:dumpHelp()

    "delete the blank line before the help and add one after it
    if g:NERDTreeMinimalUI == 0
        call setline(line(".")+1, "")
        call cursor(line(".")+1, col("."))
    endif

    if b:NERDTreeShowBookmarks
        call s:renderBookmarks()
    endif

    "add the 'up a dir' line
    if !g:NERDTreeMinimalUI
        call setline(line(".")+1, s:tree_up_dir_line)
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

"FUNCTION: s:renderViewSavingPosition {{{2
"Renders the tree and ensures the cursor stays on the current node or the
"current nodes parent if it is no longer available upon re-rendering
function! s:renderViewSavingPosition()
    let currentNode = s:TreeFileNode.GetSelected()

    "go up the tree till we find a node that will be visible or till we run
    "out of nodes
    while currentNode != {} && !currentNode.isVisible() && !currentNode.isRoot()
        let currentNode = currentNode.parent
    endwhile

    call s:renderView()

    if currentNode != {}
        call currentNode.putCursorHere(0, 0)
    endif
endfunction
"FUNCTION: s:restoreScreenState() {{{2
"
"Sets the screen state back to what it was when s:saveScreenState was last
"called.
"
"Assumes the cursor is in the NERDTree window
function! s:restoreScreenState()
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

"FUNCTION: s:saveScreenState() {{{2
"Saves the current cursor position in the current buffer and the window
"scroll position
function! s:saveScreenState()
    let win = winnr()
    try
        call s:putCursorInTreeWin()
        let b:NERDTreeOldPos = getpos(".")
        let b:NERDTreeOldTopLine = line("w0")
        let b:NERDTreeOldWindowSize = winwidth("")
        call s:exec(win . "wincmd w")
    catch /^NERDTree.InvalidOperationError/
    endtry
endfunction

"FUNCTION: s:setCommonBufOptions() {{{2
function! s:setCommonBufOptions()
    "throwaway buffer options
    setlocal noswapfile
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal nowrap
    setlocal foldcolumn=0
    setlocal foldmethod=manual
    setlocal nofoldenable
    setlocal nobuflisted
    setlocal nospell
    if g:NERDTreeShowLineNumbers
        setlocal nu
    else
        setlocal nonu
        if v:version >= 703
            setlocal nornu
        endif
    endif

    iabc <buffer>

    if g:NERDTreeHighlightCursorline
        setlocal cursorline
    endif

    call s:setupStatusline()


    let b:treeShowHelp = 0
    let b:NERDTreeIgnoreEnabled = 1
    let b:NERDTreeShowFiles = g:NERDTreeShowFiles
    let b:NERDTreeShowHidden = g:NERDTreeShowHidden
    let b:NERDTreeShowBookmarks = g:NERDTreeShowBookmarks
    setfiletype nerdtree
    call s:bindMappings()
endfunction

"FUNCTION: s:setupStatusline() {{{2
function! s:setupStatusline()
    if g:NERDTreeStatusline != -1
        let &l:statusline = g:NERDTreeStatusline
    endif
endfunction
"FUNCTION: s:stripMarkupFromLine(line, removeLeadingSpaces){{{2
"returns the given line with all the tree parts stripped off
"
"Args:
"line: the subject line
"removeLeadingSpaces: 1 if leading spaces are to be removed (leading spaces =
"any spaces before the actual text of the node)
function! s:stripMarkupFromLine(line, removeLeadingSpaces)
    let line = a:line
    "remove the tree parts and the leading space
    let line = substitute (line, s:tree_markup_reg,"","")

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

"FUNCTION: s:toggle(dir) {{{2
"Toggles the NERD tree. I.e the NERD tree is open, it is closed, if it is
"closed it is restored or initialized (if it doesnt exist)
"
"Args:
"dir: the full path for the root node (is only used if the NERD tree is being
"initialized.
function! s:toggle(dir)
    if s:treeExistsForTab()
        if !s:isTreeOpen()
            call s:createTreeWin()
            if !&hidden
                call s:renderView()
            endif
            call s:restoreScreenState()
        else
            call s:closeTree()
        endif
    else
        call s:initNerdTree(a:dir)
    endif
endfunction
"SECTION: Interface bindings {{{1
"============================================================

"FUNCTION: s:activateAll() {{{2
"handle the user activating the updir line
function! s:activateAll()
    if getline(".") ==# s:tree_up_dir_line
        return s:upDir(0)
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

"FUNCTION: s:bindMappings() {{{2
function! s:bindMappings()
    "make <cr> do the same as the default 'o' mapping
    exec "nnoremap <silent> <buffer> <cr> :call <SID>KeyMap_Invoke('". g:NERDTreeMapActivateNode ."')<cr>"

    call s:KeyMap.BindAll()

    command! -buffer -nargs=? Bookmark :call <SID>bookmarkNode('<args>')
    command! -buffer -complete=customlist,s:completeBookmarks -nargs=1 RevealBookmark :call <SID>revealBookmark('<args>')
    command! -buffer -complete=customlist,s:completeBookmarks -nargs=1 OpenBookmark :call <SID>openBookmark('<args>')
    command! -buffer -complete=customlist,s:completeBookmarks -nargs=* ClearBookmarks call <SID>clearBookmarks('<args>')
    command! -buffer -complete=customlist,s:completeBookmarks -nargs=+ BookmarkToRoot call s:Bookmark.ToRoot('<args>')
    command! -buffer -nargs=0 ClearAllBookmarks call s:Bookmark.ClearAll() <bar> call <SID>renderView()
    command! -buffer -nargs=0 ReadBookmarks call s:Bookmark.CacheBookmarks(0) <bar> call <SID>renderView()
    command! -buffer -nargs=0 WriteBookmarks call s:Bookmark.Write()
endfunction

" FUNCTION: s:bookmarkNode(name) {{{2
" Associate the current node with the given name
function! s:bookmarkNode(...)
    let currentNode = s:TreeFileNode.GetSelected()
    if currentNode != {}
        let name = a:1
        if empty(name)
            let name = currentNode.path.getLastPathComponent(0)
        endif
        try
            call currentNode.bookmark(name)
            call s:renderView()
        catch /^NERDTree.IllegalBookmarkNameError/
            call s:echo("bookmark names must not contain spaces")
        endtry
    else
        call s:echo("select a node first")
    endif
endfunction

" FUNCTION: s:chCwd(node) {{{2
function! s:chCwd(node)
    try
        call a:node.path.changeToDir()
    catch /^NERDTree.PathChangeError/
        call s:echoWarning("could not change cwd")
    endtry
endfunction

" FUNCTION: s:chRoot(node) {{{2
" changes the current root to the selected one
function! s:chRoot(node)
    call a:node.makeRoot()
    call s:renderView()
    call b:NERDTreeRoot.putCursorHere(0, 0)
endfunction

" FUNCTION: s:clearBookmarks(bookmarks) {{{2
function! s:clearBookmarks(bookmarks)
    if a:bookmarks ==# ''
        let currentNode = s:TreeFileNode.GetSelected()
        if currentNode != {}
            call currentNode.clearBookmarks()
        endif
    else
        for name in split(a:bookmarks, ' ')
            let bookmark = s:Bookmark.BookmarkFor(name)
            call bookmark.delete()
        endfor
    endif
    call s:renderView()
endfunction
" FUNCTION: s:closeChildren(node) {{{2
" closes all childnodes of the current node
function! s:closeChildren(node)
    call a:node.closeChildren()
    call s:renderView()
    call a:node.putCursorHere(0, 0)
endfunction
" FUNCTION: s:closeCurrentDir(node) {{{2
" closes the parent dir of the current node
function! s:closeCurrentDir(node)
    let parent = a:node.parent
    if parent ==# {} || parent.isRoot()
        call s:echo("cannot close tree root")
    else
        call a:node.parent.close()
        call s:renderView()
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
            call s:closeTree()
        else
            call s:echo("Cannot close last window")
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
            call s:renderView()
            redraw
        catch /^NERDTree/
            call s:echoWarning("Could not remove bookmark")
        endtry
    else
        call s:echo("delete aborted" )
    endif

endfunction

" FUNCTION: s:displayHelp() {{{2
" toggles the help display
function! s:displayHelp()
    let b:treeShowHelp = b:treeShowHelp ? 0 : 1
    call s:renderView()
    call s:centerView()
endfunction

"FUNCTION: s:handleLeftClick() {{{2
"Checks if the click should open the current node
function! s:handleLeftClick()
    let currentNode = s:TreeFileNode.GetSelected()
    if currentNode != {}

        "the dir arrows are multibyte chars, and vim's string functions only
        "deal with single bytes - so split the line up with the hack below and
        "take the line substring manually
        let line = split(getline(line(".")), '\zs')
        let startToCur = ""
        for i in range(0,virtcol(".")-1)
            let startToCur .= line[i]
        endfor

        if currentNode.path.isDirectory
            if startToCur =~# s:tree_markup_reg && startToCur =~# '[+~▾▸] \?$'
                call currentNode.activate()
                return
            endif
        endif

        if (g:NERDTreeMouseMode ==# 2 && currentNode.path.isDirectory) || g:NERDTreeMouseMode ==# 3
            let char = strpart(startToCur, strlen(startToCur)-1, 1)
            if char !~# s:tree_markup_reg
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
    let curNode = s:TreeFileNode.GetSelected()
    if curNode ==# {}
        call s:echo("Put the cursor on a node first" )
        return
    endif

    if curNode.path.isDirectory
        call s:openExplorer(curNode)
    else
        call curNode.open({'where': 'h'})
    endif
endfunction

" FUNCTION: s:jumpToFirstChild() {{{2
" wrapper for the jump to child method
function! s:jumpToFirstChild(node)
    call s:jumpToChild(a:node, 0)
endfunction

" FUNCTION: s:jumpToLastChild() {{{2
" wrapper for the jump to child method
function! s:jumpToLastChild(node)
    call s:jumpToChild(a:node, 1)
endfunction

" FUNCTION: s:jumpToParent(node) {{{2
" moves the cursor to the parent of the current node
function! s:jumpToParent(node)
    if !empty(a:node.parent)
        call a:node.parent.putCursorHere(1, 0)
        call s:centerView()
    else
        call s:echo("cannot jump to parent")
    endif
endfunction

" FUNCTION: s:jumpToRoot() {{{2
" moves the cursor to the root node
function! s:jumpToRoot()
    call b:NERDTreeRoot.putCursorHere(1, 0)
    call s:centerView()
endfunction

" FUNCTION: s:jumpToNextSibling(node) {{{2
function! s:jumpToNextSibling(node)
    call s:jumpToSibling(a:node, 1)
endfunction

" FUNCTION: s:jumpToPrevSibling(node) {{{2
function! s:jumpToPrevSibling(node)
    call s:jumpToSibling(a:node, 0)
endfunction

" FUNCTION: s:openBookmark(name) {{{2
" put the cursor on the given bookmark and, if its a file, open it
function! s:openBookmark(name)
    try
        let targetNode = s:Bookmark.GetNodeForName(a:name, 0)
        call targetNode.putCursorHere(0, 1)
        redraw!
    catch /^NERDTree.BookmarkedNodeNotFoundError/
        call s:echo("note - target node is not cached")
        let bookmark = s:Bookmark.BookmarkFor(a:name)
        let targetNode = s:TreeFileNode.New(bookmark.path)
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
    call s:echo("Recursively opening node. Please wait...")
    call a:node.openRecursively()
    call s:renderView()
    redraw
    call s:echo("Recursively opening node. Please wait... DONE")
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

" FUNCTION: s:revealBookmark(name) {{{2
" put the cursor on the node associate with the given name
function! s:revealBookmark(name)
    try
        let targetNode = s:Bookmark.GetNodeForName(a:name, 0)
        call targetNode.putCursorHere(0, 1)
    catch /^NERDTree.BookmarkNotFoundError/
        call s:echo("Bookmark isnt cached under the current root")
    endtry
endfunction
" FUNCTION: s:refreshRoot() {{{2
" Reloads the current root. All nodes below this will be lost and the root dir
" will be reloaded.
function! s:refreshRoot()
    call s:echo("Refreshing the root node. This could take a while...")
    call b:NERDTreeRoot.refresh()
    call s:renderView()
    redraw
    call s:echo("Refreshing the root node. This could take a while... DONE")
endfunction

" FUNCTION: s:refreshCurrent(node) {{{2
" refreshes the root for the current node
function! s:refreshCurrent(node)
    let node = a:node
    if !node.path.isDirectory
        let node = node.parent
    endif

    call s:echo("Refreshing node. This could take a while...")
    call node.refresh()
    call s:renderView()
    redraw
    call s:echo("Refreshing node. This could take a while... DONE")
endfunction
" FUNCTION: s:showMenu(node) {{{2
function! s:showMenu(node)
    let mc = s:MenuController.New(s:MenuItem.AllEnabled())
    call mc.showMenu()
endfunction

" FUNCTION: s:toggleIgnoreFilter() {{{2
" toggles the use of the NERDTreeIgnore option
function! s:toggleIgnoreFilter()
    let b:NERDTreeIgnoreEnabled = !b:NERDTreeIgnoreEnabled
    call s:renderViewSavingPosition()
    call s:centerView()
endfunction

" FUNCTION: s:toggleShowBookmarks() {{{2
" toggles the display of bookmarks
function! s:toggleShowBookmarks()
    let b:NERDTreeShowBookmarks = !b:NERDTreeShowBookmarks
    if b:NERDTreeShowBookmarks
        call s:renderView()
        call s:putCursorOnBookmarkTable()
    else
        call s:renderViewSavingPosition()
    endif
    call s:centerView()
endfunction
" FUNCTION: s:toggleShowFiles() {{{2
" toggles the display of hidden files
function! s:toggleShowFiles()
    let b:NERDTreeShowFiles = !b:NERDTreeShowFiles
    call s:renderViewSavingPosition()
    call s:centerView()
endfunction

" FUNCTION: s:toggleShowHidden() {{{2
" toggles the display of hidden files
function! s:toggleShowHidden()
    let b:NERDTreeShowHidden = !b:NERDTreeShowHidden
    call s:renderViewSavingPosition()
    call s:centerView()
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
    call s:upDir(1)
endfunction

" FUNCTION: s:upDirCurrentRootClosed() {{{2
function! s:upDirCurrentRootClosed()
    call s:upDir(0)
endfunction

" SECTION: Post Source Actions {{{1
call s:postSourceActions()

"reset &cpo back to users setting
let &cpo = s:old_cpo

" vim: set sw=4 sts=4 et fdm=marker:
