"CLASS: Bookmark
"============================================================
let s:Bookmark = {}
let g:NERDTreeBookmark = s:Bookmark

" FUNCTION: Bookmark.activate() {{{1
function! s:Bookmark.activate(...)
    call self.open(a:0 ? a:1 : {})
endfunction

" FUNCTION: Bookmark.AddBookmark(name, path) {{{1
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
    if g:NERDTreeBookmarksSort ==# 1
        call s:Bookmark.Sort()
    endif
endfunction

" FUNCTION: Bookmark.Bookmarks()   {{{1
" Class method to get all bookmarks. Lazily initializes the bookmarks global
" variable
function! s:Bookmark.Bookmarks()
    if !exists("g:NERDTreeBookmarks")
        let g:NERDTreeBookmarks = []
    endif
    return g:NERDTreeBookmarks
endfunction

" FUNCTION: Bookmark.BookmarkExistsFor(name)   {{{1
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

" FUNCTION: Bookmark.BookmarkFor(name)   {{{1
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

" FUNCTION: Bookmark.BookmarkNames()   {{{1
" Class method to return an array of all bookmark names
function! s:Bookmark.BookmarkNames()
    let names = []
    for i in s:Bookmark.Bookmarks()
        call add(names, i.name)
    endfor
    return names
endfunction

" FUNCTION: Bookmark.CacheBookmarks(silent) {{{1
" Class method to read all bookmarks from the bookmarks file initialize
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
                    let bookmark = s:Bookmark.New(name, g:NERDTreePath.New(path))
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
                call nerdtree#echo(invalidBookmarksFound . " invalid bookmarks were read. See :help NERDTreeInvalidBookmarks for info.")
            endif
        endif
        if g:NERDTreeBookmarksSort ==# 1
            call s:Bookmark.Sort()
        endif
    endif
endfunction

" FUNCTION: Bookmark.compareTo(otherbookmark) {{{1
" Compare these two bookmarks for sorting purposes
function! s:Bookmark.compareTo(otherbookmark)
    return a:otherbookmark.name < self.name
endfunction
" FUNCTION: Bookmark.ClearAll() {{{1
" Class method to delete all bookmarks.
function! s:Bookmark.ClearAll()
    for i in s:Bookmark.Bookmarks()
        call i.delete()
    endfor
    call s:Bookmark.Write()
endfunction

" FUNCTION: Bookmark.delete() {{{1
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

" FUNCTION: Bookmark.getNode(searchFromAbsoluteRoot) {{{1
" Gets the treenode for this bookmark
"
" Args:
" searchFromAbsoluteRoot: specifies whether we should search from the current
" tree root, or the highest cached node
function! s:Bookmark.getNode(searchFromAbsoluteRoot)
    let searchRoot = a:searchFromAbsoluteRoot ? g:NERDTreeDirNode.AbsoluteTreeRoot() : b:NERDTreeRoot
    let targetNode = searchRoot.findNode(self.path)
    if empty(targetNode)
        throw "NERDTree.BookmarkedNodeNotFoundError: no node was found for bookmark: " . self.name
    endif
    return targetNode
endfunction

" FUNCTION: Bookmark.GetNodeForName(name, searchFromAbsoluteRoot) {{{1
" Class method that finds the bookmark with the given name and returns the
" treenode for it.
function! s:Bookmark.GetNodeForName(name, searchFromAbsoluteRoot)
    let bookmark = s:Bookmark.BookmarkFor(a:name)
    return bookmark.getNode(a:searchFromAbsoluteRoot)
endfunction

" FUNCTION: Bookmark.GetSelected() {{{1
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

" FUNCTION: Bookmark.InvalidBookmarks()   {{{1
" Class method to get all invalid bookmark strings read from the bookmarks
" file
function! s:Bookmark.InvalidBookmarks()
    if !exists("g:NERDTreeInvalidBookmarks")
        let g:NERDTreeInvalidBookmarks = []
    endif
    return g:NERDTreeInvalidBookmarks
endfunction

" FUNCTION: Bookmark.mustExist() {{{1
function! s:Bookmark.mustExist()
    if !self.path.exists()
        call s:Bookmark.CacheBookmarks(1)
        throw "NERDTree.BookmarkPointsToInvalidLocationError: the bookmark \"".
            \ self.name ."\" points to a non existing location: \"". self.path.str()
    endif
endfunction

" FUNCTION: Bookmark.New(name, path) {{{1
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

" FUNCTION: Bookmark.open([options]) {{{1
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
        let opener = g:NERDTreeOpener.New(self.path, opts)
        call opener.open(self)
    endif
endfunction

" FUNCTION: Bookmark.openInNewTab(options) {{{1
" Create a new bookmark object with the given name and path object
function! s:Bookmark.openInNewTab(options)
    call nerdtree#deprecated('Bookmark.openInNewTab', 'is deprecated, use open() instead')
    call self.open(a:options)
endfunction

" FUNCTION: Bookmark.setPath(path)   {{{1
" makes this bookmark point to the given path
function! s:Bookmark.setPath(path)
    let self.path = a:path
endfunction

" FUNCTION: Bookmark.Sort()   {{{1
" Class method that sorts all bookmarks
function! s:Bookmark.Sort()
    let CompareFunc = function("nerdtree#compareBookmarks")
    call sort(s:Bookmark.Bookmarks(), CompareFunc)
endfunction

" FUNCTION: Bookmark.str()   {{{1
" Get the string that should be rendered in the view for this bookmark
function! s:Bookmark.str()
    let pathStrMaxLen = winwidth(nerdtree#getTreeWinNum()) - 4 - len(self.name)
    if &nu
        let pathStrMaxLen = pathStrMaxLen - &numberwidth
    endif

    let pathStr = self.path.str({'format': 'UI'})
    if len(pathStr) > pathStrMaxLen
        let pathStr = '<' . strpart(pathStr, len(pathStr) - pathStrMaxLen)
    endif
    return '>' . self.name . ' ' . pathStr
endfunction

" FUNCTION: Bookmark.toRoot() {{{1
" Make the node for this bookmark the new tree root
function! s:Bookmark.toRoot()
    if self.validate()
        try
            let targetNode = self.getNode(1)
        catch /^NERDTree.BookmarkedNodeNotFoundError/
            let targetNode = g:NERDTreeFileNode.New(s:Bookmark.BookmarkFor(self.name).path)
        endtry
        call targetNode.makeRoot()
        call nerdtree#renderView()
        call targetNode.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: Bookmark.ToRoot(name) {{{1
" Make the node for this bookmark the new tree root
function! s:Bookmark.ToRoot(name)
    let bookmark = s:Bookmark.BookmarkFor(a:name)
    call bookmark.toRoot()
endfunction

" FUNCTION: Bookmark.validate() {{{1
function! s:Bookmark.validate()
    if self.path.exists()
        return 1
    else
        call s:Bookmark.CacheBookmarks(1)
        call nerdtree#renderView()
        call nerdtree#echo(self.name . "now points to an invalid location. See :help NERDTreeInvalidBookmarks for info.")
        return 0
    endif
endfunction

" FUNCTION: Bookmark.Write()   {{{1
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

" vim: set sw=4 sts=4 et fdm=marker:
