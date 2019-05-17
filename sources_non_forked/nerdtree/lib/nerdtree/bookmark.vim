" ============================================================================
" CLASS: Bookmark
"
" The Bookmark class serves two purposes:
"   (1) It is the top-level prototype for new, concrete Bookmark objects.
"   (2) It provides an interface for client code to query and manipulate the
"       global list of Bookmark objects within the current Vim session.
" ============================================================================


let s:Bookmark = {}
let g:NERDTreeBookmark = s:Bookmark

" FUNCTION: Bookmark.activate(nerdtree) {{{1
function! s:Bookmark.activate(nerdtree, ...)
    call self.open(a:nerdtree, a:0 ? a:1 : {})
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
endfunction

" FUNCTION: Bookmark.Bookmarks() {{{1
" Class method to get all bookmarks. Lazily initializes the bookmarks global
" variable
function! s:Bookmark.Bookmarks()
    if !exists("g:NERDTreeBookmarks")
        let g:NERDTreeBookmarks = []
    endif
    return g:NERDTreeBookmarks
endfunction

" FUNCTION: Bookmark.BookmarkExistsFor(name) {{{1
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

" FUNCTION: Bookmark.BookmarkFor(name) {{{1
" Class method that returns the Bookmark object having the specified name.
" Throws "NERDTree.BookmarkNotFoundError" if no Bookmark is found.
function! s:Bookmark.BookmarkFor(name)
    let l:result = {}
    for l:bookmark in s:Bookmark.Bookmarks()
        if l:bookmark.name ==# a:name
            let l:result = l:bookmark
            break
        endif
    endfor
    if empty(l:result)
        throw 'NERDTree.BookmarkNotFoundError: "' . a:name  . '" not found'
    endif
    return l:result
endfunction

" FUNCTION: Bookmark.BookmarkNames() {{{1
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
                let path = fnamemodify(path, ':p')

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
    endif
endfunction

" FUNCTION: Bookmark.CompareBookmarksByName(firstBookmark, secondBookmark) {{{1
" Class method that indicates the relative position of two bookmarks when
" placed in alphabetical order by name. Case-sensitivity is determined by an
" option. Supports the "s:Bookmark.SortBookmarksList()" method.
function! s:Bookmark.CompareBookmarksByName(firstBookmark, secondBookmark)
    let l:result = 0
    if g:NERDTreeBookmarksSort == 1
        if a:firstBookmark.name <? a:secondBookmark.name
            let l:result = -1
        elseif a:firstBookmark.name >? a:secondBookmark.name
            let l:result = 1
        endif
    elseif g:NERDTreeBookmarksSort == 2
        if a:firstBookmark.name <# a:secondBookmark.name
            let l:result = -1
        elseif a:firstBookmark.name ># a:secondBookmark.name
            let l:result = 1
        endif
    endif
    return l:result
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
    call remove(s:Bookmark.Bookmarks(), index(s:Bookmark.Bookmarks(), self))
    call s:Bookmark.Write()
endfunction

" FUNCTION: s:Edit() {{{1
" opens the NERDTreeBookmarks file for manual editing
function! s:Bookmark.Edit()
    execute "wincmd w"
    execute "edit ".g:NERDTreeBookmarksFile
endfunction

" FUNCTION: Bookmark.getNode(nerdtree, searchFromAbsoluteRoot) {{{1
" Returns the tree node object associated with this Bookmark.
" Throws "NERDTree.BookmarkedNodeNotFoundError" if the node is not found.
"
" Args:
" searchFromAbsoluteRoot: boolean flag, search from the highest cached node
"   if true and from the current tree root if false
function! s:Bookmark.getNode(nerdtree, searchFromAbsoluteRoot)
    if a:searchFromAbsoluteRoot
        let l:searchRoot = a:nerdtree.root.AbsoluteTreeRoot()
    else
        let l:searchRoot = a:nerdtree.root
    endif
    let l:targetNode = l:searchRoot.findNode(self.path)
    if empty(l:targetNode)
        throw 'NERDTree.BookmarkedNodeNotFoundError: node for bookmark "' . self.name . '" not found'
    endif
    return l:targetNode
endfunction

" FUNCTION: Bookmark.GetNodeForName(name, searchFromAbsoluteRoot, nerdtree) {{{1
" Class method that returns the tree node object for the Bookmark with the
" given name. Throws "NERDTree.BookmarkNotFoundError" if a Bookmark with the
" name does not exist. Throws "NERDTree.BookmarkedNodeNotFoundError" if a
" tree node for the named Bookmark could not be found.
function! s:Bookmark.GetNodeForName(name, searchFromAbsoluteRoot, nerdtree)
    let l:bookmark = s:Bookmark.BookmarkFor(a:name)
    return l:bookmark.getNode(a:nerdtree, a:searchFromAbsoluteRoot)
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

" FUNCTION: Bookmark.InvalidBookmarks() {{{1
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

" FUNCTION: Bookmark.open(nerdtree, [options]) {{{1
"Args:
"
"nerdtree: the tree to load open the bookmark in
"
"A dictionary containing the following keys (all optional):
"  'where': Specifies whether the node should be opened in new split/tab or in
"           the previous window. Can be either 'v' (vertical split), 'h'
"           (horizontal split), 't' (new tab) or 'p' (previous window).
"  'reuse': if a window is displaying the file then jump the cursor there
"  'keepopen': dont close the tree window
"  'stay': open the file, but keep the cursor in the tree win
"
function! s:Bookmark.open(nerdtree, ...)
    let opts = a:0 ? a:1 : {}

    if nerdtree#and(g:NERDTreeQuitOnOpen,2)
        call a:nerdtree.ui.toggleShowBookmarks()
    endif

    if self.path.isDirectory && !has_key(opts, 'where')
        call self.toRoot(a:nerdtree)
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

" FUNCTION: Bookmark.setPath(path) {{{1
" makes this bookmark point to the given path
function! s:Bookmark.setPath(path)
    let self.path = a:path
endfunction

" FUNCTION: Bookmark.SortBookmarksList() {{{1
" Class method that sorts the global list of bookmarks alphabetically by name.
" Note that case-sensitivity is determined by a user option.
function! s:Bookmark.SortBookmarksList()
    call sort(s:Bookmark.Bookmarks(), s:Bookmark.CompareBookmarksByName, s:Bookmark)
endfunction

" FUNCTION: Bookmark.str() {{{1
" Get the string that should be rendered in the view for this bookmark
function! s:Bookmark.str()
    let pathStrMaxLen = winwidth(g:NERDTree.GetWinNum()) - 4 - strdisplaywidth(self.name)
    if &nu
        let pathStrMaxLen = pathStrMaxLen - &numberwidth
    endif

    let pathStr = self.path.str({'format': 'UI'})
    if strdisplaywidth(pathStr) > pathStrMaxLen
        while strdisplaywidth(pathStr) > pathStrMaxLen && strchars(pathStr) > 0
            let pathStr = substitute(pathStr, '^.', '', '')
        endwhile
        let pathStr = '<' . pathStr
    endif
    return '>' . self.name . ' ' . pathStr
endfunction

" FUNCTION: Bookmark.toRoot(nerdtree) {{{1
" Set the root of the given NERDTree to the node for this Bookmark. If a node
" for this Bookmark does not exist, a new one is initialized.
function! s:Bookmark.toRoot(nerdtree)
    if self.validate()
        try
            let l:targetNode = self.getNode(a:nerdtree, 1)
            call l:targetNode.closeChildren()
        catch /^NERDTree.BookmarkedNodeNotFoundError/
            let l:targetNode = g:NERDTreeFileNode.New(s:Bookmark.BookmarkFor(self.name).path, a:nerdtree)
        endtry
        call a:nerdtree.changeRoot(l:targetNode)
    endif
endfunction

" FUNCTION: Bookmark.ToRoot(name, nerdtree) {{{1
" Class method that makes the Bookmark with the given name the root of
" specified NERDTree.
function! s:Bookmark.ToRoot(name, nerdtree)
    let l:bookmark = s:Bookmark.BookmarkFor(a:name)
    call l:bookmark.toRoot(a:nerdtree)
endfunction

" FUNCTION: Bookmark.validate() {{{1
function! s:Bookmark.validate()
    if self.path.exists()
        return 1
    else
        call s:Bookmark.CacheBookmarks(1)
        call nerdtree#echo(self.name . "now points to an invalid location. See :help NERDTreeInvalidBookmarks for info.")
        return 0
    endif
endfunction

" FUNCTION: Bookmark.Write() {{{1
" Class method to write all bookmarks to the bookmarks file
function! s:Bookmark.Write()
    let bookmarkStrings = []
    for i in s:Bookmark.Bookmarks()
        call add(bookmarkStrings, i.name . ' ' . fnamemodify(i.path.str(), ':~'))
    endfor

    "add a blank line before the invalid ones
    call add(bookmarkStrings, "")

    for j in s:Bookmark.InvalidBookmarks()
        call add(bookmarkStrings, j)
    endfor

    try
        call writefile(bookmarkStrings, g:NERDTreeBookmarksFile)
    catch
        call nerdtree#echoError("Failed to write bookmarks file. Make sure g:NERDTreeBookmarksFile points to a valid location.")
    endtry
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
