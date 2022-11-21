" ============================================================================
" CLASS: TreeFileNode
"
" This class is the parent of the TreeDirNode class and is the 'Component'
" part of the composite design pattern between the NERDTree node classes.
" ============================================================================


let s:TreeFileNode = {}
let g:NERDTreeFileNode = s:TreeFileNode

" FUNCTION: TreeFileNode.activate(...) {{{1
function! s:TreeFileNode.activate(...)
    call self.open(a:0 ? a:1 : {})
endfunction

" FUNCTION: TreeFileNode.bookmark(name) {{{1
" bookmark this node with a:name
function! s:TreeFileNode.bookmark(name)

    " if a bookmark exists with the same name and the node is cached then save
    " it so we can update its display string
    let oldMarkedNode = {}
    try
        let oldMarkedNode = g:NERDTreeBookmark.GetNodeForName(a:name, 1, self.getNerdtree())
    catch /^NERDTree.BookmarkNotFoundError/
    catch /^NERDTree.BookmarkedNodeNotFoundError/
    endtry

    call g:NERDTreeBookmark.AddBookmark(a:name, self.path)
    call self.path.cacheDisplayString()
    call g:NERDTreeBookmark.Write()

    if !empty(oldMarkedNode)
        call oldMarkedNode.path.cacheDisplayString()
    endif
endfunction

" FUNCTION: TreeFileNode.cacheParent() {{{1
" initializes self.parent if it isnt already
function! s:TreeFileNode.cacheParent()
    if empty(self.parent)
        let parentPath = self.path.getParent()
        if parentPath.equals(self.path)
            throw 'NERDTree.CannotCacheParentError: already at root'
        endif
        let self.parent = s:TreeFileNode.New(parentPath, self.getNerdtree())
    endif
endfunction

" FUNCTION: TreeFileNode.clearBookmarks() {{{1
function! s:TreeFileNode.clearBookmarks()
    for i in g:NERDTreeBookmark.Bookmarks()
        if i.path.equals(self.path)
            call i.delete()
        end
    endfor
    call self.path.cacheDisplayString()
endfunction

" FUNCTION: TreeFileNode.copy(dest) {{{1
function! s:TreeFileNode.copy(dest)
    call self.path.copy(a:dest)
    let newPath = g:NERDTreePath.New(a:dest)
    let parent = self.getNerdtree().root.findNode(newPath.getParent())
    if !empty(parent)
        call parent.refresh()
        return parent.findNode(newPath)
    else
        return {}
    endif
endfunction

" FUNCTION: TreeFileNode.delete {{{1
" Removes this node from the tree and calls the Delete method for its path obj
function! s:TreeFileNode.delete()
    call self.path.delete()
    call self.parent.removeChild(self)
endfunction

" FUNCTION: TreeFileNode.displayString() {{{1
"
" Returns a string that specifies how the node should be represented as a
" string
"
" Return:
" a string that can be used in the view to represent this node
function! s:TreeFileNode.displayString()
    return self.path.flagSet.renderToString() . self.path.displayString()
endfunction

" FUNCTION: TreeFileNode.equals(treenode) {{{1
"
" Compares this treenode to the input treenode and returns 1 if they are the
" same node.
"
" Use this method instead of ==  because sometimes when the treenodes contain
" many children, vim seg faults when doing ==
"
" Args:
" treenode: the other treenode to compare to
function! s:TreeFileNode.equals(treenode)
    return self.path.str() ==# a:treenode.path.str()
endfunction

" FUNCTION: TreeFileNode.findNode(path) {{{1
" Returns self if this node.path.Equals the given path.
" Returns {} if not equal.
"
" Args:
" path: the path object to compare against
function! s:TreeFileNode.findNode(path)
    if a:path.equals(self.path)
        return self
    endif
    return {}
endfunction

" FUNCTION: TreeFileNode.findSibling(direction) {{{1
" Find the next or previous sibling of this node.
"
" Args:
" direction: 0 for previous, 1 for next
"
" Return:
" The next/previous TreeFileNode object or an empty dictionary if not found.
function! s:TreeFileNode.findSibling(direction)

    " There can be no siblings if there is no parent.
    if empty(self.parent)
        return {}
    endif

    let l:nodeIndex = self.parent.getChildIndex(self.path)

    if l:nodeIndex == -1
        return {}
    endif

    " Get the next index to begin the search.
    let l:nodeIndex += a:direction ? 1 : -1

    while 0 <= l:nodeIndex && l:nodeIndex < self.parent.getChildCount()

        " Return the next node if it is not ignored.
        if !self.parent.children[l:nodeIndex].path.ignore(self.getNerdtree())
            return self.parent.children[l:nodeIndex]
        endif

        let l:nodeIndex += a:direction ? 1 : -1
    endwhile

    return {}
endfunction

" FUNCTION: TreeFileNode.getNerdtree(){{{1
function! s:TreeFileNode.getNerdtree()
    return self._nerdtree
endfunction

" FUNCTION: TreeFileNode.GetRootForTab(){{{1
" get the root node for this tab
function! s:TreeFileNode.GetRootForTab()
    if g:NERDTree.ExistsForTab()
        return getbufvar(t:NERDTreeBufName, 'NERDTree').root
    end
    return {}
endfunction

" FUNCTION: TreeFileNode.GetSelected() {{{1
" If the cursor is currently positioned on a tree node, return the node.
" Otherwise, return the empty dictionary.
function! s:TreeFileNode.GetSelected()

    try
        let l:path = b:NERDTree.ui.getPath(line('.'))

        if empty(l:path)
            return {}
        endif

        return b:NERDTree.root.findNode(l:path)
    catch
        return {}
    endtry
endfunction

" FUNCTION: TreeFileNode.isVisible() {{{1
" returns 1 if this node should be visible according to the tree filters and
" hidden file filters (and their on/off status)
function! s:TreeFileNode.isVisible()
    return !self.path.ignore(self.getNerdtree())
endfunction

" FUNCTION: TreeFileNode.isRoot() {{{1
function! s:TreeFileNode.isRoot()
    if !g:NERDTree.ExistsForBuf()
        throw 'NERDTree.NoTreeError: No tree exists for the current buffer'
    endif

    return self.equals(self.getNerdtree().root)
endfunction

" FUNCTION: TreeFileNode.New(path, nerdtree) {{{1
" Returns a new TreeNode object with the given path and parent
"
" Args:
" path: file/dir that the node represents
" nerdtree: the tree the node belongs to
function! s:TreeFileNode.New(path, nerdtree)
    if a:path.isDirectory
        return g:NERDTreeDirNode.New(a:path, a:nerdtree)
    else
        let newTreeNode = copy(self)
        let newTreeNode.path = a:path
        let newTreeNode.parent = {}
        let newTreeNode._nerdtree = a:nerdtree
        return newTreeNode
    endif
endfunction

" FUNCTION: TreeFileNode.open() {{{1
function! s:TreeFileNode.open(...)
    let opts = a:0 ? a:1 : {}
    let opener = g:NERDTreeOpener.New(self.path, opts)
    call opener.open(self)
endfunction

" FUNCTION: TreeFileNode.openSplit() {{{1
" Open this node in a new window
function! s:TreeFileNode.openSplit()
    call nerdtree#deprecated('TreeFileNode.openSplit', 'is deprecated, use .open() instead.')
    call self.open({'where': 'h'})
endfunction

" FUNCTION: TreeFileNode.openVSplit() {{{1
" Open this node in a new vertical window
function! s:TreeFileNode.openVSplit()
    call nerdtree#deprecated('TreeFileNode.openVSplit', 'is deprecated, use .open() instead.')
    call self.open({'where': 'v'})
endfunction

" FUNCTION: TreeFileNode.openInNewTab(options) {{{1
function! s:TreeFileNode.openInNewTab(options)
    call nerdtree#deprecated('TreeFileNode.openinNewTab', 'is deprecated, use .open() instead.')
    call self.open(extend({'where': 't'}, a:options))
endfunction

" FUNCTION: TreeFileNode.openExplorer()
function! s:TreeFileNode.openExplorer()
    execute 'wincmd p'
    execute 'edit '.self.path.getParent().str({'format':'Edit'})
endfunction

" FUNCTION: TreeFileNode.putCursorHere(isJump, recurseUpward){{{1
" Places the cursor on the line number this node is rendered on
"
" Args:
" isJump: 1 if this cursor movement should be counted as a jump by vim
" recurseUpward: try to put the cursor on the parent if the this node isnt
" visible
function! s:TreeFileNode.putCursorHere(isJump, recurseUpward)
    let ln = self.getNerdtree().ui.getLineNum(self)
    if ln != -1
        if a:isJump
            mark '
        endif
        call cursor(ln, col('.'))
    else
        if a:recurseUpward
            let node = self
            while node != {} && self.getNerdtree().ui.getLineNum(node) ==# -1
                let node = node.parent
                call node.open()
            endwhile
            call self._nerdtree.render()
            call node.putCursorHere(a:isJump, 0)
        endif
    endif
endfunction

" FUNCTION: TreeFileNode.refresh() {{{1
function! s:TreeFileNode.refresh()
    call self.path.refresh(self.getNerdtree())
endfunction

" FUNCTION: TreeFileNode.refreshFlags() {{{1
function! s:TreeFileNode.refreshFlags()
    call self.path.refreshFlags(self.getNerdtree())
endfunction

" FUNCTION: TreeFileNode.rename() {{{1
" Calls the rename method for this nodes path obj
function! s:TreeFileNode.rename(newName)
    let newName = substitute(a:newName, '\(\\\|\/\)$', '', '')
    call self.path.rename(newName)
    call self.parent.removeChild(self)

    let parentPath = self.path.getParent()
    let newParent = self.getNerdtree().root.findNode(parentPath)

    if newParent != {}
        call newParent.createChild(self.path, 1)
        call newParent.refresh()
    endif
endfunction

" FUNCTION: TreeFileNode.renderToString {{{1
" returns a string representation for this tree to be rendered in the view
function! s:TreeFileNode.renderToString()
    return self._renderToString(0, 0)
endfunction

" Args:
" depth: the current depth in the tree for this call
" drawText: 1 if we should actually draw the line for this node (if 0 then the
" child nodes are rendered only)
" for each depth in the tree
function! s:TreeFileNode._renderToString(depth, drawText)
    let output = ''
    if a:drawText ==# 1

        let treeParts = repeat('  ', a:depth - 1)
        let treeParts .= (self.path.isDirectory || g:NERDTreeDirArrowExpandable ==# '' ? '' : '  ')

        let line = treeParts . self.displayString()
        let output = output . line . "\n"
    endif

    " if the node is an open dir, draw its children
    if self.path.isDirectory ==# 1 && self.isOpen ==# 1

        let childNodesToDraw = self.getVisibleChildren()

        if self.isCascadable() && a:depth > 0

            let output = output . childNodesToDraw[0]._renderToString(a:depth, 0)

        elseif len(childNodesToDraw) > 0
            for i in childNodesToDraw
                let output = output . i._renderToString(a:depth + 1, 1)
            endfor
        endif
    endif

    return output
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
