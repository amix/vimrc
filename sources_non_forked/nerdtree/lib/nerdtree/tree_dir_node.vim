"CLASS: TreeDirNode
"A subclass of NERDTreeFileNode.
"
"The 'composite' part of the file/dir composite.
"============================================================
let s:TreeDirNode = copy(g:NERDTreeFileNode)
let g:NERDTreeDirNode = s:TreeDirNode

"FUNCTION: TreeDirNode.AbsoluteTreeRoot(){{{1
"class method that returns the highest cached ancestor of the current root
function! s:TreeDirNode.AbsoluteTreeRoot()
    let currentNode = b:NERDTreeRoot
    while currentNode.parent != {}
        let currentNode = currentNode.parent
    endwhile
    return currentNode
endfunction

"FUNCTION: TreeDirNode.activate([options]) {{{1
unlet s:TreeDirNode.activate
function! s:TreeDirNode.activate(...)
    let opts = a:0 ? a:1 : {}
    call self.toggleOpen(opts)
    call b:NERDTree.render()
    call self.putCursorHere(0, 0)
endfunction

"FUNCTION: TreeDirNode.addChild(treenode, inOrder) {{{1
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

"FUNCTION: TreeDirNode.close() {{{1
"Closes this directory
function! s:TreeDirNode.close()
    let self.isOpen = 0
endfunction

"FUNCTION: TreeDirNode.closeChildren() {{{1
"Closes all the child dir nodes of this node
function! s:TreeDirNode.closeChildren()
    for i in self.children
        if i.path.isDirectory
            call i.close()
            call i.closeChildren()
        endif
    endfor
endfunction

"FUNCTION: TreeDirNode.createChild(path, inOrder) {{{1
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
    let newTreeNode = g:NERDTreeFileNode.New(a:path)
    call self.addChild(newTreeNode, a:inOrder)
    return newTreeNode
endfunction

"FUNCTION: TreeDirNode.findNode(path) {{{1
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

"FUNCTION: TreeDirNode.getChildCount() {{{1
"Returns the number of children this node has
function! s:TreeDirNode.getChildCount()
    return len(self.children)
endfunction

"FUNCTION: TreeDirNode.getChild(path) {{{1
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

"FUNCTION: TreeDirNode.getChildByIndex(indx, visible) {{{1
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

"FUNCTION: TreeDirNode.getChildIndex(path) {{{1
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

"FUNCTION: TreeDirNode.GetSelected() {{{1
"Returns the current node if it is a dir node, or else returns the current
"nodes parent
unlet s:TreeDirNode.GetSelected
function! s:TreeDirNode.GetSelected()
    let currentDir = g:NERDTreeFileNode.GetSelected()
    if currentDir != {} && !currentDir.isRoot()
        if currentDir.path.isDirectory ==# 0
            let currentDir = currentDir.parent
        endif
    endif
    return currentDir
endfunction

"FUNCTION: TreeDirNode.getVisibleChildCount() {{{1
"Returns the number of visible children this node has
function! s:TreeDirNode.getVisibleChildCount()
    return len(self.getVisibleChildren())
endfunction

"FUNCTION: TreeDirNode.getVisibleChildren() {{{1
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

"FUNCTION: TreeDirNode.hasVisibleChildren() {{{1
"returns 1 if this node has any childre, 0 otherwise..
function! s:TreeDirNode.hasVisibleChildren()
    return self.getVisibleChildCount() != 0
endfunction

"FUNCTION: TreeDirNode._initChildren() {{{1
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

    if version >= 703
        let filesStr = globpath(globDir, '*', !g:NERDTreeRespectWildIgnore) . "\n" . globpath(globDir, '.*', !g:NERDTreeRespectWildIgnore)
    else
        let filesStr = globpath(globDir, '*') . "\n" . globpath(globDir, '.*')
    endif

    let files = split(filesStr, "\n")

    if !a:silent && len(files) > g:NERDTreeNotificationThreshold
        call nerdtree#echo("Please wait, caching a large dir ...")
    endif

    let invalidFilesFound = 0
    for i in files

        "filter out the .. and . directories
        "Note: we must match .. AND ../ cos sometimes the globpath returns
        "../ for path with strange chars (eg $)
        if i !~# '\/\.\.\/\?$' && i !~# '\/\.\/\?$'

            "put the next file in a new node and attach it
            try
                let path = g:NERDTreePath.New(i)
                call self.createChild(path, 0)
                call g:NERDTreePathNotifier.NotifyListeners('init', path, {})
            catch /^NERDTree.\(InvalidArguments\|InvalidFiletype\)Error/
                let invalidFilesFound += 1
            endtry
        endif
    endfor

    call self.sortChildren()

    if !a:silent && len(files) > g:NERDTreeNotificationThreshold
        call nerdtree#echo("Please wait, caching a large dir ... DONE (". self.getChildCount() ." nodes cached).")
    endif

    if invalidFilesFound
        call nerdtree#echoWarning(invalidFilesFound . " file(s) could not be loaded into the NERD tree")
    endif
    return self.getChildCount()
endfunction

"FUNCTION: TreeDirNode.New(path) {{{1
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

"FUNCTION: TreeDirNode.open([opts]) {{{1
"Open the dir in the current tree or in a new tree elsewhere.
"
"If opening in the current tree, return the number of cached nodes.
unlet s:TreeDirNode.open
function! s:TreeDirNode.open(...)
    let opts = a:0 ? a:1 : {}

    if has_key(opts, 'where') && !empty(opts['where'])
        let opener = g:NERDTreeOpener.New(self.path, opts)
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

"FUNCTION: TreeDirNode.openAlong([opts]) {{{1
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

" FUNCTION: TreeDirNode.openExplorer() {{{1
" opens an explorer window for this node in the previous window (could be a
" nerd tree or a netrw)
function! s:TreeDirNode.openExplorer()
    call self.open({'where': 'p'})
endfunction

"FUNCTION: TreeDirNode.openInNewTab(options) {{{1
unlet s:TreeDirNode.openInNewTab
function! s:TreeDirNode.openInNewTab(options)
    call nerdtree#deprecated('TreeDirNode.openInNewTab', 'is deprecated, use open() instead')
    call self.open({'where': 't'})
endfunction

"FUNCTION: TreeDirNode._openInNewTab() {{{1
function! s:TreeDirNode._openInNewTab()
    tabnew
    call g:NERDTreeCreator.CreatePrimary(self.path.str())
endfunction

"FUNCTION: TreeDirNode.openRecursively() {{{1
"Opens this treenode and all of its children whose paths arent 'ignored'
"because of the file filters.
"
"This method is actually a wrapper for the OpenRecursively2 method which does
"the work.
function! s:TreeDirNode.openRecursively()
    call self._openRecursively2(1)
endfunction

"FUNCTION: TreeDirNode._openRecursively2() {{{1
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

"FUNCTION: TreeDirNode.refresh() {{{1
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
                    let path = g:NERDTreePath.New(i)
                    let newNode = self.getChild(path)
                    if newNode != {}
                        call newNode.refresh()
                        call add(newChildNodes, newNode)

                    "the node doesnt exist so create it
                    else
                        let newNode = g:NERDTreeFileNode.New(path)
                        let newNode.parent = self
                        call add(newChildNodes, newNode)
                    endif


                catch /^NERDTree.\(InvalidArguments\|InvalidFiletype\)Error/
                    let invalidFilesFound = 1
                endtry
            endif
        endfor

        "swap this nodes children out for the children we just read/refreshed
        let self.children = newChildNodes
        call self.sortChildren()

        if invalidFilesFound
            call nerdtree#echoWarning("some files could not be loaded into the NERD tree")
        endif
    endif
endfunction

"FUNCTION: TreeDirNode.refreshFlags() {{{1
unlet s:TreeDirNode.refreshFlags
function! s:TreeDirNode.refreshFlags()
    call self.path.refreshFlags()
    for i in self.children
        call i.refreshFlags()
    endfor
endfunction

"FUNCTION: TreeDirNode.refreshDirFlags() {{{1
function! s:TreeDirNode.refreshDirFlags()
    call self.path.refreshFlags()
endfunction

"FUNCTION: TreeDirNode.reveal(path) {{{1
"reveal the given path, i.e. cache and open all treenodes needed to display it
"in the UI
function! s:TreeDirNode.reveal(path)
    if !a:path.isUnder(self.path)
        throw "NERDTree.InvalidArgumentsError: " . a:path.str() . " should be under " . self.path.str()
    endif

    call self.open()

    if self.path.equals(a:path.getParent())
        let n = self.findNode(a:path)
        call b:NERDTree.render()
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

"FUNCTION: TreeDirNode.removeChild(treenode) {{{1
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

"FUNCTION: TreeDirNode.sortChildren() {{{1
"
"Sorts the children of this node according to alphabetical order and the
"directory priority.
"
function! s:TreeDirNode.sortChildren()
    let CompareFunc = function("nerdtree#compareNodes")
    call sort(self.children, CompareFunc)
endfunction

"FUNCTION: TreeDirNode.toggleOpen([options]) {{{1
"Opens this directory if it is closed and vice versa
function! s:TreeDirNode.toggleOpen(...)
    let opts = a:0 ? a:1 : {}
    if self.isOpen ==# 1
        call self.close()
    else
        if g:NERDTreeCascadeOpenSingleChildDir == 0
            call self.open(opts)
        else
            call self.openAlong(opts)
        endif
    endif
endfunction

"FUNCTION: TreeDirNode.transplantChild(newNode) {{{1
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

" vim: set sw=4 sts=4 et fdm=marker:
