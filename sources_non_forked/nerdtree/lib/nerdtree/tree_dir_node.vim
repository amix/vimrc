" ============================================================================
" CLASS: TreeDirNode
"
" A subclass of NERDTreeFileNode.
"
" The 'composite' part of the file/dir composite.
" ============================================================================


let s:TreeDirNode = copy(g:NERDTreeFileNode)
let g:NERDTreeDirNode = s:TreeDirNode

" FUNCTION: TreeDirNode.AbsoluteTreeRoot(){{{1
" Class method that returns the highest cached ancestor of the current root.
function! s:TreeDirNode.AbsoluteTreeRoot()
    let currentNode = b:NERDTree.root
    while currentNode.parent !=# {}
        let currentNode = currentNode.parent
    endwhile
    return currentNode
endfunction

" FUNCTION: TreeDirNode.activate([options]) {{{1
function! s:TreeDirNode.activate(...)
    let l:options = (a:0 > 0) ? a:1 : {}

    call self.toggleOpen(l:options)

    " Note that we only re-render the NERDTree for this node if we did NOT
    " create a new node and render it in a new window or tab.  In the latter
    " case, rendering the NERDTree for this node could overwrite the text of
    " the new NERDTree!
    if !has_key(l:options, 'where') || empty(l:options['where'])
        call self.getNerdtree().render()
        call self.putCursorHere(0, 0)
    endif
endfunction

" FUNCTION: TreeDirNode.addChild(treenode, inOrder) {{{1
" Adds the given treenode to the list of children for this node
"
" Args:
" -treenode: the node to add
" -inOrder: 1 if the new node should be inserted in sorted order
function! s:TreeDirNode.addChild(treenode, inOrder)
    call add(self.children, a:treenode)
    let a:treenode.parent = self

    if a:inOrder
        call self.sortChildren()
    endif
endfunction

" FUNCTION: TreeDirNode.close() {{{1
" Mark this TreeDirNode as closed.
function! s:TreeDirNode.close()

    " Close all directories in this directory node's cascade. This is
    " necessary to ensure consistency when cascades are rendered.
    for l:dirNode in self.getCascade()
        let l:dirNode.isOpen = 0
    endfor
endfunction

" FUNCTION: TreeDirNode.closeChildren() {{{1
" Recursively close any directory nodes that are descendants of this node.
function! s:TreeDirNode.closeChildren()
    for l:child in self.children
        if l:child.path.isDirectory
            call l:child.close()
            call l:child.closeChildren()
        endif
    endfor
endfunction

" FUNCTION: TreeDirNode.createChild(path, inOrder) {{{1
" Instantiates a new child node for this node with the given path. The new
" nodes parent is set to this node.
"
" Args:
" path: a Path object that this node will represent/contain
" inOrder: 1 if the new node should be inserted in sorted order
"
" Returns:
" the newly created node
function! s:TreeDirNode.createChild(path, inOrder)
    let newTreeNode = g:NERDTreeFileNode.New(a:path, self.getNerdtree())
    call self.addChild(newTreeNode, a:inOrder)
    return newTreeNode
endfunction

" FUNCTION: TreeDirNode.displayString() {{{1
" Assemble and return a string that can represent this TreeDirNode object in
" the NERDTree window.
function! s:TreeDirNode.displayString()
    let l:result = ''

    " Build a label that identifies this TreeDirNode.
    let l:label = ''
    let l:cascade = self.getCascade()
    for l:dirNode in l:cascade
        let l:next = l:dirNode.path.displayString()
        let l:label .= l:label ==# '' ? l:next : substitute(l:next,'^.','','')
    endfor

    " Select the appropriate open/closed status indicator symbol.
    let l:symbol = (l:cascade[-1].isOpen ? g:NERDTreeDirArrowCollapsible : g:NERDTreeDirArrowExpandable )
    let l:symbol .= (g:NERDTreeDirArrowExpandable ==# '' ? '' : ' ')
    let l:flags = l:cascade[-1].path.flagSet.renderToString()

    return l:symbol . l:flags . l:label
endfunction

" FUNCTION: TreeDirNode.findNode(path) {{{1
" Will find one of the children (recursively) that has the given path
"
" Args:
" path: a path object
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
            if retVal !=# {}
                return retVal
            endif
        endfor
    endif
    return {}
endfunction

" FUNCTION: TreeDirNode.getCascade() {{{1
" Return an array of dir nodes (starting from self) that can be cascade opened.
function! s:TreeDirNode.getCascade()
    if !self.isCascadable()
        return [self]
    endif

    let vc = self.getVisibleChildren()
    let visChild = vc[0]

    return [self] + visChild.getCascade()
endfunction

" FUNCTION: TreeDirNode.getCascadeRoot() {{{1
" Return the first directory node in the cascade in which this directory node
" is rendered.
function! s:TreeDirNode.getCascadeRoot()

    " Don't search above the current NERDTree root node.
    if self.isRoot()
        return self
    endif

    let l:cascadeRoot = self
    let l:parent = self.parent

    while !empty(l:parent) && !l:parent.isRoot()

        if index(l:parent.getCascade(), self) ==# -1
            break
        endif

        let l:cascadeRoot = l:parent
        let l:parent = l:parent.parent
    endwhile

    return l:cascadeRoot
endfunction

" FUNCTION: TreeDirNode.getChildCount() {{{1
" Returns the number of children this node has
function! s:TreeDirNode.getChildCount()
    return len(self.children)
endfunction

" FUNCTION: TreeDirNode.getChild(path) {{{1
" Returns child node of this node that has the given path or {} if no such node
" exists.
"
" This function doesnt not recurse into child dir nodes
"
" Args:
" path: a path object
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

" FUNCTION: TreeDirNode.getChildByIndex(indx, visible) {{{1
" returns the child at the given index
"
" Args:
" indx: the index to get the child from
" visible: 1 if only the visible children array should be used, 0 if all the
" children should be searched.
function! s:TreeDirNode.getChildByIndex(indx, visible)
    let array_to_search = a:visible? self.getVisibleChildren() : self.children
    if a:indx > len(array_to_search)
        throw 'NERDTree.InvalidArgumentsError: Index is out of bounds.'
    endif
    return array_to_search[a:indx]
endfunction

" FUNCTION: TreeDirNode.getChildIndex(path) {{{1
" Returns the index of the child node of this node that has the given path or
" -1 if no such node exists.
"
" This function doesnt not recurse into child dir nodes
"
" Args:
" path: a path object
function! s:TreeDirNode.getChildIndex(path)
    if stridx(a:path.str(), self.path.str(), 0) ==# -1
        return -1
    endif

    "do a binary search for the child
    let a = 0
    let z = self.getChildCount()
    while a < z
        let mid = (a+z)/2
        let diff = nerdtree#compareNodePaths(a:path, self.children[mid].path)

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

" FUNCTION: TreeDirNode.getDirChildren() {{{1
" Return a list of all child nodes from 'self.children' that are of type
" TreeDirNode. This function supports http://github.com/scrooloose/nerdtree-project-plugin.git.
function! s:TreeDirNode.getDirChildren()
    return filter(copy(self.children), 'v:val.path.isDirectory ==# 1')
endfunction

" FUNCTION: TreeDirNode._glob(pattern, all) {{{1
" Return a list of strings naming the descendants of the directory in this
" TreeDirNode object that match the specified glob pattern.
"
" Args:
" pattern: (string) the glob pattern to apply
" all: (0 or 1) if 1, include '.' and '..' if they match 'pattern'; if 0,
"      always exclude them
"
" Note: If the pathnames in the result list are below the working directory,
" they are returned as pathnames relative to that directory. This is because
" this function, internally, attempts to obey 'wildignore' rules that use
" relative paths.
function! s:TreeDirNode._glob(pattern, all)

    " Construct a path specification such that globpath() will return
    " relative pathnames, if possible.
    if self.path.str() ==# getcwd()
        let l:pathSpec = ','
    else
        let l:pathSpec = escape(fnamemodify(self.path.str({'format': 'Glob'}), ':.'), ',')

        " On Windows, the drive letter may be removed by "fnamemodify()".
        if nerdtree#runningWindows() && l:pathSpec[0] == nerdtree#slash()
            let l:pathSpec = self.path.drive . l:pathSpec
        endif
    endif

    let l:globList = []

    " See ':h version7.txt' and ':h version8.txt' for details on the
    " development of the glob() and globpath() functions.
    if v:version > 704 || (v:version ==# 704 && has('patch654'))
        let l:globList = globpath(l:pathSpec, a:pattern, !g:NERDTreeRespectWildIgnore, 1, 0)
    elseif v:version ==# 704 && has('patch279')
        let l:globList = globpath(l:pathSpec, a:pattern, !g:NERDTreeRespectWildIgnore, 1)
    elseif v:version > 702 || (v:version ==# 702 && has('patch051'))
        let l:globString = globpath(l:pathSpec, a:pattern, !g:NERDTreeRespectWildIgnore)
        let l:globList = split(l:globString, "\n")
    else
        let l:globString = globpath(l:pathSpec, a:pattern)
        let l:globList = split(l:globString, "\n")
    endif

    " If a:all is false, filter '.' and '..' from the output.
    if !a:all
        let l:toRemove = []

        for l:file in l:globList
            let l:tail = fnamemodify(l:file, ':t')

            " If l:file has a trailing slash, then its :tail will be ''. Use
            " :h to drop the slash and the empty string after it; then use :t
            " to get the directory name.
            if l:tail ==# ''
                let l:tail = fnamemodify(l:file, ':h:t')
            endif

            if l:tail ==# '.' || l:tail ==# '..'
                call add(l:toRemove, l:file)
                if len(l:toRemove) ==# 2
                    break
                endif
            endif
        endfor

        for l:file in l:toRemove
            call remove(l:globList, index(l:globList, l:file))
        endfor
    endif

    return l:globList
endfunction

" FUNCTION: TreeDirNode.GetSelected() {{{1
" Returns the current node if it is a dir node, or else returns the current
" nodes parent
unlet s:TreeDirNode.GetSelected
function! s:TreeDirNode.GetSelected()
    let currentDir = g:NERDTreeFileNode.GetSelected()
    if currentDir !=# {} && !currentDir.isRoot()
        if currentDir.path.isDirectory ==# 0
            let currentDir = currentDir.parent
        endif
    endif
    return currentDir
endfunction

" FUNCTION: TreeDirNode.getVisibleChildCount() {{{1
" Returns the number of visible children this node has
function! s:TreeDirNode.getVisibleChildCount()
    return len(self.getVisibleChildren())
endfunction

" FUNCTION: TreeDirNode.getVisibleChildren() {{{1
" Returns a list of children to display for this node, in the correct order
"
" Return:
" an array of treenodes
function! s:TreeDirNode.getVisibleChildren()
    let toReturn = []
    for i in self.children
        if i.path.ignore(self.getNerdtree()) ==# 0
            call add(toReturn, i)
        endif
    endfor
    return toReturn
endfunction

" FUNCTION: TreeDirNode.hasVisibleChildren() {{{1
" returns 1 if this node has any childre, 0 otherwise..
function! s:TreeDirNode.hasVisibleChildren()
    return self.getVisibleChildCount() !=# 0
endfunction

" FUNCTION: TreeDirNode.isCascadable() {{{1
" true if this dir has only one visible child that is also a dir
" false if this dir is bookmarked or symlinked. Why? Two reasons:
"  1. If cascaded, we don't know which dir is bookmarked or is a symlink.
"  2. If the parent is a symlink or is bookmarked, you end up with unparsable
"     text, and NERDTree cannot get the path of any child node.
" Also, return false if this directory is the tree root, which should never be
" part of a cascade.
function! s:TreeDirNode.isCascadable()
    if g:NERDTreeCascadeSingleChildDir ==# 0
        return 0
    endif

    if self.isRoot()
        return 0
    endif

    if self.path.isSymLink
        return 0
    endif

    for i in g:NERDTreeBookmark.Bookmarks()
        if i.path.equals(self.path)
            return 0
        endif
    endfor

    let c = self.getVisibleChildren()
    return len(c) ==# 1 && c[0].path.isDirectory
endfunction

" FUNCTION: TreeDirNode._initChildren() {{{1
" Removes all childen from this node and re-reads them
"
" Args:
" silent: 1 if the function should not echo any 'please wait' messages for
" large directories
"
" Return: the number of child nodes read
function! s:TreeDirNode._initChildren(silent)
    "remove all the current child nodes
    let self.children = []

    let files = self._glob('*', 1) + self._glob('.*', 0)

    if !a:silent && len(files) > g:NERDTreeNotificationThreshold
        call nerdtree#echo('Please wait, caching a large dir ...')
    endif

    let invalidFilesFound = 0
    for i in files
        try
            let path = g:NERDTreePath.New(i)
            call self.createChild(path, 0)
            call g:NERDTreePathNotifier.NotifyListeners('init', path, self.getNerdtree(), {})
        catch /^NERDTree.\(InvalidArguments\|InvalidFiletype\)Error/
            let invalidFilesFound += 1
        endtry
    endfor

    let g:NERDTreeOldSortOrder = g:NERDTreeSortOrder
    call self.sortChildren()

    call nerdtree#echo('')

    if invalidFilesFound
        call nerdtree#echoWarning(invalidFilesFound . ' file(s) could not be loaded into the NERD tree')
    endif
    return self.getChildCount()
endfunction

" FUNCTION: TreeDirNode.New(path, nerdtree) {{{1
" Return a new TreeDirNode object with the given path and parent.
"
" Args:
" path: dir that the node represents
" nerdtree: the tree the node belongs to
function! s:TreeDirNode.New(path, nerdtree)
    if a:path.isDirectory !=# 1
        throw 'NERDTree.InvalidArgumentsError: A TreeDirNode object must be instantiated with a directory Path object.'
    endif

    let newTreeNode = copy(self)
    let newTreeNode.path = a:path

    let newTreeNode.isOpen = 0
    let newTreeNode.children = []

    let newTreeNode.parent = {}
    let newTreeNode._nerdtree = a:nerdtree

    return newTreeNode
endfunction

" FUNCTION: TreeDirNode.open([options]) {{{1
" Open this directory node in the current tree or elsewhere if special options
" are provided. Return 0 if options were processed. Otherwise, return the
" number of new cached nodes.
function! s:TreeDirNode.open(...)
    let l:options = a:0 ? a:1 : {}

    " If special options were specified, process them and return.
    if has_key(l:options, 'where') && !empty(l:options['where'])
        let l:opener = g:NERDTreeOpener.New(self.path, l:options)
        call l:opener.open(self)
        return 0
    endif

    " Open any ancestors of this node that render within the same cascade.
    let l:parent = self.parent
    while !empty(l:parent) && !l:parent.isRoot()
        if index(l:parent.getCascade(), self) >= 0
            let l:parent.isOpen = 1
            let l:parent = l:parent.parent
        else
            break
        endif
    endwhile

    let self.isOpen = 1

    let l:numChildrenCached = 0
    if empty(self.children)
        let l:numChildrenCached = self._initChildren(0)
    endif

    return l:numChildrenCached
endfunction

" FUNCTION: TreeDirNode.openAlong([opts]) {{{1
" recursive open the dir if it has only one directory child.
"
" return the level of opened directories.
function! s:TreeDirNode.openAlong(...)
    let opts = a:0 ? a:1 : {}
    let level = 0

    let node = self
    while node.path.isDirectory
        call node.open(opts)
        let level += 1
        if node.getVisibleChildCount() ==# 1
            let node = node.getChildByIndex(0, 1)
        else
            break
        endif
    endwhile
    return level
endfunction

" FUNCTION: TreeDirNode.openExplorer() {{{1
" Open an explorer window for this node in the previous window. The explorer
" can be a NERDTree window or a netrw window.
function! s:TreeDirNode.openExplorer()
    execute 'wincmd p'
    execute 'edit '.self.path.str({'format':'Edit'})
endfunction

" FUNCTION: TreeDirNode.openInNewTab(options) {{{1
unlet s:TreeDirNode.openInNewTab
function! s:TreeDirNode.openInNewTab(options)
    call nerdtree#deprecated('TreeDirNode.openInNewTab', 'is deprecated, use open() instead')
    call self.open({'where': 't'})
endfunction

" FUNCTION: TreeDirNode._openInNewTab() {{{1
function! s:TreeDirNode._openInNewTab()
    tabnew
    call g:NERDTreeCreator.CreateTabTree(self.path.str())
endfunction

" FUNCTION: TreeDirNode.openRecursively() {{{1
" Open this directory node and any descendant directory nodes whose pathnames
" are not ignored.
function! s:TreeDirNode.openRecursively()
    silent call self.open()

    for l:child in self.children
        if l:child.path.isDirectory && !l:child.path.ignore(l:child.getNerdtree())
            call l:child.openRecursively()
        endif
    endfor
endfunction

" FUNCTION: TreeDirNode.refresh() {{{1
function! s:TreeDirNode.refresh()
    call self.path.refresh(self.getNerdtree())

    "if this node was ever opened, refresh its children
    if self.isOpen || !empty(self.children)
        let files = self._glob('*', 1) + self._glob('.*', 0)
        let newChildNodes = []
        let invalidFilesFound = 0
        for i in files
            try
                "create a new path and see if it exists in this nodes children
                let path = g:NERDTreePath.New(i)
                let newNode = self.getChild(path)
                if newNode !=# {}
                    call newNode.refresh()
                    call add(newChildNodes, newNode)

                "the node doesnt exist so create it
                else
                    let newNode = g:NERDTreeFileNode.New(path, self.getNerdtree())
                    let newNode.parent = self
                    call add(newChildNodes, newNode)
                endif
            catch /^NERDTree.\(InvalidArguments\|InvalidFiletype\)Error/
                let invalidFilesFound = 1
            endtry
        endfor

        "swap this nodes children out for the children we just read/refreshed
        let self.children = newChildNodes
        call self.sortChildren()

        if invalidFilesFound
            call nerdtree#echoWarning('some files could not be loaded into the NERD tree')
        endif
    endif
endfunction

" FUNCTION: TreeDirNode.refreshFlags() {{{1
unlet s:TreeDirNode.refreshFlags
function! s:TreeDirNode.refreshFlags()
    call self.path.refreshFlags(self.getNerdtree())
    for i in self.children
        call i.refreshFlags()
    endfor
endfunction

" FUNCTION: TreeDirNode.refreshDirFlags() {{{1
function! s:TreeDirNode.refreshDirFlags()
    call self.path.refreshFlags(self.getNerdtree())
endfunction

" FUNCTION: TreeDirNode.reveal(path) {{{1
" reveal the given path, i.e. cache and open all treenodes needed to display it
" in the UI
" Returns the revealed node
function! s:TreeDirNode.reveal(path, ...)
    let opts = a:0 ? a:1 : {}

    if !a:path.isUnder(self.path)
        throw 'NERDTree.InvalidArgumentsError: ' . a:path.str() . ' should be under ' . self.path.str()
    endif

    call self.open()

    if self.path.equals(a:path.getParent())
        let n = self.findNode(a:path)
        " We may be looking for a newly-saved file that isn't in the tree yet.
        if n ==# {}
            call self.refresh()
            let n = self.findNode(a:path)
        endif
        if has_key(opts, 'open')
            call n.open()
        endif
        return n
    endif

    let p = a:path
    while !p.getParent().equals(self.path)
        let p = p.getParent()
    endwhile

    let n = self.findNode(p)
    return n.reveal(a:path, opts)
endfunction

" FUNCTION: TreeDirNode.removeChild(treenode) {{{1
" Remove the given treenode from self.children.
" Throws NERDTree.ChildNotFoundError if the node is not found.
"
" Args:
" treenode: the node object to remove
function! s:TreeDirNode.removeChild(treenode)
    for i in range(0, self.getChildCount()-1)
        if self.children[i].equals(a:treenode)
            call remove(self.children, i)
            return
        endif
    endfor

    throw 'NERDTree.ChildNotFoundError: child node was not found'
endfunction

" FUNCTION: TreeDirNode.sortChildren() {{{1
" Sort self.children by alphabetical order and directory priority.
function! s:TreeDirNode.sortChildren()
    if count(g:NERDTreeSortOrder, '*') < 1
        call add(g:NERDTreeSortOrder, '*')
    endif
    let CompareFunc = function('nerdtree#compareNodes')
    call sort(self.children, CompareFunc)
    let g:NERDTreeOldSortOrder = g:NERDTreeSortOrder
endfunction

" FUNCTION: TreeDirNode.toggleOpen([options]) {{{1
" Opens this directory if it is closed and vice versa
function! s:TreeDirNode.toggleOpen(...)
    let opts = a:0 ? a:1 : {}
    if self.isOpen ==# 1
        call self.close()
    else
        if g:NERDTreeCascadeOpenSingleChildDir ==# 0
            call self.open(opts)
        else
            call self.openAlong(opts)
        endif
    endif
endfunction

" FUNCTION: TreeDirNode.transplantChild(newNode) {{{1
" Replaces the child of this with the given node (where the child node's full
" path matches a:newNode's fullpath). The search for the matching node is
" non-recursive
"
" Arg:
" newNode: the node to graft into the tree
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
