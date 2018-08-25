" ============================================================================
" CLASS: Path
"
" The Path class provides an abstracted representation of a file system
" pathname.  Various operations on pathnames are provided and a number of
" representations of a given path name can be accessed here.
" ============================================================================


let s:Path = {}
let g:NERDTreePath = s:Path

" FUNCTION: Path.AbsolutePathFor(pathStr) {{{1
function! s:Path.AbsolutePathFor(pathStr)
    let l:prependWorkingDir = 0

    if nerdtree#runningWindows()
        let l:prependWorkingDir = a:pathStr !~# '^.:\(\\\|\/\)' && a:pathStr !~# '^\(\\\\\|\/\/\)'
    else
        let l:prependWorkingDir = a:pathStr !~# '^/'
    endif

    let l:result = a:pathStr

    if l:prependWorkingDir
        let l:result = getcwd() . s:Path.Slash() . a:pathStr
    endif

    return l:result
endfunction

" FUNCTION: Path.bookmarkNames() {{{1
function! s:Path.bookmarkNames()
    if !exists("self._bookmarkNames")
        call self.cacheDisplayString()
    endif
    return self._bookmarkNames
endfunction

" FUNCTION: Path.cacheDisplayString() {{{1
function! s:Path.cacheDisplayString() abort
    let self.cachedDisplayString = self.getLastPathComponent(1)

    if self.isExecutable
        let self.cachedDisplayString = self.cachedDisplayString . '*'
    endif

    let self._bookmarkNames = []
    for i in g:NERDTreeBookmark.Bookmarks()
        if i.path.equals(self)
            call add(self._bookmarkNames, i.name)
        endif
    endfor
    if !empty(self._bookmarkNames) && g:NERDTreeMarkBookmarks == 1
        let self.cachedDisplayString .= ' {' . join(self._bookmarkNames) . '}'
    endif

    if self.isSymLink
        let self.cachedDisplayString .=  ' -> ' . self.symLinkDest
    endif

    if self.isReadOnly
        let self.cachedDisplayString .=  ' ['.g:NERDTreeGlyphReadOnly.']'
    endif
endfunction

" FUNCTION: Path.changeToDir() {{{1
function! s:Path.changeToDir()
    let dir = self.str({'format': 'Cd'})
    if self.isDirectory ==# 0
        let dir = self.getParent().str({'format': 'Cd'})
    endif

    try
        execute "cd " . dir
        call nerdtree#echo("CWD is now: " . getcwd())
    catch
        throw "NERDTree.PathChangeError: cannot change CWD to " . dir
    endtry
endfunction

" FUNCTION: Path.compareTo() {{{1
"
" Compares this Path to the given path and returns 0 if they are equal, -1 if
" this Path is "less than" the given path, or 1 if it is "greater".
"
" Args:
" path: the path object to compare this to
"
" Return:
" 1, -1 or 0
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
        if !g:NERDTreeSortHiddenFirst
            let thisPath = substitute(thisPath, '^[._]', '', '')
            let thatPath = substitute(thatPath, '^[._]', '', '')
        endif
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

" FUNCTION: Path.Create(fullpath) {{{1
"
" Factory method.
"
" Creates a path object with the given path. The path is also created on the
" filesystem. If the path already exists, a NERDTree.Path.Exists exception is
" thrown. If any other errors occur, a NERDTree.Path exception is thrown.
"
" Args:
" fullpath: the full filesystem path to the file/dir to create
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
            call s:Path.createParentDirectories(a:fullpath)
            call writefile([], a:fullpath)
        endif
    catch
        throw "NERDTree.CreatePathError: Could not create path: '" . a:fullpath . "'"
    endtry

    return s:Path.New(a:fullpath)
endfunction

" FUNCTION: Path.copy(dest) {{{1
"
" Copies the file/dir represented by this Path to the given location
"
" Args:
" dest: the location to copy this dir/file to
function! s:Path.copy(dest)
    if !s:Path.CopyingSupported()
        throw "NERDTree.CopyingNotSupportedError: Copying is not supported on this OS"
    endif

    call s:Path.createParentDirectories(a:dest)

    if exists('g:NERDTreeCopyCmd')
        let cmd_prefix = g:NERDTreeCopyCmd
    else
        let cmd_prefix = (self.isDirectory ? g:NERDTreeCopyDirCmd : g:NERDTreeCopyFileCmd)
    endif

    let cmd = cmd_prefix . " " . escape(self.str(), self._escChars()) . " " . escape(a:dest, self._escChars())
    let success = system(cmd)
    if v:shell_error != 0
        throw "NERDTree.CopyError: Could not copy ''". self.str() ."'' to: '" . a:dest . "'"
    endif
endfunction

" FUNCTION: Path.CopyingSupported() {{{1
"
" returns 1 if copying is supported for this OS
function! s:Path.CopyingSupported()
    return exists('g:NERDTreeCopyCmd') || (exists('g:NERDTreeCopyDirCmd') && exists('g:NERDTreeCopyFileCmd'))
endfunction

" FUNCTION: Path.copyingWillOverwrite(dest) {{{1
"
" returns 1 if copy this path to the given location will cause files to
" overwritten
"
" Args:
" dest: the location this path will be copied to
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

" FUNCTION: Path.createParentDirectories(path) {{{1
"
" create parent directories for this path if needed
" without throwing any errors if those directories already exist
"
" Args:
" path: full path of the node whose parent directories may need to be created
function! s:Path.createParentDirectories(path)
    let dir_path = fnamemodify(a:path, ':h')
    if !isdirectory(dir_path)
        call mkdir(dir_path, 'p')
    endif
endfunction

" FUNCTION: Path.delete() {{{1
"
" Deletes the file or directory represented by this path.
"
" Throws NERDTree.Path.Deletion exceptions
function! s:Path.delete()
    if self.isDirectory

        let cmd = g:NERDTreeRemoveDirCmd . self.str({'escape': 1})
        let success = system(cmd)

        if v:shell_error != 0
            throw "NERDTree.PathDeletionError: Could not delete directory: '" . self.str() . "'"
        endif
    else
        if exists('g:NERDTreeRemoveFileCmd')
            let cmd = g:NERDTreeRemoveFileCmd . self.str({'escape': 1})
            let success = system(cmd)
        else
            let success = delete(self.str())
        endif

        if success != 0
            throw "NERDTree.PathDeletionError: Could not delete file: '" . self.str() . "'"
        endif
    endif

    "delete all bookmarks for this path
    for i in self.bookmarkNames()
        let bookmark = g:NERDTreeBookmark.BookmarkFor(i)
        call bookmark.delete()
    endfor
endfunction

" FUNCTION: Path.displayString() {{{1
"
" Returns a string that specifies how the path should be represented as a
" string
function! s:Path.displayString()
    if self.cachedDisplayString ==# ""
        call self.cacheDisplayString()
    endif

    return self.cachedDisplayString
endfunction

" FUNCTION: Path.edit() {{{1
function! s:Path.edit()
    exec "edit " . self.str({'format': 'Edit'})
endfunction

" FUNCTION: Path.extractDriveLetter(fullpath) {{{1
"
" If running windows, cache the drive letter for this path
function! s:Path.extractDriveLetter(fullpath)
    if nerdtree#runningWindows()
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

" FUNCTION: Path.exists() {{{1
" return 1 if this path points to a location that is readable or is a directory
function! s:Path.exists()
    let p = self.str()
    return filereadable(p) || isdirectory(p)
endfunction

" FUNCTION: Path._escChars() {{{1
function! s:Path._escChars()
    if nerdtree#runningWindows()
        return " `\|\"#%&,?()\*^<>$"
    endif

    return " \\`\|\"#%&,?()\*^<>[]$"
endfunction

" FUNCTION: Path.getDir() {{{1
"
" Returns this path if it is a directory, else this paths parent.
"
" Return:
" a Path object
function! s:Path.getDir()
    if self.isDirectory
        return self
    else
        return self.getParent()
    endif
endfunction

" FUNCTION: Path.getParent() {{{1
"
" Returns a new path object for this paths parent
"
" Return:
" a new Path object
function! s:Path.getParent()
    if nerdtree#runningWindows()
        let path = self.drive . '\' . join(self.pathSegments[0:-2], '\')
    else
        let path = '/'. join(self.pathSegments[0:-2], '/')
    endif

    return s:Path.New(path)
endfunction

" FUNCTION: Path.getLastPathComponent(dirSlash) {{{1
"
" Gets the last part of this path.
"
" Args:
" dirSlash: if 1 then a trailing slash will be added to the returned value for
" directory nodes.
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

" FUNCTION: Path.getSortOrderIndex() {{{1
" returns the index of the pattern in g:NERDTreeSortOrder that this path matches
function! s:Path.getSortOrderIndex()
    let i = 0
    while i < len(g:NERDTreeSortOrder)
        if  self.getLastPathComponent(1) =~# g:NERDTreeSortOrder[i]
            return i
        endif
        let i = i + 1
    endwhile

    return index(g:NERDTreeSortOrder, '*')
endfunction

" FUNCTION: Path._splitChunks(path) {{{1
" returns a list of path chunks
function! s:Path._splitChunks(path)
    let chunks = split(a:path, '\(\D\+\|\d\+\)\zs')
    let i = 0
    while i < len(chunks)
        "convert number literals to numbers
        if match(chunks[i], '^\d\+$') == 0
            let chunks[i] = str2nr(chunks[i])
        endif
        let i = i + 1
    endwhile
    return chunks
endfunction

" FUNCTION: Path.getSortKey() {{{1
" returns a key used in compare function for sorting
function! s:Path.getSortKey()
    if !exists("self._sortKey") || g:NERDTreeSortOrder !=# g:NERDTreeOldSortOrder
        let path = self.getLastPathComponent(1)
        if !g:NERDTreeSortHiddenFirst
            let path = substitute(path, '^[._]', '', '')
        endif
        if !g:NERDTreeCaseSensitiveSort
            let path = tolower(path)
        endif
        if !g:NERDTreeNaturalSort
            let self._sortKey = [self.getSortOrderIndex(), path]
        else
            let self._sortKey = [self.getSortOrderIndex()] + self._splitChunks(path)
        endif
    endif

    return self._sortKey
endfunction

" FUNCTION: Path.isHiddenUnder(path) {{{1
function! s:Path.isHiddenUnder(path)

    if !self.isUnder(a:path)
        return 0
    endif

    let l:startIndex = len(a:path.pathSegments)
    let l:segments = self.pathSegments[l:startIndex : ]

    for l:segment in l:segments

        if l:segment =~# '^\.'
            return 1
        endif
    endfor

    return 0
endfunction

" FUNCTION: Path.isUnixHiddenFile() {{{1
" check for unix hidden files
function! s:Path.isUnixHiddenFile()
    return self.getLastPathComponent(0) =~# '^\.'
endfunction

" FUNCTION: Path.isUnixHiddenPath() {{{1
" check for unix path with hidden components
function! s:Path.isUnixHiddenPath()
    if self.getLastPathComponent(0) =~# '^\.'
        return 1
    else
        for segment in self.pathSegments
            if segment =~# '^\.'
                return 1
            endif
        endfor
        return 0
    endif
endfunction

" FUNCTION: Path.ignore(nerdtree) {{{1
" returns true if this path should be ignored
function! s:Path.ignore(nerdtree)
    "filter out the user specified paths to ignore
    if a:nerdtree.ui.isIgnoreFilterEnabled()
        for i in g:NERDTreeIgnore
            if self._ignorePatternMatches(i)
                return 1
            endif
        endfor

        for callback in g:NERDTree.PathFilters()
            if {callback}({'path': self, 'nerdtree': a:nerdtree})
                return 1
            endif
        endfor
    endif

    "dont show hidden files unless instructed to
    if !a:nerdtree.ui.getShowHidden() && self.isUnixHiddenFile()
        return 1
    endif

    if a:nerdtree.ui.getShowFiles() ==# 0 && self.isDirectory ==# 0
        return 1
    endif

    return 0
endfunction

" FUNCTION: Path._ignorePatternMatches(pattern) {{{1
" returns true if this path matches the given ignore pattern
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

" FUNCTION: Path.isAncestor(path) {{{1
" return 1 if this path is somewhere above the given path in the filesystem.
"
" a:path should be a dir
function! s:Path.isAncestor(path)
    if !self.isDirectory
        return 0
    endif

    let this = self.str()
    let that = a:path.str()
    return stridx(that, this) == 0
endfunction

" FUNCTION: Path.isUnder(path) {{{1
" return 1 if this path is somewhere under the given path in the filesystem.
function! s:Path.isUnder(path)
    if a:path.isDirectory == 0
        return 0
    endif

    let this = self.str()
    let that = a:path.str()
    return stridx(this, that . s:Path.Slash()) == 0
endfunction

" FUNCTION: Path.JoinPathStrings(...) {{{1
function! s:Path.JoinPathStrings(...)
    let components = []
    for i in a:000
        let components = extend(components, split(i, '/'))
    endfor
    return '/' . join(components, '/')
endfunction

" FUNCTION: Path.equals() {{{1
"
" Determines whether 2 path objects are "equal".
" They are equal if the paths they represent are the same
"
" Args:
" path: the other path obj to compare this with
function! s:Path.equals(path)
    return self.str() ==# a:path.str()
endfunction

" FUNCTION: Path.New(pathStr) {{{1
function! s:Path.New(pathStr)
    let l:newPath = copy(self)

    call l:newPath.readInfoFromDisk(s:Path.AbsolutePathFor(a:pathStr))

    let l:newPath.cachedDisplayString = ''
    let l:newPath.flagSet = g:NERDTreeFlagSet.New()

    return l:newPath
endfunction

" FUNCTION: Path.Slash() {{{1
" Return the path separator used by the underlying file system.  Special
" consideration is taken for the use of the 'shellslash' option on Windows
" systems.
function! s:Path.Slash()

    if nerdtree#runningWindows()
        if exists('+shellslash') && &shellslash
            return '/'
        endif

        return '\'
    endif

    return '/'
endfunction

" FUNCTION: Path.Resolve() {{{1
" Invoke the vim resolve() function and return the result
" This is necessary because in some versions of vim resolve() removes trailing
" slashes while in other versions it doesn't.  This always removes the trailing
" slash
function! s:Path.Resolve(path)
    let tmp = resolve(a:path)
    return tmp =~# '.\+/$' ? substitute(tmp, '/$', '', '') : tmp
endfunction

" FUNCTION: Path.readInfoFromDisk(fullpath) {{{1
"
"
" Throws NERDTree.Path.InvalidArguments exception.
function! s:Path.readInfoFromDisk(fullpath)
    call self.extractDriveLetter(a:fullpath)

    let fullpath = s:Path.WinToUnixPath(a:fullpath)

    if getftype(fullpath) ==# "fifo"
        throw "NERDTree.InvalidFiletypeError: Cant handle FIFO files: " . a:fullpath
    endif

    let self.pathSegments = filter(split(fullpath, '/'), '!empty(v:val)')

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

" FUNCTION: Path.refresh(nerdtree) {{{1
function! s:Path.refresh(nerdtree)
    call self.readInfoFromDisk(self.str())
    call g:NERDTreePathNotifier.NotifyListeners('refresh', self, a:nerdtree, {})
    call self.cacheDisplayString()
endfunction

" FUNCTION: Path.refreshFlags(nerdtree) {{{1
function! s:Path.refreshFlags(nerdtree)
    call g:NERDTreePathNotifier.NotifyListeners('refreshFlags', self, a:nerdtree, {})
    call self.cacheDisplayString()
endfunction

" FUNCTION: Path.rename() {{{1
"
" Renames this node on the filesystem
function! s:Path.rename(newPath)
    if a:newPath ==# ''
        throw "NERDTree.InvalidArgumentsError: Invalid newPath for renaming = ". a:newPath
    endif

    call s:Path.createParentDirectories(a:newPath)

    let success =  rename(self.str(), a:newPath)
    if success != 0
        throw "NERDTree.PathRenameError: Could not rename: '" . self.str() . "'" . 'to:' . a:newPath
    endif
    call self.readInfoFromDisk(a:newPath)

    for i in self.bookmarkNames()
        let b = g:NERDTreeBookmark.BookmarkFor(i)
        call b.setPath(copy(self))
    endfor
    call g:NERDTreeBookmark.Write()
endfunction

" FUNCTION: Path.str() {{{1
" Return a string representation of this Path object.
"
" Args:
" This function takes a single dictionary (optional) with keys and values that
" specify how the returned pathname should be formatted.
"
" The dictionary may have the following keys:
"  'format'
"  'escape'
"  'truncateTo'
"
" The 'format' key may have a value of:
"  'Cd' - a string to be used with ":cd" and similar commands
"  'Edit' - a string to be used with ":edit" and similar commands
"  'UI' - a string to be displayed in the NERDTree user interface
"
" The 'escape' key, if specified, will cause the output to be escaped with
" Vim's internal "shellescape()" function.
"
" The 'truncateTo' key shortens the length of the path to that given by the
" value associated with 'truncateTo'. A '<' is prepended.
function! s:Path.str(...)
    let options = a:0 ? a:1 : {}
    let toReturn = ""

    if has_key(options, 'format')
        let format = options['format']
        if has_key(self, '_strFor' . format)
            exec 'let toReturn = self._strFor' . format . '()'
        else
            throw 'NERDTree.UnknownFormatError: unknown format "'. format .'"'
        endif
    else
        let toReturn = self._str()
    endif

    if nerdtree#has_opt(options, 'escape')
        let toReturn = shellescape(toReturn)
    endif

    if has_key(options, 'truncateTo')
        let limit = options['truncateTo']
        if strdisplaywidth(toReturn) > limit-1
            while strdisplaywidth(toReturn) > limit-1 && strchars(toReturn) > 0
                let toReturn = substitute(toReturn, '^.', '', '')
            endwhile
            if len(split(toReturn, '/')) > 1
                let toReturn = '</' . join(split(toReturn, '/')[1:], '/') . '/'
            else
                let toReturn = '<' . toReturn
            endif
        endif
    endif

    return toReturn
endfunction

" FUNCTION: Path._strForUI() {{{1
function! s:Path._strForUI()
    let toReturn = '/' . join(self.pathSegments, '/')
    if self.isDirectory && toReturn != '/'
        let toReturn  = toReturn . '/'
    endif
    return toReturn
endfunction

" FUNCTION: Path._strForCd() {{{1
" Return a string representation of this Path that is suitable for use as an
" argument to Vim's internal ":cd" command.
function! s:Path._strForCd()
    return fnameescape(self.str())
endfunction

" FUNCTION: Path._strForEdit() {{{1
" Return a string representation of this Path that is suitable for use as an
" argument to Vim's internal ":edit" command.
function! s:Path._strForEdit()

    " Make the path relative to the current working directory, if possible.
    let l:result = fnamemodify(self.str(), ':.')

    " On Windows, the drive letter may be removed by "fnamemodify()".  Add it
    " back, if necessary.
    if nerdtree#runningWindows() && l:result[0] == s:Path.Slash()
        let l:result = self.drive . l:result
    endif

    let l:result = fnameescape(l:result)

    if empty(l:result)
        let l:result = '.'
    endif

    return l:result
endfunction

" FUNCTION: Path._strForGlob() {{{1
function! s:Path._strForGlob()
    let lead = s:Path.Slash()

    "if we are running windows then slap a drive letter on the front
    if nerdtree#runningWindows()
        let lead = self.drive . '\'
    endif

    let toReturn = lead . join(self.pathSegments, s:Path.Slash())

    if !nerdtree#runningWindows()
        let toReturn = escape(toReturn, self._escChars())
    endif
    return toReturn
endfunction

" FUNCTION: Path._str() {{{1
" Return the absolute pathname associated with this Path object.  The pathname
" returned is appropriate for the underlying file system.
function! s:Path._str()
    let l:separator = s:Path.Slash()
    let l:leader = l:separator

    if nerdtree#runningWindows()
        let l:leader = self.drive . l:separator
    endif

    return l:leader . join(self.pathSegments, l:separator)
endfunction

" FUNCTION: Path.strTrunk() {{{1
" Gets the path without the last segment on the end.
function! s:Path.strTrunk()
    return self.drive . '/' . join(self.pathSegments[0:-2], '/')
endfunction

" FUNCTION: Path.tabnr() {{{1
" return the number of the first tab that is displaying this file
"
" return 0 if no tab was found
function! s:Path.tabnr()
    let str = self.str()
    for t in range(tabpagenr('$'))
        for b in tabpagebuflist(t+1)
            if str ==# expand('#' . b . ':p')
                return t+1
            endif
        endfor
    endfor
    return 0
endfunction

" FUNCTION: Path.WinToUnixPath(pathstr){{{1
" Takes in a windows path and returns the unix equiv
"
" A class level method
"
" Args:
" pathstr: the windows path to convert
function! s:Path.WinToUnixPath(pathstr)
    if !nerdtree#runningWindows()
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

" vim: set sw=4 sts=4 et fdm=marker:
