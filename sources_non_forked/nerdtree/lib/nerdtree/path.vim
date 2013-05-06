"we need to use this number many times for sorting... so we calculate it only
"once here
let s:NERDTreeSortStarIndex = index(g:NERDTreeSortOrder, '*')

"CLASS: Path
"============================================================
let s:Path = {}
let g:NERDTreePath = s:Path

"FUNCTION: Path.AbsolutePathFor(str) {{{1
function! s:Path.AbsolutePathFor(str)
    let prependCWD = 0
    if nerdtree#runningWindows()
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

"FUNCTION: Path.bookmarkNames() {{{1
function! s:Path.bookmarkNames()
    if !exists("self._bookmarkNames")
        call self.cacheDisplayString()
    endif
    return self._bookmarkNames
endfunction

"FUNCTION: Path.cacheDisplayString() {{{1
function! s:Path.cacheDisplayString()
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

"FUNCTION: Path.changeToDir() {{{1
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

"FUNCTION: Path.compareTo() {{{1
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

"FUNCTION: Path.Create(fullpath) {{{1
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

"FUNCTION: Path.copy(dest) {{{1
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

    let cmd = g:NERDTreeCopyCmd . " " . escape(self.str(), nerdtree#escChars()) . " " . escape(dest, nerdtree#escChars())
    let success = system(cmd)
    if success != 0
        throw "NERDTree.CopyError: Could not copy ''". self.str() ."'' to: '" . a:dest . "'"
    endif
endfunction

"FUNCTION: Path.CopyingSupported() {{{1
"
"returns 1 if copying is supported for this OS
function! s:Path.CopyingSupported()
    return exists('g:NERDTreeCopyCmd')
endfunction

"FUNCTION: Path.copyingWillOverwrite(dest) {{{1
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

"FUNCTION: Path.delete() {{{1
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
        let bookmark = g:NERDTreeBookmark.BookmarkFor(i)
        call bookmark.delete()
    endfor
endfunction

"FUNCTION: Path.displayString() {{{1
"
"Returns a string that specifies how the path should be represented as a
"string
function! s:Path.displayString()
    if self.cachedDisplayString ==# ""
        call self.cacheDisplayString()
    endif

    return self.cachedDisplayString
endfunction

"FUNCTION: Path.edit() {{{1
function! s:Path.edit()
    exec "edit " . self.str({'format': 'Edit'})
endfunction

"FUNCTION: Path.extractDriveLetter(fullpath) {{{1
"
"If running windows, cache the drive letter for this path
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

"FUNCTION: Path.exists() {{{1
"return 1 if this path points to a location that is readable or is a directory
function! s:Path.exists()
    let p = self.str()
    return filereadable(p) || isdirectory(p)
endfunction

"FUNCTION: Path.getDir() {{{1
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

"FUNCTION: Path.getParent() {{{1
"
"Returns a new path object for this paths parent
"
"Return:
"a new Path object
function! s:Path.getParent()
    if nerdtree#runningWindows()
        let path = self.drive . '\' . join(self.pathSegments[0:-2], '\')
    else
        let path = '/'. join(self.pathSegments[0:-2], '/')
    endif

    return s:Path.New(path)
endfunction

"FUNCTION: Path.getLastPathComponent(dirSlash) {{{1
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

"FUNCTION: Path.getSortOrderIndex() {{{1
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

"FUNCTION: Path.isUnixHiddenFile() {{{1
"check for unix hidden files
function! s:Path.isUnixHiddenFile()
    return self.getLastPathComponent(0) =~# '^\.'
endfunction

"FUNCTION: Path.isUnixHiddenPath() {{{1
"check for unix path with hidden components
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

"FUNCTION: Path.ignore() {{{1
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
    if b:NERDTreeShowHidden ==# 0 && self.isUnixHiddenFile()
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

"FUNCTION: Path._ignorePatternMatches(pattern) {{{1
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

"FUNCTION: Path.isUnder(path) {{{1
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

"FUNCTION: Path.JoinPathStrings(...) {{{1
function! s:Path.JoinPathStrings(...)
    let components = []
    for i in a:000
        let components = extend(components, split(i, '/'))
    endfor
    return '/' . join(components, '/')
endfunction

"FUNCTION: Path.equals() {{{1
"
"Determines whether 2 path objects are "equal".
"They are equal if the paths they represent are the same
"
"Args:
"path: the other path obj to compare this with
function! s:Path.equals(path)
    return self.str() ==# a:path.str()
endfunction

"FUNCTION: Path.New() {{{1
"The Constructor for the Path object
function! s:Path.New(path)
    let newPath = copy(self)

    call newPath.readInfoFromDisk(s:Path.AbsolutePathFor(a:path))

    let newPath.cachedDisplayString = ""

    return newPath
endfunction

"FUNCTION: Path.Slash() {{{1
"return the slash to use for the current OS
function! s:Path.Slash()
    return nerdtree#runningWindows() ? '\' : '/'
endfunction

"FUNCTION: Path.Resolve() {{{1
"Invoke the vim resolve() function and return the result
"This is necessary because in some versions of vim resolve() removes trailing
"slashes while in other versions it doesn't.  This always removes the trailing
"slash
function! s:Path.Resolve(path)
    let tmp = resolve(a:path)
    return tmp =~# '.\+/$' ? substitute(tmp, '/$', '', '') : tmp
endfunction

"FUNCTION: Path.readInfoFromDisk(fullpath) {{{1
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

"FUNCTION: Path.refresh() {{{1
function! s:Path.refresh()
    call self.readInfoFromDisk(self.str())
    call self.cacheDisplayString()
endfunction

"FUNCTION: Path.rename() {{{1
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
        let b = g:NERDTreeBookmark.BookmarkFor(i)
        call b.setPath(copy(self))
    endfor
    call g:NERDTreeBookmark.Write()
endfunction

"FUNCTION: Path.str() {{{1
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

    if nerdtree#has_opt(options, 'escape')
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

"FUNCTION: Path._strForUI() {{{1
function! s:Path._strForUI()
    let toReturn = '/' . join(self.pathSegments, '/')
    if self.isDirectory && toReturn != '/'
        let toReturn  = toReturn . '/'
    endif
    return toReturn
endfunction

"FUNCTION: Path._strForCd() {{{1
"
" returns a string that can be used with :cd
function! s:Path._strForCd()
    return escape(self.str(), nerdtree#escChars())
endfunction

"FUNCTION: Path._strForEdit() {{{1
"
"Return: the string for this path that is suitable to be used with the :edit
"command
function! s:Path._strForEdit()
    let p = escape(self.str({'format': 'UI'}), nerdtree#escChars())
    let cwd = getcwd() . s:Path.Slash()

    "return a relative path if we can
    let isRelative = 0
    if nerdtree#runningWindows()
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

"FUNCTION: Path._strForGlob() {{{1
function! s:Path._strForGlob()
    let lead = s:Path.Slash()

    "if we are running windows then slap a drive letter on the front
    if nerdtree#runningWindows()
        let lead = self.drive . '\'
    endif

    let toReturn = lead . join(self.pathSegments, s:Path.Slash())

    if !nerdtree#runningWindows()
        let toReturn = escape(toReturn, nerdtree#escChars())
    endif
    return toReturn
endfunction

"FUNCTION: Path._str() {{{1
"
"Gets the string path for this path object that is appropriate for the OS.
"EG, in windows c:\foo\bar
"    in *nix  /foo/bar
function! s:Path._str()
    let lead = s:Path.Slash()

    "if we are running windows then slap a drive letter on the front
    if nerdtree#runningWindows()
        let lead = self.drive . '\'
    endif

    return lead . join(self.pathSegments, s:Path.Slash())
endfunction

"FUNCTION: Path.strTrunk() {{{1
"Gets the path without the last segment on the end.
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
            if str == expand('#' . b . ':p')
                return t+1
            endif
        endfor
    endfor
    return 0
endfunction

"FUNCTION: Path.WinToUnixPath(pathstr){{{1
"Takes in a windows path and returns the unix equiv
"
"A class level method
"
"Args:
"pathstr: the windows path to convert
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
