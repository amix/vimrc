"CLASS: Creator
"Creates primary/secondary/mirror nerdtree windows. Sets up all the window and
"buffer options and key mappings etc.
"============================================================
let s:Creator = {}
let g:NERDTreeCreator = s:Creator

"FUNCTION: s:Creator._bindMappings() {{{1
function! s:Creator._bindMappings()
    "make <cr> do the same as the default 'o' mapping
    exec "nnoremap <silent> <buffer> <cr> :call nerdtree#invokeKeyMap('". g:NERDTreeMapActivateNode ."')<cr>"

    call g:NERDTreeKeyMap.BindAll()

    command! -buffer -nargs=? Bookmark :call nerdtree#bookmarkNode('<args>')
    command! -buffer -complete=customlist,nerdtree#completeBookmarks -nargs=1 RevealBookmark :call nerdtree#revealBookmark('<args>')
    command! -buffer -complete=customlist,nerdtree#completeBookmarks -nargs=1 OpenBookmark :call nerdtree#openBookmark('<args>')
    command! -buffer -complete=customlist,nerdtree#completeBookmarks -nargs=* ClearBookmarks call nerdtree#clearBookmarks('<args>')
    command! -buffer -complete=customlist,nerdtree#completeBookmarks -nargs=+ BookmarkToRoot call g:NERDTreeBookmark.ToRoot('<args>')
    command! -buffer -nargs=0 ClearAllBookmarks call g:NERDTreeBookmark.ClearAll() <bar> call nerdtree#renderView()
    command! -buffer -nargs=0 ReadBookmarks call g:NERDTreeBookmark.CacheBookmarks(0) <bar> call nerdtree#renderView()
    command! -buffer -nargs=0 WriteBookmarks call g:NERDTreeBookmark.Write()
endfunction

"FUNCTION: s:Creator._broadcastInitEvent() {{{1
function! s:Creator._broadcastInitEvent()
    silent doautocmd User NERDTreeInit
endfunction

" FUNCTION: s:Creator.BufNamePrefix() {{{2
function! s:Creator.BufNamePrefix()
    return 'NERD_tree_'
endfunction

"FUNCTION: s:Creator.CreatePrimary(a:name) {{{1
function! s:Creator.CreatePrimary(name)
    let creator = s:Creator.New()
    call creator.createPrimary(a:name)
endfunction

"FUNCTION: s:Creator.createPrimary(a:name) {{{1
"name: the name of a bookmark or a directory
function! s:Creator.createPrimary(name)
    let path = self._pathForString(a:name)

    "if instructed to, then change the vim CWD to the dir the NERDTree is
    "inited in
    if g:NERDTreeChDirMode != 0
        call path.changeToDir()
    endif

    if nerdtree#treeExistsForTab()
        if nerdtree#isTreeOpen()
            call nerdtree#closeTree()
        endif
        unlet t:NERDTreeBufName
    endif

    let newRoot = g:NERDTreeDirNode.New(path)
    call newRoot.open()

    call self._createTreeWin()
    let b:treeShowHelp = 0
    let b:NERDTreeIgnoreEnabled = 1
    let b:NERDTreeShowFiles = g:NERDTreeShowFiles
    let b:NERDTreeShowHidden = g:NERDTreeShowHidden
    let b:NERDTreeShowBookmarks = g:NERDTreeShowBookmarks
    let b:NERDTreeRoot = newRoot
    let b:NERDTreeType = "primary"

    call nerdtree#renderView()
    call b:NERDTreeRoot.putCursorHere(0, 0)

    call self._broadcastInitEvent()
endfunction

"FUNCTION: s:Creator.CreateSecondary(dir) {{{1
function! s:Creator.CreateSecondary(dir)
    let creator = s:Creator.New()
    call creator.createSecondary(a:dir)
endfunction

"FUNCTION: s:Creator.createSecondary(dir) {{{1
function! s:Creator.createSecondary(dir)
    try
        let path = g:NERDTreePath.New(a:dir)
    catch /^NERDTree.InvalidArgumentsError/
        call nerdtree#echo("Invalid directory name:" . a:name)
        return
    endtry

    "we want the directory buffer to disappear when we do the :edit below
    setlocal bufhidden=wipe

    let previousBuf = expand("#")

    "we need a unique name for each secondary tree buffer to ensure they are
    "all independent
    exec "silent edit " . self._nextBufferName()

    let b:NERDTreePreviousBuf = bufnr(previousBuf)

    let b:NERDTreeRoot = g:NERDTreeDirNode.New(path)
    call b:NERDTreeRoot.open()

    call self._setCommonBufOptions()
    let b:NERDTreeType = "secondary"

    call nerdtree#renderView()

    call self._broadcastInitEvent()
endfunction

" FUNCTION: s:Creator.CreateMirror() {{{1
function! s:Creator.CreateMirror()
    let creator = s:Creator.New()
    call creator.createMirror()
endfunction

" FUNCTION: s:Creator.createMirror() {{{1
function! s:Creator.createMirror()
    "get the names off all the nerd tree buffers
    let treeBufNames = []
    for i in range(1, tabpagenr("$"))
        let nextName = nerdtree#tabpagevar(i, 'NERDTreeBufName')
        if nextName != -1 && (!exists("t:NERDTreeBufName") || nextName != t:NERDTreeBufName)
            call add(treeBufNames, nextName)
        endif
    endfor
    let treeBufNames = nerdtree#unique(treeBufNames)

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
        call nerdtree#echo("No trees to mirror")
        return
    endif

    if nerdtree#treeExistsForTab() && nerdtree#isTreeOpen()
        call nerdtree#closeTree()
    endif

    let t:NERDTreeBufName = bufferName
    call self._createTreeWin()
    exec 'buffer ' .  bufferName
    if !&hidden
        call nerdtree#renderView()
    endif
endfunction

"FUNCTION: s:Creator._createTreeWin() {{{1
"Inits the NERD tree window. ie. opens it, sizes it, sets all the local
"options etc
function! s:Creator._createTreeWin()
    "create the nerd tree window
    let splitLocation = g:NERDTreeWinPos ==# "left" ? "topleft " : "botright "
    let splitSize = g:NERDTreeWinSize

    if !exists('t:NERDTreeBufName')
        let t:NERDTreeBufName = self._nextBufferName()
        silent! exec splitLocation . 'vertical ' . splitSize . ' new'
        silent! exec "edit " . t:NERDTreeBufName
    else
        silent! exec splitLocation . 'vertical ' . splitSize . ' split'
        silent! exec "buffer " . t:NERDTreeBufName
    endif

    setlocal winfixwidth
    call self._setCommonBufOptions()
endfunction

"FUNCTION: s:Creator.New() {{{1
function! s:Creator.New()
    let newCreator = copy(self)
    return newCreator
endfunction

" FUNCTION: s:Creator._nextBufferName() {{{2
" returns the buffer name for the next nerd tree
function! s:Creator._nextBufferName()
    let name = s:Creator.BufNamePrefix() . self._nextBufferNumber()
    return name
endfunction

" FUNCTION: s:Creator._nextBufferNumber() {{{2
" the number to add to the nerd tree buffer name to make the buf name unique
function! s:Creator._nextBufferNumber()
    if !exists("s:Creator._NextBufNum")
        let s:Creator._NextBufNum = 1
    else
        let s:Creator._NextBufNum += 1
    endif

    return s:Creator._NextBufNum
endfunction

"FUNCTION: s:Creator._pathForString(str) {{{1
"find a bookmark or adirectory for the given string
function! s:Creator._pathForString(str)
    let path = {}
    if g:NERDTreeBookmark.BookmarkExistsFor(a:str)
        let path = g:NERDTreeBookmark.BookmarkFor(a:str).path
    else
        let dir = a:str ==# '' ? getcwd() : a:str

        "hack to get an absolute path if a relative path is given
        if dir =~# '^\.'
            let dir = getcwd() . g:NERDTreePath.Slash() . dir
        endif
        let dir = g:NERDTreePath.Resolve(dir)

        try
            let path = g:NERDTreePath.New(dir)
        catch /^NERDTree.InvalidArgumentsError/
            call nerdtree#echo("No bookmark or directory found for: " . a:str)
            return
        endtry
    endif
    if !path.isDirectory
        let path = path.getParent()
    endif

    return path
endfunction

"FUNCTION: s:Creator._setCommonBufOptions() {{{1
function! s:Creator._setCommonBufOptions()
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

    call self._setupStatusline()

    let b:treeShowHelp = 0
    let b:NERDTreeIgnoreEnabled = 1
    let b:NERDTreeShowFiles = g:NERDTreeShowFiles
    let b:NERDTreeShowHidden = g:NERDTreeShowHidden
    let b:NERDTreeShowBookmarks = g:NERDTreeShowBookmarks
    setfiletype nerdtree
    call self._bindMappings()
endfunction

"FUNCTION: s:Creator._setupStatusline() {{{1
function! s:Creator._setupStatusline()
    if g:NERDTreeStatusline != -1
        let &l:statusline = g:NERDTreeStatusline
    endif
endfunction

"FUNCTION: s:Creator.TogglePrimary(dir) {{{1
function! s:Creator.TogglePrimary(dir)
    let creator = s:Creator.New()
    call creator.togglePrimary(a:dir)
endfunction

"FUNCTION: s:Creator.togglePrimary(dir) {{{1
"Toggles the NERD tree. I.e the NERD tree is open, it is closed, if it is
"closed it is restored or initialized (if it doesnt exist)
"
"Args:
"dir: the full path for the root node (is only used if the NERD tree is being
"initialized.
function! s:Creator.togglePrimary(dir)
    if nerdtree#treeExistsForTab()
        if !nerdtree#isTreeOpen()
            call self._createTreeWin()
            if !&hidden
                call nerdtree#renderView()
            endif
            call nerdtree#restoreScreenState()
        else
            call nerdtree#closeTree()
        endif
    else
        call self.createPrimary(a:dir)
    endif
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
