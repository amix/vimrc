" ============================================================================
" CLASS: Opener
"
" The Opener class defines an API for 'opening' operations.
" ============================================================================


let s:Opener = {}
let g:NERDTreeOpener = s:Opener

" FUNCTION: s:Opener._bufInWindows(bnum) {{{1
" [[STOLEN FROM VTREEEXPLORER.VIM]]
" Determine the number of windows open to this buffer number.
" Care of Yegappan Lakshman.  Thanks!
"
" Args:
" bnum: the subject buffers buffer number
function! s:Opener._bufInWindows(bnum)
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
endfunction

" FUNCTION: Opener._checkToCloseTree(newtab) {{{1
" Check the class options and global options (i.e. NERDTreeQuitOnOpen) to see
" if the tree should be closed now.
"
" Args:
" a:newtab - boolean. If set, only close the tree now if we are opening the
" target in a new tab. This is needed because we have to close tree before we
" leave the tab
function! s:Opener._checkToCloseTree(newtab)
    if self._keepopen
        return
    endif

    if (a:newtab && self._where ==# 't') || !a:newtab
        call g:NERDTree.CloseIfQuitOnOpen()
    endif
endfunction

" FUNCTION: s:Opener._firstUsableWindow() {{{1
" find the window number of the first normal window
function! s:Opener._firstUsableWindow()
    let i = 1
    while i <= winnr('$')
        let bnum = winbufnr(i)
        if bnum !=# -1 && getbufvar(bnum, '&buftype') ==# ''
                    \ && !getwinvar(i, '&previewwindow')
                    \ && (!getbufvar(bnum, '&modified') || &hidden)
            return i
        endif

        let i += 1
    endwhile
    return -1
endfunction

" FUNCTION: Opener._gotoTargetWin() {{{1
function! s:Opener._gotoTargetWin()
    if b:NERDTree.isWinTree()
        if self._where ==# 'v'
            call self._newVSplit()
        elseif self._where ==# 'h'
            call self._newSplit()
        elseif self._where ==# 't'
            tabnew
        endif
    else
        call self._checkToCloseTree(1)

        if self._where ==# 'v'
            call self._newVSplit()
        elseif self._where ==# 'h'
            call self._newSplit()
        elseif self._where ==# 't'
            tabnew
        elseif self._where ==# 'p'
            call self._previousWindow()
        endif

        call self._checkToCloseTree(0)
    endif
endfunction

" FUNCTION: s:Opener._isWindowUsable(winnumber) {{{1
" Returns 0 if opening a file from the tree in the given window requires it to
" be split, 1 otherwise
"
" Args:
" winnumber: the number of the window in question
function! s:Opener._isWindowUsable(winnumber)
    "gotta split if theres only one window (i.e. the NERD tree)
    if winnr('$') ==# 1
        return 0
    endif

    let oldwinnr = winnr()
    call nerdtree#exec(a:winnumber . 'wincmd p', 1)
    let specialWindow = getbufvar('%', '&buftype') !=# '' || getwinvar('%', '&previewwindow')
    let modified = &modified
    call nerdtree#exec(oldwinnr . 'wincmd p', 1)

    "if its a special window e.g. quickfix or another explorer plugin then we
    "have to split
    if specialWindow
        return 0
    endif

    if &hidden
        return 1
    endif

    return !modified || self._bufInWindows(winbufnr(a:winnumber)) >= 2
endfunction

" FUNCTION: Opener.New(path, opts) {{{1
" Instantiate a new NERDTreeOpener object.
" Args:
" a:path: the path object that is to be opened
" a:opts: a dictionary containing the following optional keys...
"   'where': specifies whether the node should be opened in new split, in
"            a new tab or, in the last window; takes values 'v', 'h', or 't'
"   'reuse': if file is already shown in a window, jump there; takes values
"            'all', 'currenttab', or empty
"   'keepopen': boolean (0 or 1); if true, the tree window will not be closed
"   'stay': boolean (0 or 1); if true, remain in tree window after opening
function! s:Opener.New(path, opts)
    let l:newOpener = copy(self)

    let l:newOpener._keepopen = nerdtree#has_opt(a:opts, 'keepopen')
    let l:newOpener._nerdtree = b:NERDTree
    let l:newOpener._path = a:path
    let l:newOpener._reuse = has_key(a:opts, 'reuse') ? a:opts['reuse'] : ''
    let l:newOpener._stay = nerdtree#has_opt(a:opts, 'stay')
    let l:newOpener._where = has_key(a:opts, 'where') ? a:opts['where'] : ''

    call l:newOpener._saveCursorPos()

    return l:newOpener
endfunction

" FUNCTION: Opener._newSplit() {{{1
function! s:Opener._newSplit()
    let onlyOneWin = (winnr('$') ==# 1)
    let savesplitright = &splitright
    if onlyOneWin
        let &splitright = (g:NERDTreeWinPos ==# 'left')
    endif
    " If only one window (ie. NERDTree), split vertically instead.
    let splitMode = onlyOneWin ? 'vertical' : ''

    " Open the new window
    try
        call nerdtree#exec('wincmd p', 1)
        call nerdtree#exec(splitMode . ' split',1)
    catch /^Vim\%((\a\+)\)\=:E37/
        call g:NERDTree.CursorToTreeWin()
        throw 'NERDTree.FileAlreadyOpenAndModifiedError: '. self._path.str() .' is already open and modified.'
    catch /^Vim\%((\a\+)\)\=:/
        "do nothing
    endtry

    "resize the tree window if no other window was open before
    if onlyOneWin
        call nerdtree#exec('wincmd p', 1)
        call nerdtree#exec('silent '. splitMode .' resize '. g:NERDTreeWinSize, 1)
        call nerdtree#exec('wincmd p', 0)
    endif

    let &splitright=savesplitright
endfunction

" FUNCTION: Opener._newVSplit() {{{1
function! s:Opener._newVSplit()
    let l:winwidth = winwidth('.')

    let onlyOneWin = (winnr('$') ==# 1)
    let savesplitright = &splitright
    if onlyOneWin
        let &splitright = (g:NERDTreeWinPos ==# 'left')
        let l:winwidth = g:NERDTreeWinSize
    endif

    call nerdtree#exec('wincmd p', 1)
    call nerdtree#exec('vsplit', 1)

    let l:currentWindowNumber = winnr()

    " Restore the NERDTree to its original width.
    call g:NERDTree.CursorToTreeWin()
    execute 'silent vertical resize ' . l:winwidth

    call nerdtree#exec(l:currentWindowNumber . 'wincmd w', 0)
    let &splitright=savesplitright
endfunction

" FUNCTION: Opener.open(target) {{{1
function! s:Opener.open(target)
    if self._path.isDirectory
        call self._openDirectory(a:target)
        return
    endif

    call self._openFile()
endfunction

" FUNCTION: Opener._openFile() {{{1
function! s:Opener._openFile()
    if !self._stay && !nerdtree#and(g:NERDTreeQuitOnOpen,1) && exists('b:NERDTreeZoomed') && b:NERDTreeZoomed
        call b:NERDTree.ui.toggleZoom()
    endif

    if self._reuseWindow()
        return
    endif

    call self._gotoTargetWin()

    if self._stay
        silent call self._path.edit()
        call self._restoreCursorPos()
        return
    endif

    call self._path.edit()
endfunction

" FUNCTION: Opener._openDirectory(node) {{{1
function! s:Opener._openDirectory(node)
    call self._gotoTargetWin()

    if self._nerdtree.isWinTree()
        call g:NERDTreeCreator.CreateWindowTree(a:node.path.str())
    else
        if empty(self._where)
            call b:NERDTree.changeRoot(a:node)
        elseif self._where ==# 't'
            call g:NERDTreeCreator.CreateTabTree(a:node.path.str())
        else
            call g:NERDTreeCreator.CreateWindowTree(a:node.path.str())
        endif
    endif

    if self._stay
        call self._restoreCursorPos()
    endif
endfunction

" FUNCTION: Opener._previousWindow() {{{1
function! s:Opener._previousWindow()
    if !self._isWindowUsable(winnr('#')) && self._firstUsableWindow() ==# -1
        call self._newSplit()
    else
        try
            if !self._isWindowUsable(winnr('#'))
                call nerdtree#exec(self._firstUsableWindow() . 'wincmd w', 1)
            else
                call nerdtree#exec('wincmd p', 1)
            endif
        catch /^Vim\%((\a\+)\)\=:E37/
            call g:NERDTree.CursorToTreeWin()
            throw 'NERDTree.FileAlreadyOpenAndModifiedError: '. self._path.str() .' is already open and modified.'
        catch /^Vim\%((\a\+)\)\=:/
            echo v:exception
        endtry
    endif
endfunction

" FUNCTION: Opener._restoreCursorPos() {{{1
function! s:Opener._restoreCursorPos()
    call nerdtree#exec(self._tabnr . 'tabnext', 1)
    call nerdtree#exec(bufwinnr(self._bufnr) . 'wincmd w', 1)
endfunction

" FUNCTION: Opener._reuseWindow() {{{1
" put the cursor in the first window we find for this file
"
" return 1 if we were successful
function! s:Opener._reuseWindow()
    if empty(self._reuse)
        return 0
    endif

    "check the current tab for the window
    let winnr = bufwinnr('^' . self._path.str() . '$')
    if winnr !=# -1
        call nerdtree#exec(winnr . 'wincmd w', 0)
        call self._checkToCloseTree(0)
        return 1
    endif

    if self._reuse ==# 'currenttab'
        return 0
    endif

    "check other tabs
    let tabnr = self._path.tabnr()
    if tabnr
        call self._checkToCloseTree(1)
        call nerdtree#exec(tabnr . 'tabnext', 1)
        let winnr = bufwinnr('^' . self._path.str() . '$')
        call nerdtree#exec(winnr . 'wincmd w', 0)
        return 1
    endif

    return 0
endfunction

" FUNCTION: Opener._saveCursorPos() {{{1
function! s:Opener._saveCursorPos()
    let self._bufnr = bufnr('')
    let self._tabnr = tabpagenr()
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
