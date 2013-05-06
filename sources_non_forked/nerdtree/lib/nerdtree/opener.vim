"CLASS: Opener
"============================================================
let s:Opener = {}
let g:NERDTreeOpener = s:Opener

"FUNCTION: Opener._checkToCloseTree(newtab) {{{1
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
        call nerdtree#closeTreeIfQuitOnOpen()
    endif
endfunction

"FUNCTION: Opener._gotoTargetWin() {{{1
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

"FUNCTION: Opener.New(path, opts) {{{1
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
    let newObj._stay = nerdtree#has_opt(a:opts, 'stay')
    let newObj._reuse = nerdtree#has_opt(a:opts, 'reuse')
    let newObj._keepopen = nerdtree#has_opt(a:opts, 'keepopen')
    let newObj._where = has_key(a:opts, 'where') ? a:opts['where'] : ''
    let newObj._treetype = b:NERDTreeType
    call newObj._saveCursorPos()

    return newObj
endfunction

"FUNCTION: Opener._newSplit() {{{1
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
    call nerdtree#exec(back)

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
        call nerdtree#putCursorInTreeWin()
        throw "NERDTree.FileAlreadyOpenAndModifiedError: ". self._path.str() ." is already open and modified."
    catch /^Vim\%((\a\+)\)\=:/
        "do nothing
    endtry

    "resize the tree window if no other window was open before
    if onlyOneWin
        let size = exists("b:NERDTreeOldWindowSize") ? b:NERDTreeOldWindowSize : g:NERDTreeWinSize
        call nerdtree#exec(there)
        exec("silent ". splitMode ." resize ". size)
        call nerdtree#exec('wincmd p')
    endif

    " Restore splitmode settings
    let &splitbelow=savesplitbelow
    let &splitright=savesplitright
endfunction

"FUNCTION: Opener._newVSplit() {{{1
function! s:Opener._newVSplit()
    let winwidth = winwidth(".")
    if winnr("$")==#1
        let winwidth = g:NERDTreeWinSize
    endif

    call nerdtree#exec("wincmd p")
    vnew

    "resize the nerd tree back to the original size
    call nerdtree#putCursorInTreeWin()
    exec("silent vertical resize ". winwidth)
    call nerdtree#exec('wincmd p')
endfunction

"FUNCTION: Opener.open(target) {{{1
function! s:Opener.open(target)
    if self._path.isDirectory
        call self._openDirectory(a:target)
    else
        call self._openFile()
    endif
endfunction

"FUNCTION: Opener._openFile() {{{1
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

"FUNCTION: Opener._openDirectory(node) {{{1
function! s:Opener._openDirectory(node)
    if self._treetype ==# "secondary"
        call self._gotoTargetWin()
        call g:NERDTreeCreator.CreateSecondary(a:node.path.str())
    else
        call self._gotoTargetWin()
        if empty(self._where)
            call a:node.makeRoot()
            call nerdtree#renderView()
            call a:node.putCursorHere(0, 0)
        elseif self._where == 't'
            call g:NERDTreeCreator.CreatePrimary(a:node.path.str())
        else
            call g:NERDTreeCreator.CreateSecondary(a:node.path.str())
        endif
    endif

    if self._stay
        call self._restoreCursorPos()
    endif
endfunction

"FUNCTION: Opener._previousWindow() {{{1
function! s:Opener._previousWindow()
    if !nerdtree#isWindowUsable(winnr("#")) && nerdtree#firstUsableWindow() ==# -1
        call self._newSplit()
    else
        try
            if !nerdtree#isWindowUsable(winnr("#"))
                call nerdtree#exec(nerdtree#firstUsableWindow() . "wincmd w")
            else
                call nerdtree#exec('wincmd p')
            endif
        catch /^Vim\%((\a\+)\)\=:E37/
            call nerdtree#putCursorInTreeWin()
            throw "NERDTree.FileAlreadyOpenAndModifiedError: ". self._path.str() ." is already open and modified."
        catch /^Vim\%((\a\+)\)\=:/
            echo v:exception
        endtry
    endif
endfunction

"FUNCTION: Opener._restoreCursorPos(){{{1
function! s:Opener._restoreCursorPos()
    call nerdtree#exec('normal ' . self._tabnr . 'gt')
    call nerdtree#exec(bufwinnr(self._bufnr) . 'wincmd w')
endfunction

"FUNCTION: Opener._reuseWindow(){{{1
"put the cursor in the first window we find for this file
"
"return 1 if we were successful
function! s:Opener._reuseWindow()
    "check the current tab for the window
    let winnr = bufwinnr('^' . self._path.str() . '$')
    if winnr != -1
        call nerdtree#exec(winnr . "wincmd w")
        call self._checkToCloseTree(0)
        return 1
    else
        "check other tabs
        let tabnr = self._path.tabnr()
        if tabnr
            call self._checkToCloseTree(1)
            call nerdtree#exec('normal! ' . tabnr . 'gt')
            let winnr = bufwinnr('^' . self._path.str() . '$')
            call nerdtree#exec(winnr . "wincmd w")
            return 1
        endif
    endif
    return 0
endfunction

"FUNCTION: Opener._saveCursorPos(){{{1
function! s:Opener._saveCursorPos()
    let self._bufnr = bufnr("")
    let self._tabnr = tabpagenr()
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
