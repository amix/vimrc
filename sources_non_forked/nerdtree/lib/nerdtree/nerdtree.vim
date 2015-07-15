"CLASS: NERDTree
"============================================================
let s:NERDTree = {}
let g:NERDTree = s:NERDTree

"FUNCTION: s:NERDTree.AddPathFilter() {{{1
function! s:NERDTree.AddPathFilter(callback)
    call add(s:NERDTree.PathFilters(), a:callback)
endfunction

"FUNCTION: s:NERDTree.Close() {{{1
"Closes the primary NERD tree window for this tab
function! s:NERDTree.Close()
    if !s:NERDTree.IsOpen()
        return
    endif

    if winnr("$") != 1
        if winnr() == s:NERDTree.GetWinNum()
            call nerdtree#exec("wincmd p")
            let bufnr = bufnr("")
            call nerdtree#exec("wincmd p")
        else
            let bufnr = bufnr("")
        endif

        call nerdtree#exec(s:NERDTree.GetWinNum() . " wincmd w")
        close
        call nerdtree#exec(bufwinnr(bufnr) . " wincmd w")
    else
        close
    endif
endfunction

"FUNCTION: s:NERDTree.CloseIfQuitOnOpen() {{{1
"Closes the NERD tree window if the close on open option is set
function! s:NERDTree.CloseIfQuitOnOpen()
    if g:NERDTreeQuitOnOpen && s:NERDTree.IsOpen()
        call s:NERDTree.Close()
    endif
endfunction

"FUNCTION: s:NERDTree.CursorToBookmarkTable(){{{1
"Places the cursor at the top of the bookmarks table
function! s:NERDTree.CursorToBookmarkTable()
    if !b:NERDTreeShowBookmarks
        throw "NERDTree.IllegalOperationError: cant find bookmark table, bookmarks arent active"
    endif

    if g:NERDTreeMinimalUI
        return cursor(1, 2)
    endif

    let rootNodeLine = b:NERDTree.ui.getRootLineNum()

    let line = 1
    while getline(line) !~# '^>-\+Bookmarks-\+$'
        let line = line + 1
        if line >= rootNodeLine
            throw "NERDTree.BookmarkTableNotFoundError: didnt find the bookmarks table"
        endif
    endwhile
    call cursor(line, 2)
endfunction

"FUNCTION: s:NERDTree.CursorToTreeWin(){{{1
"Places the cursor in the nerd tree window
function! s:NERDTree.CursorToTreeWin()
    call g:NERDTree.MustBeOpen()
    call nerdtree#exec(g:NERDTree.GetWinNum() . "wincmd w")
endfunction

" Function: s:NERDTree.ExistsForBuffer()   {{{1
" Returns 1 if a nerd tree root exists in the current buffer
function! s:NERDTree.ExistsForBuf()
    return exists("b:NERDTreeRoot")
endfunction

" Function: s:NERDTree.ExistsForTab()   {{{1
" Returns 1 if a nerd tree root exists in the current tab
function! s:NERDTree.ExistsForTab()
    return exists("t:NERDTreeBufName")
endfunction

function! s:NERDTree.ForCurrentBuf()
    if s:NERDTree.ExistsForBuf()
        return b:NERDTree
    else
        return {}
    endif
endfunction

"FUNCTION: s:NERDTree.GetWinNum() {{{1
"gets the nerd tree window number for this tab
function! s:NERDTree.GetWinNum()
    if exists("t:NERDTreeBufName")
        return bufwinnr(t:NERDTreeBufName)
    else
        return -1
    endif
endfunction

"FUNCTION: s:NERDTree.IsOpen() {{{1
function! s:NERDTree.IsOpen()
    return s:NERDTree.GetWinNum() != -1
endfunction

"FUNCTION: s:NERDTree.MustBeOpen() {{{1
function! s:NERDTree.MustBeOpen()
    if !s:NERDTree.IsOpen()
        throw "NERDTree.TreeNotOpen"
    endif
endfunction

"FUNCTION: s:NERDTree.New() {{{1
function! s:NERDTree.New(path)
    let newObj = copy(self)
    let newObj.ui = g:NERDTreeUI.New(newObj)
    let newObj.root = g:NERDTreeDirNode.New(a:path)

    return newObj
endfunction

"FUNCTION: s:NERDTree.PathFilters() {{{1
function! s:NERDTree.PathFilters()
    if !exists('s:NERDTree._PathFilters')
        let s:NERDTree._PathFilters = []
    endif
    return s:NERDTree._PathFilters
endfunction


"FUNCTION: s:NERDTree.render() {{{1
"A convenience function - since this is called often
function! s:NERDTree.render()
    call self.ui.render()
endfunction
