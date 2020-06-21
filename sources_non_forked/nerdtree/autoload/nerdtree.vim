if exists('g:loaded_nerdtree_autoload')
    finish
endif
let g:loaded_nerdtree_autoload = 1

let s:rootNERDTreePath = resolve(expand('<sfile>:p:h:h'))

"FUNCTION: nerdtree#version(...) {{{1
"  If any value is given as an argument, the entire line of text from the
"  change log is shown for the current version; otherwise, only the version
"  number is shown.
function! nerdtree#version(...) abort
    let l:text = 'Unknown'
    try
        let l:changelog = readfile(join([s:rootNERDTreePath, 'CHANGELOG.md'], nerdtree#slash()))
        let l:line = 0
        while l:line <= len(l:changelog)
            if l:changelog[l:line] =~# '\d\+\.\d\+'
                let l:text = substitute(l:changelog[l:line], '.*\(\d\+.\d\+\).*', '\1', '')
                let l:text .= substitute(l:changelog[l:line+1], '^.\{-}\(\.\d\+\).\{-}:\(.*\)', a:0>0 ? '\1:\2' : '\1', '')
                break
            endif
            let l:line += 1
        endwhile
    catch
    endtry
    return l:text
endfunction

" SECTION: General Functions {{{1
"============================================================

"FUNCTION: nerdtree#slash() {{{2
function! nerdtree#slash() abort

    if nerdtree#runningWindows()
        if exists('+shellslash') && &shellslash
            return '/'
        endif

        return '\'
    endif

    return '/'
endfunction

"FUNCTION: nerdtree#and(x,y) {{{2
" Implements and() function for Vim <= 7.4
function! nerdtree#and(x,y) abort
    if exists('*and')
        return and(a:x, a:y)
    else
        let l:x = a:x
        let l:y = a:y
        let l:n = 0
        let l:result = 0
        while l:x > 0 && l:y > 0
            if (l:x % 2) && (l:y % 2)
                let l:result += float2nr(pow(2, l:n))
            endif
            let l:x = float2nr(l:x / 2)
            let l:y = float2nr(l:y / 2)
            let l:n += 1
        endwhile
        return l:result
    endif
endfunction

"FUNCTION: nerdtree#checkForBrowse(dir) {{{2
"inits a window tree in the current buffer if appropriate
function! nerdtree#checkForBrowse(dir) abort
    if !isdirectory(a:dir)
        return
    endif

    if s:reuseWin(a:dir)
        return
    endif

    call g:NERDTreeCreator.CreateWindowTree(a:dir)
endfunction

"FUNCTION: s:reuseWin(dir) {{{2
"finds a NERDTree buffer with root of dir, and opens it.
function! s:reuseWin(dir) abort
    let path = g:NERDTreePath.New(fnamemodify(a:dir, ':p'))

    for i in range(1, bufnr('$'))
        unlet! nt
        let nt = getbufvar(i, 'NERDTree')
        if empty(nt)
            continue
        endif

        if nt.isWinTree() && nt.root.path.equals(path)
            call nt.setPreviousBuf(bufnr('#'))
            exec 'buffer ' . i
            return 1
        endif
    endfor

    return 0
endfunction

" FUNCTION: nerdtree#completeBookmarks(A,L,P) {{{2
" completion function for the bookmark commands
function! nerdtree#completeBookmarks(A,L,P) abort
    return filter(g:NERDTreeBookmark.BookmarkNames(), 'v:val =~# "^' . a:A . '"')
endfunction

"FUNCTION: nerdtree#compareNodes(dir) {{{2
function! nerdtree#compareNodes(n1, n2) abort
    return a:n1.path.compareTo(a:n2.path)
endfunction

"FUNCTION: nerdtree#compareNodesBySortKey(n1, n2) {{{2
function! nerdtree#compareNodesBySortKey(n1, n2) abort
    let sortKey1 = a:n1.path.getSortKey()
    let sortKey2 = a:n2.path.getSortKey()
    let i = 0
    while i < min([len(sortKey1), len(sortKey2)])
        " Compare chunks upto common length.
        " If chunks have different type, the one which has
        " integer type is the lesser.
        if type(sortKey1[i]) ==# type(sortKey2[i])
            if sortKey1[i] <# sortKey2[i]
                return - 1
            elseif sortKey1[i] ># sortKey2[i]
                return 1
            endif
        elseif type(sortKey1[i]) ==# v:t_number
            return -1
        elseif type(sortKey2[i]) ==# v:t_number
            return 1
        endif
        let i = i + 1
    endwhile

    " Keys are identical upto common length.
    " The key which has smaller chunks is the lesser one.
    if len(sortKey1) < len(sortKey2)
        return -1
    elseif len(sortKey1) > len(sortKey2)
        return 1
    else
        return 0
    endif
endfunction

" FUNCTION: nerdtree#deprecated(func, [msg]) {{{2
" Issue a deprecation warning for a:func. If a second arg is given, use this
" as the deprecation message
function! nerdtree#deprecated(func, ...) abort
    let msg = a:0 ? a:func . ' ' . a:1 : a:func . ' is deprecated'

    if !exists('s:deprecationWarnings')
        let s:deprecationWarnings = {}
    endif
    if !has_key(s:deprecationWarnings, a:func)
        let s:deprecationWarnings[a:func] = 1
        echomsg msg
    endif
endfunction

" FUNCTION: nerdtree#exec(cmd, ignoreAll) {{{2
" Same as :exec cmd but, if ignoreAll is TRUE, set eventignore=all for the duration
function! nerdtree#exec(cmd, ignoreAll) abort
    let old_ei = &eventignore
    if a:ignoreAll
        set eventignore=all
    endif
    try
        exec a:cmd
    finally
        let &eventignore = old_ei
    endtry
endfunction

" FUNCTION: nerdtree#has_opt(options, name) {{{2
function! nerdtree#has_opt(options, name) abort
    return has_key(a:options, a:name) && a:options[a:name] ==# 1
endfunction

" FUNCTION: nerdtree#loadClassFiles() {{{2
function! nerdtree#loadClassFiles() abort
    runtime lib/nerdtree/path.vim
    runtime lib/nerdtree/menu_controller.vim
    runtime lib/nerdtree/menu_item.vim
    runtime lib/nerdtree/key_map.vim
    runtime lib/nerdtree/bookmark.vim
    runtime lib/nerdtree/tree_file_node.vim
    runtime lib/nerdtree/tree_dir_node.vim
    runtime lib/nerdtree/opener.vim
    runtime lib/nerdtree/creator.vim
    runtime lib/nerdtree/flag_set.vim
    runtime lib/nerdtree/nerdtree.vim
    runtime lib/nerdtree/ui.vim
    runtime lib/nerdtree/event.vim
    runtime lib/nerdtree/notifier.vim
endfunction

" FUNCTION: nerdtree#postSourceActions() {{{2
function! nerdtree#postSourceActions() abort
    call g:NERDTreeBookmark.CacheBookmarks(1)
    call nerdtree#ui_glue#createDefaultBindings()

    "load all nerdtree plugins
    runtime! nerdtree_plugin/**/*.vim
endfunction

"FUNCTION: nerdtree#runningWindows(dir) {{{2
function! nerdtree#runningWindows() abort
    return has('win16') || has('win32') || has('win64')
endfunction

"FUNCTION: nerdtree#runningCygwin(dir) {{{2
function! nerdtree#runningCygwin() abort
    return has('win32unix')
endfunction

" SECTION: View Functions {{{1
"============================================================

"FUNCTION: nerdtree#echo  {{{2
"A wrapper for :echo. Appends 'NERDTree:' on the front of all messages
"
"Args:
"msg: the message to echo
function! nerdtree#echo(msg) abort
    redraw
    echomsg empty(a:msg) ? '' : ('NERDTree: ' . a:msg)
endfunction

"FUNCTION: nerdtree#echoError {{{2
"Wrapper for nerdtree#echo, sets the message type to errormsg for this message
"Args:
"msg: the message to echo
function! nerdtree#echoError(msg) abort
    echohl errormsg
    call nerdtree#echo(a:msg)
    echohl normal
endfunction

"FUNCTION: nerdtree#echoWarning {{{2
"Wrapper for nerdtree#echo, sets the message type to warningmsg for this message
"Args:
"msg: the message to echo
function! nerdtree#echoWarning(msg) abort
    echohl warningmsg
    call nerdtree#echo(a:msg)
    echohl normal
endfunction

"FUNCTION: nerdtree#renderView {{{2
function! nerdtree#renderView() abort
    call b:NERDTree.render()
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
