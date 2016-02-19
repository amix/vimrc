" ============================================================================
" File:        fs_menu.vim
" Description: plugin for the NERD Tree that provides a file system menu
" Maintainer:  Martin Grenfell <martin.grenfell at gmail dot com>
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================
if exists("g:loaded_nerdtree_fs_menu")
    finish
endif
let g:loaded_nerdtree_fs_menu = 1

"Automatically delete the buffer after deleting or renaming a file
if !exists("g:NERDTreeAutoDeleteBuffer")
    let g:NERDTreeAutoDeleteBuffer = 0
endif

call NERDTreeAddMenuItem({'text': '(a)dd a childnode', 'shortcut': 'a', 'callback': 'NERDTreeAddNode'})
call NERDTreeAddMenuItem({'text': '(m)ove the current node', 'shortcut': 'm', 'callback': 'NERDTreeMoveNode'})
call NERDTreeAddMenuItem({'text': '(d)elete the current node', 'shortcut': 'd', 'callback': 'NERDTreeDeleteNode'})

if has("gui_mac") || has("gui_macvim") || has("mac")
    call NERDTreeAddMenuItem({'text': '(r)eveal in Finder the current node', 'shortcut': 'r', 'callback': 'NERDTreeRevealInFinder'})
    call NERDTreeAddMenuItem({'text': '(o)pen the current node with system editor', 'shortcut': 'o', 'callback': 'NERDTreeExecuteFile'})
    call NERDTreeAddMenuItem({'text': '(q)uicklook the current node', 'shortcut': 'q', 'callback': 'NERDTreeQuickLook'})
endif

if g:NERDTreePath.CopyingSupported()
    call NERDTreeAddMenuItem({'text': '(c)opy the current node', 'shortcut': 'c', 'callback': 'NERDTreeCopyNode'})
endif

if has("unix") || has("osx")
    call NERDTreeAddMenuItem({'text': '(l)ist the current node', 'shortcut': 'l', 'callback': 'NERDTreeListNode'})
else
    call NERDTreeAddMenuItem({'text': '(l)ist the current node', 'shortcut': 'l', 'callback': 'NERDTreeListNodeWin32'})
endif

"FUNCTION: s:promptToDelBuffer(bufnum, msg){{{1
"prints out the given msg and, if the user responds by pushing 'y' then the
"buffer with the given bufnum is deleted
"
"Args:
"bufnum: the buffer that may be deleted
"msg: a message that will be echoed to the user asking them if they wish to
"     del the buffer
function! s:promptToDelBuffer(bufnum, msg)
    echo a:msg
    if g:NERDTreeAutoDeleteBuffer || nr2char(getchar()) ==# 'y'
        " 1. ensure that all windows which display the just deleted filename
        " now display an empty buffer (so a layout is preserved).
        " Is not it better to close single tabs with this file only ?
        let s:originalTabNumber = tabpagenr()
        let s:originalWindowNumber = winnr()
        exec "tabdo windo if winbufnr(0) == " . a:bufnum . " | exec ':enew! ' | endif"
        exec "tabnext " . s:originalTabNumber
        exec s:originalWindowNumber . "wincmd w"
        " 3. We don't need a previous buffer anymore
        exec "bwipeout! " . a:bufnum
    endif
endfunction

"FUNCTION: s:promptToRenameBuffer(bufnum, msg){{{1
"prints out the given msg and, if the user responds by pushing 'y' then the
"buffer with the given bufnum is replaced with a new one
"
"Args:
"bufnum: the buffer that may be deleted
"msg: a message that will be echoed to the user asking them if they wish to
"     del the buffer
function! s:promptToRenameBuffer(bufnum, msg, newFileName)
    echo a:msg
    if g:NERDTreeAutoDeleteBuffer || nr2char(getchar()) ==# 'y'
        let quotedFileName = "'" . a:newFileName . "'"
        " 1. ensure that a new buffer is loaded
        exec "badd " . quotedFileName
        " 2. ensure that all windows which display the just deleted filename
        " display a buffer for a new filename.
        let s:originalTabNumber = tabpagenr()
        let s:originalWindowNumber = winnr()
        let editStr = g:NERDTreePath.New(a:newFileName).str({'format': 'Edit'})
        exec "tabdo windo if winbufnr(0) == " . a:bufnum . " | exec ':e! " . editStr . "' | endif"
        exec "tabnext " . s:originalTabNumber
        exec s:originalWindowNumber . "wincmd w"
        " 3. We don't need a previous buffer anymore
        exec "bwipeout! " . a:bufnum
    endif
endfunction
"FUNCTION: NERDTreeAddNode(){{{1
function! NERDTreeAddNode()
    let curDirNode = g:NERDTreeDirNode.GetSelected()

    let newNodeName = input("Add a childnode\n".
                          \ "==========================================================\n".
                          \ "Enter the dir/file name to be created. Dirs end with a '/'\n" .
                          \ "", curDirNode.path.str() . g:NERDTreePath.Slash(), "file")

    if newNodeName ==# ''
        call nerdtree#echo("Node Creation Aborted.")
        return
    endif

    try
        let newPath = g:NERDTreePath.Create(newNodeName)
        let parentNode = b:NERDTree.root.findNode(newPath.getParent())

        let newTreeNode = g:NERDTreeFileNode.New(newPath, b:NERDTree)
        if empty(parentNode)
            call b:NERDTree.root.refresh()
            call b:NERDTree.render()
        elseif parentNode.isOpen || !empty(parentNode.children)
            call parentNode.addChild(newTreeNode, 1)
            call NERDTreeRender()
            call newTreeNode.putCursorHere(1, 0)
        endif
    catch /^NERDTree/
        call nerdtree#echoWarning("Node Not Created.")
    endtry
endfunction

"FUNCTION: NERDTreeMoveNode(){{{1
function! NERDTreeMoveNode()
    let curNode = g:NERDTreeFileNode.GetSelected()
    let newNodePath = input("Rename the current node\n" .
                          \ "==========================================================\n" .
                          \ "Enter the new path for the node:                          \n" .
                          \ "", curNode.path.str(), "file")

    if newNodePath ==# ''
        call nerdtree#echo("Node Renaming Aborted.")
        return
    endif

    try
        let bufnum = bufnr("^".curNode.path.str()."$")

        call curNode.rename(newNodePath)
        call NERDTreeRender()

        "if the node is open in a buffer, ask the user if they want to
        "close that buffer
        if bufnum != -1
            let prompt = "\nNode renamed.\n\nThe old file is open in buffer ". bufnum . (bufwinnr(bufnum) ==# -1 ? " (hidden)" : "") .". Replace this buffer with a new file? (yN)"
            call s:promptToRenameBuffer(bufnum,  prompt, newNodePath)
        endif

        call curNode.putCursorHere(1, 0)

        redraw
    catch /^NERDTree/
        call nerdtree#echoWarning("Node Not Renamed.")
    endtry
endfunction

" FUNCTION: NERDTreeDeleteNode() {{{1
function! NERDTreeDeleteNode()
    let currentNode = g:NERDTreeFileNode.GetSelected()
    let confirmed = 0

    if currentNode.path.isDirectory
        let choice =input("Delete the current node\n" .
                         \ "==========================================================\n" .
                         \ "STOP! To delete this entire directory, type 'yes'\n" .
                         \ "" . currentNode.path.str() . ": ")
        let confirmed = choice ==# 'yes'
    else
        echo "Delete the current node\n" .
           \ "==========================================================\n".
           \ "Are you sure you wish to delete the node:\n" .
           \ "" . currentNode.path.str() . " (yN):"
        let choice = nr2char(getchar())
        let confirmed = choice ==# 'y'
    endif


    if confirmed
        try
            call currentNode.delete()
            call NERDTreeRender()

            "if the node is open in a buffer, ask the user if they want to
            "close that buffer
            let bufnum = bufnr("^".currentNode.path.str()."$")
            if buflisted(bufnum)
                let prompt = "\nNode deleted.\n\nThe file is open in buffer ". bufnum . (bufwinnr(bufnum) ==# -1 ? " (hidden)" : "") .". Delete this buffer? (yN)"
                call s:promptToDelBuffer(bufnum, prompt)
            endif

            redraw
        catch /^NERDTree/
            call nerdtree#echoWarning("Could not remove node")
        endtry
    else
        call nerdtree#echo("delete aborted")
    endif

endfunction

" FUNCTION: NERDTreeListNode() {{{1
function! NERDTreeListNode()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if treenode != {}
        let metadata = split(system('ls -ld ' . shellescape(treenode.path.str())), '\n')
        call nerdtree#echo(metadata[0])
    else
        call nerdtree#echo("No information avaialable")
    endif
endfunction

" FUNCTION: NERDTreeListNodeWin32() {{{1
function! NERDTreeListNodeWin32()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if treenode != {}
        let metadata = split(system('DIR /Q ' . shellescape(treenode.path.str()) . ' | FINDSTR "^[012][0-9]/[0-3][0-9]/[12][0-9][0-9][0-9]"'), '\n')
        call nerdtree#echo(metadata[0])
    else
        call nerdtree#echo("No information avaialable")
    endif

endfunction

" FUNCTION: NERDTreeCopyNode() {{{1
function! NERDTreeCopyNode()
    let currentNode = g:NERDTreeFileNode.GetSelected()
    let newNodePath = input("Copy the current node\n" .
                          \ "==========================================================\n" .
                          \ "Enter the new path to copy the node to:                   \n" .
                          \ "", currentNode.path.str(), "file")

    if newNodePath != ""
        "strip trailing slash
        let newNodePath = substitute(newNodePath, '\/$', '', '')

        let confirmed = 1
        if currentNode.path.copyingWillOverwrite(newNodePath)
            call nerdtree#echo("Warning: copying may overwrite files! Continue? (yN)")
            let choice = nr2char(getchar())
            let confirmed = choice ==# 'y'
        endif

        if confirmed
            try
                let newNode = currentNode.copy(newNodePath)
                if empty(newNode)
                    call b:NERDTree.root.refresh()
                    call b:NERDTree.render()
                else
                    call NERDTreeRender()
                    call newNode.putCursorHere(0, 0)
                endif
            catch /^NERDTree/
                call nerdtree#echoWarning("Could not copy node")
            endtry
        endif
    else
        call nerdtree#echo("Copy aborted.")
    endif
    redraw
endfunction

" FUNCTION: NERDTreeQuickLook() {{{1
function! NERDTreeQuickLook()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if treenode != {}
        call system("qlmanage -p 2>/dev/null '" . treenode.path.str() . "'")
    endif
endfunction

" FUNCTION: NERDTreeRevealInFinder() {{{1
function! NERDTreeRevealInFinder()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if treenode != {}
        call system("open -R '" . treenode.path.str() . "'")
    endif
endfunction

" FUNCTION: NERDTreeExecuteFile() {{{1
function! NERDTreeExecuteFile()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if treenode != {}
        call system("open '" . treenode.path.str() . "'")
    endif
endfunction
" vim: set sw=4 sts=4 et fdm=marker:
