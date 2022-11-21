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
if exists('g:loaded_nerdtree_fs_menu')
    finish
endif
let g:loaded_nerdtree_fs_menu = 1

"Automatically delete the buffer after deleting or renaming a file
if !exists('g:NERDTreeAutoDeleteBuffer')
    let g:NERDTreeAutoDeleteBuffer = 0
endif

call NERDTreeAddMenuItem({'text': '(a)dd a childnode', 'shortcut': 'a', 'callback': 'NERDTreeAddNode'})
call NERDTreeAddMenuItem({'text': '(m)ove the current node', 'shortcut': 'm', 'callback': 'NERDTreeMoveNode'})
call NERDTreeAddMenuItem({'text': '(d)elete the current node', 'shortcut': 'd', 'callback': 'NERDTreeDeleteNode'})

if has('gui_mac') || has('gui_macvim') || has('mac')
    call NERDTreeAddMenuItem({'text': '(r)eveal in Finder the current node', 'shortcut': 'r', 'callback': 'NERDTreeRevealInFinder'})
    call NERDTreeAddMenuItem({'text': '(o)pen the current node with system editor', 'shortcut': 'o', 'callback': 'NERDTreeExecuteFile'})
    call NERDTreeAddMenuItem({'text': '(q)uicklook the current node', 'shortcut': 'q', 'callback': 'NERDTreeQuickLook'})
endif

if executable('xdg-open')
    call NERDTreeAddMenuItem({'text': '(r)eveal the current node in file manager', 'shortcut': 'r', 'callback': 'NERDTreeRevealFileLinux'})
    call NERDTreeAddMenuItem({'text': '(o)pen the current node with system editor', 'shortcut': 'o', 'callback': 'NERDTreeExecuteFileLinux'})
endif

if nerdtree#runningWindows()
    call NERDTreeAddMenuItem({'text': '(o)pen the current node with system editor', 'shortcut': 'o', 'callback': 'NERDTreeExecuteFileWindows'})
endif

if g:NERDTreePath.CopyingSupported()
    call NERDTreeAddMenuItem({'text': '(c)opy the current node', 'shortcut': 'c', 'callback': 'NERDTreeCopyNode'})
endif
call NERDTreeAddMenuItem({'text': (has('clipboard')?'copy (p)ath to clipboard':'print (p)ath to screen'), 'shortcut': 'p', 'callback': 'NERDTreeCopyPath'})

if has('unix') || has('osx')
    call NERDTreeAddMenuItem({'text': '(l)ist the current node', 'shortcut': 'l', 'callback': 'NERDTreeListNode'})
else
    call NERDTreeAddMenuItem({'text': '(l)ist the current node', 'shortcut': 'l', 'callback': 'NERDTreeListNodeWin32'})
endif

if exists('*system')
    call NERDTreeAddMenuItem({'text': 'Run (s)ystem command in this directory', 'shortcut':'s', 'callback': 'NERDTreeSystemCommand'})
endif

"FUNCTION: s:inputPrompt(action){{{1
"returns the string that should be prompted to the user for the given action
"
"Args:
"action: the action that is being performed, e.g. 'delete'
function! s:inputPrompt(action)
    if a:action ==# 'add'
        let title = 'Add a childnode'
        let info = "Enter the dir/file name to be created. Dirs end with a '/'"
        let minimal = 'Add node:'

    elseif a:action ==# 'copy'
        let title = 'Copy the current node'
        let info = 'Enter the new path to copy the node to:'
        let minimal = 'Copy to:'

    elseif a:action ==# 'delete'
        let title = 'Delete the current node'
        let info = 'Are you sure you wish to delete the node:'
        let minimal = 'Delete?'

    elseif a:action ==# 'deleteNonEmpty'
        let title = 'Delete the current node'
        let info =  "STOP! Directory is not empty! To delete, type 'yes'"
        let minimal = 'Delete directory?'

    elseif a:action ==# 'move'
        let title = 'Rename the current node'
        let info = 'Enter the new path for the node:'
        let minimal = 'Move to:'
    endif

    if g:NERDTreeMenuController.isMinimal()
        redraw! " Clear the menu
        return minimal . ' '
    else
        let divider = '=========================================================='
        return title . "\n" . divider . "\n" . info . "\n"
    end
endfunction

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
        " Go to the next buffer in buffer list if at least one extra buffer is listed
        " Otherwise open a new empty buffer
        if v:version >= 800
            let l:listedBufferCount = len(getbufinfo({'buflisted':1}))
        elseif v:version >= 702
            let l:listedBufferCount = len(filter(range(1, bufnr('$')), 'buflisted(v:val)'))
        else
            " Ignore buffer count in this case to make sure we keep the old
            " behavior
            let l:listedBufferCount = 0
        endif
        if l:listedBufferCount > 1
            call nerdtree#exec('tabdo windo if winbufnr(0) ==# ' . a:bufnum . " | exec ':bnext! ' | endif", 1)
        else
            call nerdtree#exec('tabdo windo if winbufnr(0) ==# ' . a:bufnum . " | exec ':enew! ' | endif", 1)
        endif
        call nerdtree#exec('tabnext ' . s:originalTabNumber, 1)
        call nerdtree#exec(s:originalWindowNumber . 'wincmd w', 1)
        " 3. We don't need a previous buffer anymore
        call nerdtree#exec('bwipeout! ' . a:bufnum, 0)
    endif
endfunction

"FUNCTION: s:renameBuffer(bufNum, newNodeName, isDirectory){{{1
"The buffer with the given bufNum is replaced with a new one
"
"Args:
"bufNum: the buffer that may be deleted
"newNodeName: the name given to the renamed node
"isDirectory: determines how to do the create the new filenames
function! s:renameBuffer(bufNum, newNodeName, isDirectory)
    if a:isDirectory
        let quotedFileName = fnameescape(a:newNodeName . '/' . fnamemodify(bufname(a:bufNum),':t'))
        let editStr = g:NERDTreePath.New(a:newNodeName . '/' . fnamemodify(bufname(a:bufNum),':t')).str({'format': 'Edit'})
    else
        let quotedFileName = fnameescape(a:newNodeName)
        let editStr = g:NERDTreePath.New(a:newNodeName).str({'format': 'Edit'})
    endif
    " 1. ensure that a new buffer is loaded
    call nerdtree#exec('badd ' . quotedFileName, 0)
    " 2. ensure that all windows which display the just deleted filename
    " display a buffer for a new filename.
    let s:originalTabNumber = tabpagenr()
    let s:originalWindowNumber = winnr()
    call nerdtree#exec('tabdo windo if winbufnr(0) ==# ' . a:bufNum . " | exec ':e! " . editStr . "' | endif", 0)
    call nerdtree#exec('tabnext ' . s:originalTabNumber, 1)
    call nerdtree#exec(s:originalWindowNumber . 'wincmd w', 1)
    " 3. We don't need a previous buffer anymore
    try
        call nerdtree#exec('confirm bwipeout ' . a:bufNum, 0)
    catch
        " This happens when answering Cancel if confirmation is needed. Do nothing.
    endtry
endfunction

"FUNCTION: NERDTreeAddNode(){{{1
function! NERDTreeAddNode()
    let curDirNode = g:NERDTreeDirNode.GetSelected()
    let prompt = s:inputPrompt('add')
    let newNodeName = substitute(input(prompt, curDirNode.path.str() . nerdtree#slash(), 'file'), '\(^\s*\|\s*$\)', '', 'g')

    if newNodeName ==# ''
        call nerdtree#echo('Node Creation Aborted.')
        return
    endif

    try
        let newPath = g:NERDTreePath.Create(newNodeName)
        let parentNode = b:NERDTree.root.findNode(newPath.getParent())

        let newTreeNode = g:NERDTreeFileNode.New(newPath, b:NERDTree)
        " Emptying g:NERDTreeOldSortOrder forces the sort to
        " recalculate the cached sortKey so nodes sort correctly.
        let g:NERDTreeOldSortOrder = []
        if empty(parentNode)
            call b:NERDTree.root.refresh()
            call b:NERDTree.render()
        elseif parentNode.isOpen || !empty(parentNode.children)
            call parentNode.addChild(newTreeNode, 1)
            call NERDTreeRender()
            call newTreeNode.putCursorHere(1, 0)
        endif

        redraw!
    catch /^NERDTree/
        call nerdtree#echoWarning('Node Not Created.')
    endtry
endfunction

"FUNCTION: NERDTreeMoveNode(){{{1
function! NERDTreeMoveNode()
    let curNode = g:NERDTreeFileNode.GetSelected()
    let prompt = s:inputPrompt('move')
    let newNodePath = input(prompt, curNode.path.str(), 'file')
    while filereadable(newNodePath)
        call nerdtree#echoWarning('This destination already exists. Try again.')
        let newNodePath = substitute(input(prompt, curNode.path.str(), 'file'), '\(^\s*\|\s*$\)', '', 'g')
    endwhile


    if newNodePath ==# ''
        call nerdtree#echo('Node Renaming Aborted.')
        return
    endif

    try
        if curNode.path.isDirectory
            let l:curPath = escape(curNode.path.str(),'\') . (nerdtree#runningWindows()?'\\':'/') . '.*'
            let l:openBuffers = filter(range(1,bufnr('$')),'bufexists(v:val) && fnamemodify(bufname(v:val),":p") =~# "'.escape(l:curPath,'\').'"')
        else
            let l:openBuffers = filter(range(1,bufnr('$')),'bufexists(v:val) && fnamemodify(bufname(v:val),":p") ==# curNode.path.str()')
        endif

        call curNode.rename(newNodePath)
        " Emptying g:NERDTreeOldSortOrder forces the sort to
        " recalculate the cached sortKey so nodes sort correctly.
        let g:NERDTreeOldSortOrder = []
        call b:NERDTree.root.refresh()
        call NERDTreeRender()

        " If the file node is open, or files under the directory node are
        " open, ask the user if they want to replace the file(s) with the
        " renamed files.
        if !empty(l:openBuffers)
            if curNode.path.isDirectory
                echo "\nDirectory renamed.\n\nFiles with the old directory name are open in buffers " . join(l:openBuffers, ', ') . '. Replace these buffers with the new files? (yN)'
            else
                echo "\nFile renamed.\n\nThe old file is open in buffer " . l:openBuffers[0] . '. Replace this buffer with the new file? (yN)'
            endif
            if g:NERDTreeAutoDeleteBuffer || nr2char(getchar()) ==# 'y'
                for bufNum in l:openBuffers
                    call s:renameBuffer(bufNum, newNodePath, curNode.path.isDirectory)
                endfor
            endif
        endif

        call curNode.putCursorHere(1, 0)

        redraw!
    catch /^NERDTree/
        call nerdtree#echoWarning('Node Not Renamed.')
    endtry
endfunction

" FUNCTION: NERDTreeDeleteNode() {{{1
function! NERDTreeDeleteNode()
    let currentNode = g:NERDTreeFileNode.GetSelected()
    let confirmed = 0

    if currentNode.path.isDirectory && ((currentNode.isOpen && currentNode.getChildCount() > 0) ||
                                      \ (len(currentNode._glob('*', 1)) > 0))
        let prompt = s:inputPrompt('deleteNonEmpty') . currentNode.path.str() . ': '
        let choice = input(prompt)
        let confirmed = choice ==# 'yes'
    else
        let prompt = s:inputPrompt('delete') . currentNode.path.str() . ' (yN): '
        echo prompt
        let choice = nr2char(getchar())
        let confirmed = choice ==# 'y'
    endif

    if confirmed
        try
            call currentNode.delete()
            call NERDTreeRender()

            "if the node is open in a buffer, ask the user if they want to
            "close that buffer
            let bufnum = bufnr('^'.currentNode.path.str().'$')
            if buflisted(bufnum)
                let prompt = "\nNode deleted.\n\nThe file is open in buffer ". bufnum . (bufwinnr(bufnum) ==# -1 ? ' (hidden)' : '') .'. Delete this buffer? (yN)'
                call s:promptToDelBuffer(bufnum, prompt)
            endif

            redraw!
        catch /^NERDTree/
            call nerdtree#echoWarning('Could not remove node')
        endtry
    else
        call nerdtree#echo('delete aborted')
    endif
endfunction

" FUNCTION: NERDTreeListNode() {{{1
function! NERDTreeListNode()
    let treenode = g:NERDTreeFileNode.GetSelected()
    if !empty(treenode)
        let s:uname = system('uname')
        let stat_cmd = 'stat -c "%s" '

        if s:uname =~? 'Darwin'
            let stat_cmd = 'stat -f "%z" '
        endif

        let cmd = 'size=$(' . stat_cmd . shellescape(treenode.path.str()) . ') && ' .
        \         'size_with_commas=$(echo $size | sed -e :a -e "s/\(.*[0-9]\)\([0-9]\{3\}\)/\1,\2/;ta") && ' .
        \         'ls -ld ' . shellescape(treenode.path.str()) . ' | sed -e "s/ $size / $size_with_commas /"'

        let metadata = split(system(cmd),'\n')
        call nerdtree#echo(metadata[0])
    else
        call nerdtree#echo('No information available')
    endif
endfunction

" FUNCTION: NERDTreeListNodeWin32() {{{1
function! NERDTreeListNodeWin32()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if !empty(l:node)
        let l:path = l:node.path.str()
        call nerdtree#echo(printf('%s:%s  MOD:%s  BYTES:%d  PERMISSIONS:%s',
                    \ toupper(getftype(l:path)),
                    \ fnamemodify(l:path, ':t'),
                    \ strftime('%c', getftime(l:path)),
                    \ getfsize(l:path),
                    \ getfperm(l:path)))
        return
    endif

    call nerdtree#echo('node not recognized')
endfunction

" FUNCTION: NERDTreeCopyNode() {{{1
function! NERDTreeCopyNode()
    let currentNode = g:NERDTreeFileNode.GetSelected()
    let prompt = s:inputPrompt('copy')
    let newNodePath = substitute(input(prompt, currentNode.path.str(), 'file'), '\(^\s*\|\s*$\)', '', 'g')

    if newNodePath !=# ''
        "strip trailing slash
        let newNodePath = substitute(newNodePath, '\/$', '', '')

        let confirmed = 1
        if currentNode.path.copyingWillOverwrite(newNodePath)
            call nerdtree#echo('Warning: copying may overwrite files! Continue? (yN)')
            let choice = nr2char(getchar())
            let confirmed = choice ==# 'y'
        endif

        if confirmed
            try
                let newNode = currentNode.copy(newNodePath)
                " Emptying g:NERDTreeOldSortOrder forces the sort to
                " recalculate the cached sortKey so nodes sort correctly.
                let g:NERDTreeOldSortOrder = []
                if empty(newNode)
                    call b:NERDTree.root.refresh()
                    call b:NERDTree.render()
                else
                    call NERDTreeRender()
                    call newNode.putCursorHere(0, 0)
                endif
            catch /^NERDTree/
                call nerdtree#echoWarning('Could not copy node')
            endtry
        endif
    else
        call nerdtree#echo('Copy aborted.')
    endif
    redraw!
endfunction

" FUNCTION: NERDTreeCopyPath() {{{1
function! NERDTreeCopyPath()
    let l:nodePath = g:NERDTreeFileNode.GetSelected().path.str()
    if has('clipboard')
        if &clipboard ==# 'unnamedplus'
            let @+ = l:nodePath
        else
            let @* = l:nodePath
        endif
        call nerdtree#echo('The path [' . l:nodePath . '] was copied to your clipboard.')
    else
        call nerdtree#echo('The full path is: ' . l:nodePath)
    endif
endfunction

" FUNCTION: NERDTreeQuickLook() {{{1
function! NERDTreeQuickLook()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    call system('qlmanage -p 2>/dev/null ' . shellescape(l:node.path.str()))
endfunction

" FUNCTION: NERDTreeRevealInFinder() {{{1
function! NERDTreeRevealInFinder()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    call system('open -R ' . shellescape(l:node.path.str()))
endfunction

" FUNCTION: NERDTreeExecuteFile() {{{1
function! NERDTreeExecuteFile()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    call system('open ' . shellescape(l:node.path.str()))
endfunction

" FUNCTION: NERDTreeRevealFileLinux() {{{1
function! NERDTreeRevealFileLinux()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    " Handle the edge case of "/", which has no parent.
    if l:node.path.str() ==# '/'
        call system('xdg-open /')
        return
    endif

    if empty(l:node.parent)
        return
    endif

    call system('xdg-open ' . shellescape(l:node.parent.path.str()))
endfunction

" FUNCTION: NERDTreeExecuteFileLinux() {{{1
function! NERDTreeExecuteFileLinux()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    call system('xdg-open ' . shellescape(l:node.path.str()))
endfunction

" FUNCTION: NERDTreeExecuteFileWindows() {{{1
function! NERDTreeExecuteFileWindows()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    call system('cmd.exe /c start "" ' . shellescape(l:node.path.str()))
endfunction

" FUNCTION: NERDTreeSystemCommand() {{{1
function! NERDTreeSystemCommand()
    let l:node = g:NERDTreeFileNode.GetSelected()

    if empty(l:node)
        return
    endif

    let l:cwd = getcwd()
    let l:directory = l:node.path.isDirectory ? l:node.path.str() : l:node.parent.path.str()
    execute 'cd '.l:directory

    let l:nl = nr2char(10)
    echo l:nl . system(input(l:directory . (nerdtree#runningWindows() ? '> ' : ' $ ')))
    execute 'cd '.l:cwd
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
